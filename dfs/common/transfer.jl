
(define-structure dfs.common.transfer

    (export transfer/filesystem-id
	    transfer/begin
	    transfer/send
	    transfer/send-file
	    transfer/end
	    transfer-failed
	    transfer-file
	    transfer-string)

    (open rep
	  dfs.common.util
	  dfs.common.rpc-hints
	  rep.data.tables
	  rep.io.files
	  rep.io.timers
	  rep.system
	  rep.util.time
	  rep.util.md5
	  rep.test.framework)

  (defconst block-size 4096)
  (defconst token-lifetime 1800)

;;; utilities

  (define unique-filename "/etc/ssh/ssh_host_key.pub")

  (define transfer/filesystem-id
    (let ((done nil)
	  (hash nil))
      (lambda ()
	(unless done
	  (when (file-exists-p unique-filename)
	    (let ((file (open-file unique-filename 'read)))
	      (unwind-protect
		  (setq hash (number->string
			      (md5-string (read-line file)) 36))
		(close-file file))))
	  (setq done t))
	hash)))

  (define (compare-fs-ids x y) (and x y (string= x y)))

  ;; map from OBJECT -> FS-ID
  (define known-fs-ids (make-weak-table eq-hash eq))

  (define (object-fs-id obj)
    (or (table-ref known-fs-ids obj)
	(table-set known-fs-ids obj (obj 'transfer/filesystem-id))))

  (define (same-filesystem-p obj)
    (compare-fs-ids (transfer/filesystem-id) (object-fs-id obj)))

;;; transfer receiver side

  ;; map from TOKEN -> (KEY STAGE-FILE EXPIRES)
  (define outstanding-tokens (make-table string-hash string=))

  ;; register that someone is going to send us the data associated
  ;; with KEY. (CALLBACK KEY) will be called to get the name of a
  ;; file to stream the data into
  (define (transfer/begin key callback)
    (let ((filename (callback key)))
      (when filename
	(let ((file (open-file filename 'write)))
	  (table-set outstanding-tokens filename
		     (list key file (+ (time) token-lifetime)))
	  filename))))

  ;; implements the transfer/send method
  (define (transfer/send token index data)
    (let ((file (nth 1 (table-ref outstanding-tokens token))))
      (seek-file file index 'start)
      (write file data)))

  ;; implements the transfer/send-file method
  (define (transfer/send-file token filename)
    (let ((out (nth 1 (table-ref outstanding-tokens token)))
	  (in (open-file filename 'read)))
      (unwind-protect
	  (copy-stream in out)
	(close-file in))
      t))

  ;; returns the function that implements the transfer/end method.
  ;; (CALLBACK FILE) will be called when the method is invoked. FILE
  ;; will be null if something failed
  (define (transfer/end callback)
    (lambda (token success)
      (let ((cell (table-ref outstanding-tokens token)))
	(or cell (error "No record of token: %s" token))
	(table-unset outstanding-tokens token)
	(when cell
	  (close-file (nth 1 cell))
	  (callback token (car cell) (and success token))
	  (when (file-exists-p token)
	    (delete-file token))))))

  ;; remove TOKEN from our records
  (define (close-token token)
    (let ((cell (table-ref outstanding-tokens token)))
      (when (filep (nth 1 cell))
	(close-file (nth 1 cell)))
      (condition-case nil
	  (delete-file token)
	(file-error))))

  ;; expire tokens that are taking too long to be received
  (define (expire-tokens)
    (let ((now (time))
	  (to-delete '()))
      (table-walk (lambda (token cell)
		    (when (<= (nth 2 cell) now)
		      (format standard-error "Expiring token %s\n" token)
		      (setq to-delete (cons token to-delete))))
		  outstanding-tokens)
      (mapc (lambda (token)
	      ;; XXX call the callback somehow
	      (close-token token)
	      (table-unset outstanding-tokens token)) to-delete)))

  ;; run expiry function every minute
  (make-timer (lambda (timer)
		(expire-tokens)
		(set-timer timer)) 60)

  (define (transfer-before-exit)
    (table-walk (lambda (token cell)
		  (declare (unused cell))
		  (format standard-error "Token %s outstanding!\n" token)
		  (close-token token))
		outstanding-tokens)
    (setq outstanding-tokens nil))

  (add-hook 'before-exit-hook transfer-before-exit)

;;; transfer sender side

  ;; tell OBJECT that TOKEN has finished being sent. STATUS should
  ;; be true if the transfer was successful
  (define (transfer-finish object token status)
    (object 'transfer/end token status))

  (define (transfer-failed object token) (transfer-finish object token nil))

  ;; send FILENAME to OBJECT under TOKEN
  (define (transfer-file object token filename)
    (let ((success nil))
      (unwind-protect
	  (if (same-filesystem-p object)
	      ;; Woo! Let's just copy locally..
	      (setq success (object 'transfer/send-file token filename))
	    (let ((file (open-file filename 'read)))
	      (unwind-protect
		  (do ((bytes (read-chars file block-size)
			      (read-chars file block-size))
		       (point 0 (+ point block-size)))
		      ((null bytes) (setq success t))
		    (object 'transfer/send token point bytes))
		(close-file file))))
	(transfer-finish object token success))))

  ;; send STRING to OBJECT under TOKEN
  (define (transfer-string object token string)
    (let ((success nil))
      (unwind-protect
	  (progn
	    (object 'transfer/send token 0 string)
	    (setq success t))
	(transfer-finish object token success))))

;;; rpc method hints

  (define-rpc-method 'transfer/filesystem-id #:result t)
  (define-rpc-method 'transfer/send-file #:result t))
