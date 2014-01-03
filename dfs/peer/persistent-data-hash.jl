#| persistent-data-hash.jl -- use the file system to implement data-hash nodes
   $Id: persistent-data-hash.jl,v 1.13 2002/03/26 07:48:20 jsh Exp $
|#

(define-structure dfs.peer.persistent-data-hash

    (export make-persistent-hash-node)

    (open rep
	  dfs.peer.data-hash
	  dfs.common.util
	  rep.system
	  rep.regexp
	  rep.io.files
	  rep.io.db.gdbm
	  rep.data.objects
	  rep.util.md5
	  rep.util.time
	  rep.test.framework)

  (defconst sub-directories 16)

  ;; Try to create dir and all nonexistent parent dirs (like mkdir -p)
  (define (make-directory-recursively dir)
    (while (not (file-exists-p dir))
      (let ((tem dir))
	(while (not (file-exists-p (expand-file-name ".." tem)))
	  (setq tem (expand-file-name ".." tem)))
	(make-directory tem))))

  (defun delete-file-recursively (file)
    (cond ((file-symlink-p file)
	   (delete-file file))
	  ((file-directory-p file)
	   (mapc (lambda (f)
		   (unless (or (string= "." f) (string= ".." f))
		     (delete-file-recursively
		      (concat (file-name-as-directory file) f))))
		 (directory-files file))
	   (delete-directory file))
	  ((file-exists-p file)
	   (delete-file file))))

  (define (quote-key key)
    (string-replace "[^a-zA-Z0-9_-]"
		    (lambda (s)
		      (format nil "%%%02x" (aref s (match-start))))
		    key))

  (define (disk-usage node)
    (cond ((file-symlink-p node) 0)
	  ((file-directory-p node)
	   (apply + (mapcar (lambda (f)
			      (if (or (string= "." f) (string= ".." f))
				  0
				(disk-usage (expand-file-name f node))))
			    (directory-files node))))
	  ((file-regular-p node) (file-size node))
	  (t 0)))

  ;; SUPER should be a data-hash object
  (define (make-persistent-hash-node #!key super address directory target-size)
    (let ((leases (gdbm-open (expand-file-name "Leases" directory) 'append))
	  (dirty-keys (gdbm-open (expand-file-name "Dirty" directory) 'append)))

      (define (key->filename key)
	(let ((quoted (quote-key key)))
	  (expand-file-name
	   (format nil "%02x/%s" (mod (md5-string key) sub-directories) quoted)
	   directory)))

      (unless super
	(setq super (make-data-hash-node #:address address)))

      (object super
	((class) 'persistent-data-hash)

	((-key-address key) (md5-string key))

	((-key-defined? key) (file-exists-p (key->filename key)))

	((-key-import! key temp-file)
	 (let ((filename (key->filename key)))
	   (make-directory-recursively (file-name-directory filename))
	   (copy-file temp-file filename))
	 (self '-debug "Imported %S data" key))

	((-key-export key callback)
	 (or (self '-key-defined? key)
	     (error "%d: No data for key: %s" (self 'address) key))
	 (callback (key->filename key)))

	((-key-delete! key)
	 (when (self '-key-dirty? key)
	   (error "Deleting a retained key: %s" key))
	 (let ((filename (key->filename key)))
	   (when (file-exists-p filename)
	     (delete-file filename)))
	 (gdbm-delete leases key)
	 (self '-debug "Deleted key %S" key))

	((-key-foreach fun) (gdbm-walk fun leases))

	((-key-lease-set! key lease)
	 (let ((current (gdbm-fetch leases key)))
	   (when (or (not current) (> lease (string->number current)))
	     (gdbm-store leases key (number->string lease))
	     (self '-debug "Set lease of %S to %s"
		   key (current-time-string
			(seconds->time lease) "%Y-%m-%d %H:%M:%S")))))

	((-key-lease-ref key)
	 (let ((string (gdbm-fetch leases key)))
	   (and string (string->number string))))

	((-key-dirty key) (gdbm-store dirty-keys key "t"))
	((-key-undirty key) (gdbm-delete dirty-keys key))
	((-key-dirty? key) (gdbm-fetch dirty-keys key))
	((-key-dirty-foreach fun) (gdbm-walk fun dirty-keys))

	((-total-size) (disk-usage directory))
	((-target-size) target-size)
	((-key-size k) (disk-usage (key->filename k))))))


  (define (self-test)
    (define root-dir (make-temp-name))

    (define (make-test-nodes addresses)
      (make-directory root-dir)
      (mapcar (lambda (x)
		(let ((dir (expand-file-name (number->string x) root-dir)))
		  (make-directory dir)
		  (make-persistent-hash-node #:address x
					     #:directory dir
					     #:target-size (* 32 1024))))
	      addresses))

    (define (destroy-test-nodes) (delete-file-recursively root-dir))

    (require 'dfs.common.transfer)
    (require 'rep.util.md5)

    (let* ((node-addresses '(10 170 100 210 50 240))
	   ;; XXX add directory args...
	   (nodes (make-test-nodes node-addresses))
	   (items (mapcar
		   (lambda (x)
		     (cons (number->string (md5-string (car x)) 36) (cdr x)))
		   '(("5" . "five") ("10" . "ten") ("30" . "thirty")
		     ("60" . "sixty") ("100" . "hundred")
		     ("169" . "one-six-nine") ("170" . "one-seventy")
		     ("171" . "one-seven-one") ("190" . "one-ninety")
		     ("250" . "two-fifty")))))

      (define (x-define node key lease value)
	(let ((token (node 'define key (+ (time) lease))))
	  (when token
	    (transfer-string node token value))))

      (define (x-fetch node key)
	(let ((token (transfer/begin key (lambda () (make-temp-name))))
	      (value nil))
	  (node 'fetch key
		(object ()
		  (transfer/send transfer/send)
		  (transfer/end
		   (transfer/end
		    (lambda (token key file)
		      (declare (unused token key))
		      (when file
			(setq value (read-string-from-file file))))))
		  (transfer/filesystem-id transfer/filesystem-id)
		  (transfer/send-file transfer/send-file))
		token)
	  value))

      (define (read-all m)
	(mapc (lambda (x)
		(let ((value (x-fetch m (car x))))
		  (unless (equal value (cdr x))
		    ;;(break)
		    (self-test/failed 'test (format nil "(equal %S %S)"
						    value (cdr x))))))
	      items))

      (define (apply-method m . args)
	(mapc (lambda (n) (apply n m args)) nodes))

      (define (periodically)
	(apply-method 'periodically)
	(apply-method 'garbage-collect))

      ;; insert the first node
      ((car nodes) 'join)

      ;; write all items to the network
      (mapc (lambda (x)
	      (x-define (car nodes) (car x) 60 (cdr x))) items)

      ;; try to read back all values
      (read-all (car nodes))

      ;; add all other nodes in a non-ordered order
      (mapc (lambda (n) (n 'join (car nodes))) (cdr nodes))

      (periodically)

      ;; try to read back all values from all nodes
      (mapc (lambda (n) (read-all n)) nodes)

      ;; now delete nodes one at a time, testing as we go
      (let loop ((rest nodes))
	(when (cdr rest)
	  ((car rest) 'leave)
	  (read-all (cadr rest))
	  (loop (cdr rest))))

      (destroy-test-nodes)))

  ;;###autoload
  (define-self-test 'dfs.peer.persistent-data-hash self-test))
