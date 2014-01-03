#| local-data-hash.jl -- use a hash table to implement data-hash nodes
   $Id: local-data-hash.jl,v 1.15 2002/03/26 07:48:20 jsh Exp $
|#

(define-structure dfs.peer.local-data-hash

    (export make-local-hash-node)

    (open rep
	  dfs.peer.data-hash
	  dfs.common.util
	  rep.system
	  rep.data.tables
	  rep.data.objects
	  rep.util.time
	  rep.io.files
	  rep.test.framework)

  ;; A local in-memory version of a consistent hash node, just for testing
  ;; SUPER should be a data-hash object
  (define (make-local-hash-node #!key super address)
    (let ((store (make-table equal-hash equal))
	  (leases (make-table equal-hash equal))
	  (dirty-table (make-table equal-hash equal)))

      (unless super
	(setq super (make-data-hash-node #:address address)))

      (object super
	((class) 'local-data-hash)

	((-key-address key) (equal-hash key))

	((-key-defined? key) (table-bound-p store key))

	((-key-import! key filename)
	 (table-set store key (read-string-from-file filename))
	 (self '-debug "Imported %S data" key))

	((-key-export key callback)
	 (or (self '-key-defined? key)
	     (error "%d: No data for key: %s" (self 'address) key))
	 (let ((temp-file (make-temp-name)))
	   (write-string-to-file temp-file (table-ref store key))
	   (unwind-protect
	       (callback temp-file)
	     (delete-file temp-file))))

	((-key-delete! key)
	 (table-unset store key)
	 (table-unset leases key)
	 (self '-debug "Deleted key %S" key))

	((-key-foreach fun)
	 (table-walk (lambda (key value)
		       (declare (unused value))
		       (fun key)) store))

	((-key-lease-set! key lease)
	 (let ((current (table-ref leases key)))
	   (when (or (not current) (> lease current))
	     (table-set leases key lease)
	     (self '-debug "Set lease of %S to %s"
		   key (current-time-string
			(seconds->time lease) "%Y-%m-%d %H:%M:%S")))))

	((-key-lease-ref key)
	 (and (table-bound-p leases key) (table-ref leases key)))

	((-key-dirty key) (table-set dirty-table key t))
	((-key-undirty key) (table-unset dirty-table key))
	((-key-dirty? key) (table-ref dirty-table key))
	((-key-dirty-foreach fun) (table-walk fun dirty-table)))))


  (define (self-test)
    (require 'dfs.common.transfer)

    (let* ((node-addresses '(10 170 100 210 50 240))
	   (nodes (mapcar (lambda (x)
			    (make-local-hash-node #:address x))
			  node-addresses))
	   (items '((5 . "five") (10 . "ten") (30 . "thirty")
		    (60 . "sixty") (100 . "hundred") (169 . "one-six-nine")
		    (170 . "one-seventy") (171 . "one-seven-one")
		    (190 . "one-ninety") (250 . "two-fifty"))))

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
		(m '-debug "Reading %S" (car x))
		(let ((value (x-fetch m (car x))))
		  (m '-debug "Read %S as %S" (car x) value)
		  (unless (equal value (cdr x))
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
      (mapc read-all nodes)

      ;; now delete nodes one at a time, testing as we go
      (let loop ((rest nodes))
	(when (cdr rest)
	  ((car rest) 'leave)
	  (periodically)
	  (read-all (cadr rest))
	  (loop (cdr rest))))))

  ;;###autoload
  (define-self-test 'dfs.peer.local-data-hash self-test))
