#| rpc-proxy.jl -- stubs/skels for transparent rpc calls
   $Id: rpc-proxy.jl,v 1.14 2002/03/31 03:41:05 jsh Exp $
|#

(define-structure dfs.common.rpc-proxy 

    (export rpc-id->hash-object
	    hash-object->rpc-id)

    (open rep
	  dfs.common.util
	  dfs.common.rpc-hints
	  rep.net.rpc
	  rep.data.tables
	  rep.system
	  rep.util.time
	  rep.test.framework)

  (define rpc-debug nil)

  ;; OBJECT -> GLOBAL-ID
  (define object-table (make-weak-table eq-hash eq))

  ;; GLOBAL-ID -> OBJECT
  (define gid-table (make-table string-hash string=))

  (define (debug fmt . args)
    (let ((print-escape t))
      (when rpc-debug
	(apply format standard-error fmt args))))

  (define (convert-arg-list method args converter)
    (let ((arglist (rpc-hints/method-arglist method))
	  (objects (rpc-hints/method-object-args method)))
      (let loop ((i 0)
		 (rest args)
		 (out '()))
	(cond ((null rest)
	       (nreverse out))
	      ((= i arglist)
	       (nconc (nreverse out)
		      (list (car rest))
		      (convert-arg-list (car rest) (cdr rest) converter)))
	      (t (loop (1+ i)
		       (cdr rest)
		       (cons (if (memql i objects)
				 (converter (car rest))
			       (car rest)) out)))))))

  (define (convert-result method result converter)
    (if (and result (eq (rpc-hints/method-result method) 'object))
	(converter result)
      result))

  ;; rpc proxy for local servant
  (define (make-skel fun)
    (lambda (method . args)
      (debug " in: %S %S\n" fun method)
      (let* ((real-args (convert-arg-list method args rpc-id->hash-object))
	     (result (apply fun method real-args)))
	(if (rpc-hints/method-result method)
	    (convert-result method result hash-object->rpc-id)
	  #undefined))))

  ;; rpc proxy for remote servant
  (define (make-stub fun)

    (let ((cached-address nil)
	  (cached-joined nil)
	  (cached-joined-expires nil))

      (define (wrap fun . args)
	(call-with-exception-handler
	 (lambda ()
	   (apply fun args))
	 (lambda (ex)
	   (if (and (eq (car ex) 'error) (eq (cadr ex) 'file-error))
	       (progn
		 (debug "out: connection broken\n")
		 (setq cached-joined nil)
		 (setq cached-joined-expires (expt 2 64))
		 nil)
	     (raise-exception ex)))))

      (lambda (method . args)
	(cond ((eq method 'address)
	       (unless cached-address
		 (setq cached-address (wrap fun 'address)))
	       cached-address)
	      ((eq method 'joined?)
	       (let ((now (time)))
		 (when (or (not cached-joined-expires)
			   (>= now cached-joined-expires))
		   (debug "out: %S %S\n" fun method)
		   (setq cached-joined (wrap fun 'joined?))
		   (setq cached-joined-expires (+ now 1)))
		 cached-joined))
	      (t (let* ((real-args (convert-arg-list
				    method args hash-object->rpc-id)))
		   (debug "out: %S %S\n" fun method)
		   (if (rpc-hints/method-result method)
		       (let ((result (wrap apply fun method real-args)))
			 ;;(debug "out: result %S\n" result)
			 (convert-result method result rpc-id->hash-object))
		     ;; no result wanted, so just call async
		     (wrap apply async-rpc-call fun method real-args))))))))

;;; public entrypoints

  ;; Accepts either a global or local id. If a local id, assumes it
  ;; belongs to the currently invoked remote servant
  (define (rpc-id->hash-object id)
    (assert (or (stringp id) (symbolp id)))
    (let ((gid (if (symbolp id)
		   (remote-servant-id->global-id id)
		 id)))
      (or (table-ref gid-table gid)
	  (let ((fun (make-stub (global-id->rpc-proxy gid))))
	    ;; Remote objects are always accessed through their stub
	    (table-set gid-table gid fun)
	    (table-set object-table fun gid)
	    ;;(debug "demarshal %s: stub %s\n" id fun)
	    fun))))

  ;; allows OBJ to be an id already..
  (define (hash-object->rpc-id obj)
    (assert (or (closurep obj)
		(stringp obj)
		(symbolp obj)))
    (if (closurep obj)
	(or (table-ref object-table obj)
	    (let* ((skel (make-skel obj))
		   (gid (servant-id->global-id (make-rpc-servant skel))))
	      ;; Local objects are always accessed without their skel. The
	      ;; skel is only for incoming rpc calls
	      (table-set object-table obj gid)
	      (table-set gid-table gid obj)
	      ;;(debug "created hash dispatcher %s for %s\n" obj gid)
	      gid))
      obj)))
