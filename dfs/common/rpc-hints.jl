
(define-structure dfs.common.rpc-hints

    (export define-rpc-method
	    rpc-hints/method-arglist
	    rpc-hints/method-object-args
	    rpc-hints/method-result)

    (open rep)

  (define (define-rpc-method m #!key result object-args arglist)
    (when result
      (put m 'rpc-hints/result result))
    (when object-args
      (put m 'rpc-hints/object-args (if (consp object-args)
					object-args
				      (list object-args))))
    (when arglist
      (put m 'rpc-hints/arglist arglist)))

  (define (rpc-hints/method-arglist m) (get m 'rpc-hints/arglist))
  (define (rpc-hints/method-object-args m) (get m 'rpc-hints/object-args))
  (define (rpc-hints/method-result m) (get m 'rpc-hints/result)))
