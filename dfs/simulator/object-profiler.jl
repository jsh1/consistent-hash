
(define-structure dfs.simulator.object-profiler

    (export make-object-profile
	    clear-object-profile
	    print-object-profile
	    make-profiled-object)

    (open rep
	  rep.data.tables
	  rep.data.objects)

  (define (make-object-profile)
    (make-table eq-hash eq))

  (define (clear-object-profile state)
    (let ((keys '()))
      (table-walk (lambda (k v)
		    (declare (unused v))
		    (setq keys (cons k keys)))
		  state)
      (mapc (lambda (k)
	      (table-unset state k)) keys)))

  (define (print-object-profile state #!optional name)
    (format standard-error "\nMethod profile of %s:\n" name)
    (table-walk (lambda (method count)
		  (format standard-error "%32s %d\n" method count)) state))

  (define (make-profiled-object obj #!key (profile (make-object-profile))
				(methods t))
    (object-lambda (method . args)
      (when (or (eq methods t) (memq method methods))
	(table-set profile method
		   (1+ (or (table-ref profile method) 0))))
      (apply obj method #:self self args))))
