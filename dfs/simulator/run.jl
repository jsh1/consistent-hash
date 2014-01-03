
(define-structure dfs.simulator.run

    (export random-fraction
	    simulate-operation
	    ticker)

    (open rep
	  rep.data.tables
	  rep.io.files
	  dfs.simulator.node)

  (define update-period 1000)

  (define (random-fraction) (/ (random 65536) 65536.))

  (define node-clocks (make-table eq-hash eq))

  (define master-clock 0)

  (define (simulate-operation node method . args)
    (setq master-clock (1+ master-clock))
    (foreach-simulator-node
     (lambda (n)
       (let ((n-time (table-ref node-clocks n)))
	 (unless n-time
	   ;; random first update time for unknown nodes
	   (setq n-time (- master-clock (random update-period)))
	   (table-set node-clocks n n-time))
	 (when (>= master-clock (+ n-time update-period))
	   (table-set node-clocks n master-clock)
	   (n 'periodically)))))
    (apply node method args))

  (define ticker
    (let ((counter 0))
      (lambda ()
	(write standard-error (aref ["\\\r" "|\r" "/\r" "-\r"] counter))
	(flush-file standard-error)
	(setq counter (mod (1+ counter) 4))))))
