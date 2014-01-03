
(define-structure dfs.simulations.lookup

    (export main)

    (open rep
	  rep.io.files
	  rep.data.tables
	  dfs.simulator.node
	  dfs.simulator.run)

  (define (main #!key
		(starting-nodes 64)
		(maximum-nodes 256)
		(churn-probability 0.1)
		(simulation-length 128)
		(simulation-inner-length 128))

    (let ((total-nodes 0))

      (define (add-node)
	(when (< total-nodes maximum-nodes)
	  (make-simulator-node)
	  (setq total-nodes (1+ total-nodes))))

      (define (remove-node)
	(when (> total-nodes 0)
	  (delete-simulator-node (random-simulator-node))
	  (setq total-nodes (1- total-nodes))))

      (start-simulation starting-nodes)

      ;; steady state
      (do ((i 0 (1+ i)))
	  ((= i simulation-length))

	(ticker)

	(when (< (random-fraction) churn-probability)
	  (remove-node))
	(when (< (random-fraction) churn-probability)
	  (add-node))

	(do ((j 0 (1+ j)))
	    ((= j simulation-inner-length))

	  (let ((node (random-simulator-node)))
	    ;; XXX check we get the correct value back?
	    (simulate-operation node 'successor (random-simulator-address)))))

      (let ((profile (end-simulation)))
	(exact->inexact
	 (/ (table-ref profile '-closest-preceding-node)
	    (table-ref profile 'predecessor)))))))
