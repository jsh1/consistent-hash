
(define-structure dfs.simulations.read

    (export main)

    (open rep
	  rep.io.files
	  rep.data.objects
	  dfs.common.transfer
	  dfs.simulator.node
	  dfs.simulator.run)

  (define (random-string len)
    (let ((string (make-string len)))
      (do ((i 0 (1+ i)))
	  ((= i len))
	(aset string i (+ (random 26) #\A)))
      string))

  (define new-token
    (let ((counter 0))
      (lambda ()
	(prog1 counter
	  (setq counter (1+ counter))))))

  (define (main #!key synchronize
		(starting-nodes 64)
		(maximum-nodes 256)
		(total-datums 64)
		(churn-probability 0)
		(simulation-length 64)
		(simulation-inner-length 64))

    (let ((datums (make-vector total-datums))
	  (total-nodes starting-nodes)
	  (failures 0)
	  (failure-hist (make-vector simulation-length 0))
	  (result nil))

      (define local-node
	(object nil
	  (transfer/filesystem-id transfer/filesystem-id)
	  ((transfer/send token offset data)
	   (declare (unused token offset data)))
	  ((transfer/end token status)
	   (declare (unused token))
	   (setq result status))))

      (define (random-datum) (aref datums (random total-datums)))

      (define (add-node)
	(when (< total-nodes maximum-nodes)
	  (make-simulator-node)
	  (setq total-nodes (1+ total-nodes))))

      (define (remove-node)
	(when (> total-nodes 0)
	  (delete-simulator-node (random-simulator-node))
	  (setq total-nodes (1- total-nodes))))

      (start-simulation total-nodes)

      ;; setup
      (let ((node (random-simulator-node)))
	(do ((i 0 (1+ i)))
	    ((= i total-datums))
	  (let ((key (random-string 16)))
	    (let ((token (node 'define key 3600)))
	      (when token
		(transfer-string node token key)))
	    (aset datums i key))))

      (when synchronize
	(synchronize-simulation-nodes))

      ;; steady state
      (do ((i 0 (1+ i)))
	  ((= i simulation-length))

	(ticker)

	(when (< (random-fraction) churn-probability)
	  (remove-node))
	(when (< (random-fraction) churn-probability)
	  (add-node))

	(let ((key (random-datum)))

	  (do ((j 0 (1+ j)))
	      ((= j simulation-inner-length))

	    (let* ((node (random-simulator-node))
		   (token (new-token)))

	      (setq result nil)
	      (simulate-operation node 'fetch key local-node token)
	      (unless result
		;;(format standard-error "%s from %s failed\n" key (node 'address))
		(setq failures (1+ failures))
		(aset failure-hist i (1+ (aref failure-hist i))))))))

      (let ((profile (end-simulation)))
	(list (exact->inexact (/ failures (* simulation-length
					     simulation-inner-length)))
	      failure-hist)))))
