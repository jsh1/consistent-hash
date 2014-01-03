
(define-structure dfs.simulations.write

    (export main)

    (open rep
	  rep.io.files
	  dfs.common.transfer
	  dfs.simulator.node
	  dfs.simulator.run)

  (define (random-string len)
    (let ((string (make-string len)))
      (do ((i 0 (1+ i)))
	  ((= i len))
	(aset string i (+ (random 26) #\A)))
      string))

  (define (main #!key (total-nodes 64)
		(simulation-length 64)
		(simulation-inner-length 64))

    (start-simulation total-nodes)

    ;; steady state
    (do ((i 0 (1+ i)))
	((= i simulation-length))

      (ticker)

      (do ((j 0 (1+ j)))
	  ((= j simulation-inner-length))

	(let ((node (random-simulator-node))
	      (key (random-string 16)))
	  (let ((token (simulate-operation node 'define key 3600)))
	    (when token
	      ;; XXX should really use simulate-operation..
	      (transfer-string node token key))))))

    (end-simulation)))
