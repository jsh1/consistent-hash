
(define-structure dfs.simulator.node

    (export random-simulator-address
	    random-simulator-node
	    find-simulator-node
	    make-simulator-node
	    foreach-simulator-node
	    delete-simulator-node
	    start-simulation
	    end-simulation
	    synchronize-simulation-nodes)

    (open rep
	  rep.data.tables
	  dfs.peer.local-data-hash
	  dfs.simulator.object-profiler)

  ;; map from ADDRESS -> OBJECT
  (define node-table (make-table equal-hash equal))

  ;; list of all nodes
  (define node-list '())

  (define profile (make-object-profile))
  (define profiled-methods t)
			     
  ;; matches definition of degree in consistent-hash
  (define (random-simulator-address) (random (expt 2 16)))

  (define (random-simulator-node)
    (let ((total (table-size node-table)))
      (if (= total 0)
	  nil
	(nth (random total) node-list))))

  (define (find-simulator-node address) (table-ref node-table address))

  (define (make-simulator-node)
    (let loop ((address (random-simulator-address)))
      (if (table-ref node-table address)
	  (loop (random-simulator-address))

	;; got a unique address

	;; XXX use the persistent hash, or fixup the local
	;; XXX one to support all (gc) features
	(let ((node (make-local-hash-node #:address address))
	      (peer (random-simulator-node)))

	  (when profile
	    (setq node (make-profiled-object node #:profile profile
					     #:methods profiled-methods)))

	  (table-set node-table address node)
	  (setq node-list (cons node node-list))

	  (node 'join peer)

	  node))))

  (define (foreach-simulator-node fun) (mapc fun node-list))

  (define (delete-simulator-node node)
    (let ((address (node 'address)))
      (when (node 'joined?)
	(node 'leave))
      (table-unset node-table address)
      (setq node-list (delq node node-list))))

  (define (start-simulation n)
    (when profile
      (clear-object-profile profile))
    (do ((i 0 (1+ i)))
	((= i n))
      (make-simulator-node)))

  (define (end-simulation #!key print-profile)
    (when (and profile print-profile)
      (print-object-profile profile))
    (setq node-list '())
    (setq node-table (make-table equal-hash equal))
    profile)

  (define (synchronize-simulation-nodes)
    (foreach-simulator-node
     (lambda (n) (n 'periodically)))))
