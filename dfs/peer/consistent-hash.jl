#| consistent-hash.jl -- consistent hash with O(log N) lookups
   $Id: consistent-hash.jl,v 1.19 2002/03/31 03:41:05 jsh Exp $
|#

(define-structure dfs.peer.consistent-hash

    (export between
	    distance
	    consistent-hash-public-methods
	    make-consistent-hash-node)

    (open rep
	  rep.data.objects
	  rep.test.framework
	  dfs.common.rpc-hints)

  (define degree 16)			;XXX increase this for real use
  (define total-positions (expt 2 degree))

  (define known-successors 3)

  (define infinite-distance (1+ total-positions))

  ;; Return true if address X is between LOWER and UPPER, modulo the
  ;; size of the ring. Being `between' means if you move clockwise from
  ;; LOWER to UPPER (not including UPPER), do you pass X.

  (define (between x lower upper)
    (let ((region-size (mod (- upper lower) total-positions)))
      (< (mod (- x lower) total-positions)
	 ;; if (= lower upper) (mod m), then x is always inside
	 (if (zerop region-size) total-positions region-size))))

  ;; Return the distance between addresses FIRST and LAST, modulo the
  ;; size of the ring. The distance is defined as the number of
  ;; clockwise steps needed to advance from FIRST to LAST

  (define (distance first last)
    (let ((dist (mod (- last first) total-positions)))
      (if (= dist 0)
	  total-positions
	dist)))

  (define consistent-hash-public-methods
    '(address successor predecessor -closest-preceding-node
      joined? responsible-for? join leave broadcast -broadcast-stage
      -node-successor -node-predecessor))

  (define (make-consistent-hash-node self-address)

    (let (;; Knowledge of the network, our immediate predecessor (may
          ;; be null), and our finger table pointing to some of our
          ;; successors, the i'th entry stores (successor (+ 
          ;; self-address 2^i))
	  (previous nil)

	  ;; order is [SUCCESSOR_N-1 ... SUCCESSOR_0 FINGER_1 .. FINGER_N-1]
	  (routing-table (make-vector (+ degree known-successors -1)))

	  ;; true if we've joined a network
	  (in-network nil)

	  (debug-mode nil))

      ;; Return the ring address that finger I of this node points to.
      ;; (It actually points to the successor of this address)
      (define (finger->address i)
	(mod (+ self-address (expt 2 i)) total-positions))

      ;; Return the index of the finger storing the node (successor ADDR).
      ;; Or false if there isn't a finger for that address.
      (define (address->finger addr)
	(let* ((offset (mod (- addr self-address) total-positions))
	       (degree (log offset 2)))
	  (and (integerp degree)
	       (inexact->exact degree)))) 

      (define (finger-ref i)
	(aref routing-table (+ i (1- known-successors))))
      (define (finger-set! i x)
	(aset routing-table (+ i (1- known-successors)) x))

      (define (successor-ref i)
	(aref routing-table (- (1- known-successors) i)))
      (define (successor-set! i x)
	(aset routing-table (- (1- known-successors) i) x))

      (define (previous-set! x) (setq previous x))

      (object ()
	((class) 'consistent-hash)

	;; Abstract methods:
	;;  (predecessor-changed NODE)
	;;  (successor-changed NODE DEGREE)
	;;  (left-network)

	;; Return the address of this node
	((address) self-address)

	;; Search for the successor node of ADDRESS. This is the node
	;; either with ADDRESS (modulo the size of the ring), or the
	;; first node following ADDRESS
	((successor address)
	 (let ((pred (self 'predecessor address)))
	   (let ((succ (and pred (pred '-node-successor))))
	     (when succ
	       (let ((finger (address->finger address)))
		 ;; the address we were asked for is one of our fingers..
		 ;; this is especially useful when broadcasting
		 (when finger
		   (finger-set! finger succ)))
	       succ))))

	;; Search for the predecessor node of ADDRESS. This is the
	;; first node preceeding ADDRESS
	((predecessor address)
	 (let loop ((node self)
		    (seen '()))
	   (if (let ((succ (node '-node-successor)))
		 (and succ (between address (1+ (node 'address))
				    (1+ (succ 'address)))))
	       ;; found it
	       node
	     (let ((closest (node '-closest-preceding-node address)))
	       (cond ((null closest) nil)
		     ((or (eq closest node) (memq closest seen))
		      (self '-debug "predecessor %d: detected loop" address)
		      closest)
		     (t (loop closest (cons node seen))))))))

	;; Find the closest node that we know about (using our finger
	;; table) which precedes ADDRESS
	((-closest-preceding-node address)
	 (let loop ((i 0)
		    (closest-node self)
		    (closest-dist (distance self-address address)))
	   (if (= i degree)
	       closest-node
	     (if (not (finger-ref i))
		 (loop (1+ i) closest-node closest-dist)
	       (if (not ((finger-ref i) 'joined?))
		   (progn
		     ;; clear out stale entries
		     (finger-set! i nil)
		     (loop (1+ i) closest-node closest-dist))
		 (let ((dist (distance ((finger-ref i) 'address) address)))
		   (if (< dist closest-dist)
		       (loop (1+ i) (finger-ref i) dist)
		     (loop (1+ i) closest-node closest-dist))))))))

	;; Return true if the node has been inserted already
	((joined?) in-network)

	;; Return true if the node is currently responsible for storing
	;; data with ADDRESS
	((responsible-for? address)
	 (and in-network previous
	      (between address (1+ (previous 'address)) (1+ self-address))))

	((responsibilities)
	 (cons (1+ (previous 'address)) (1+ self-address)))

	((foreach-known-successor fun)
	 (do ((i 0 (1+ i)))
	     ((= i known-successors))
	   (when (successor-ref i)
	     (fun (successor-ref i)))))

	;; Insert the node into the same network as SIBLING (which
	;; may be false, to start a new network)
	((join sibling)
	 (when in-network
	   (error "Trying to insert a node twice"))

	 (setq in-network t)
	 (self '-debug "joined the network")

	 (if (or (not sibling) (eq sibling self))
	     (progn
	       ;; adding the first node
	       (do ((i 0 (1+ i)))
		   ((= i known-successors))
		 (successor-set! i self))
	       (previous-set! self))

	   ;; insert between our immediate siblings. This may weird
	   ;; the network if multiple nodes join concurrently, but
	   ;; the stabilization feature will correct it over time

	   (let* ((pred (sibling 'predecessor self-address))
		  (succ (pred '-node-successor)))
	     (if (= (succ 'address) self-address)
		 (error "Adding node with same address as existing node")

	       (previous-set! pred)
	       (successor-set! 0 succ)))

	   (previous '-notify-successor self)
	   ((successor-ref 0) '-notify-predecessor self)
	   (self '-fix-successors)

	   (self '-debug "predecessor: %s, successor %s"
		 (previous 'address) ((successor-ref 0) 'address))

	   (self 'successor-changed (successor-ref 0) 0)
	   (self 'predecessor-changed previous))

	 ;; return true always
	 t)

	;; Remove the node from the network
	((leave)
	 (unless in-network
	   (error "Trying to remove a node that hasn't been inserted"))

	 (setq in-network nil)
	 (self '-debug "left the network")

	 ;; notify our immediate siblings
	 (when previous
	   ;; tell our successor that it's predecessor changed
	   ((successor-ref 0) '-notify-predecessor previous)

	   ;; it would be possible to wait for -fix-fingers to
	   ;; adapt the successor pointer of our predecessor, but
	   ;; during that time the network would be broken, so I
	   ;; think this is better...
	   (previous '-notify-successor (successor-ref 0)))

	 (self 'left-network)

	 (previous-set! nil)
	 (do ((i 0 (1+ i)))
	     ((= i known-successors))
	   (successor-set! i nil))
	 (do ((i 0 (1+ i)))
	     ((= i degree))
	   (finger-set! i nil))

	 ;; return true always
	 t)

	;; This needs to be called regularly to maintain the
	;; consistency of the ring
	((periodically)
	 ;; XXX is it ok to have these on the same schedule?
	 (self '-fix-successors)
	 (self '-stabilize)
	 (self '-fix-fingers)
	 (self '-print-fingers))

	;; Private methods

	;; Return the node after/before us
	((-node-successor k) (successor-ref (or k 0)))
	((-node-predecessor) previous)

	;; NODE thinks it may be our predecessor
	((-notify-predecessor node)
	 (when (or (null previous)
		   (not (previous 'joined?))
		   (between (node 'address) (previous 'address) self-address))
	   ;; it's right
	   (previous-set! node)
	   (self '-debug "set predecessor to %s" (node 'address))
	   (self 'predecessor-changed previous)
	   t))

	;; NODE thinks it may be our successor
	((-notify-successor node)
	 (when (or (null (successor-ref 0))
		   (not ((successor-ref 0) 'joined?))
		   (between (node 'address)
			    self-address ((successor-ref 0) 'address)))
	   ;; it's right
	   (successor-set! 0 node)
	   (self '-debug "set successor to %s" (node 'address))
	   (self 'successor-changed node 0)
	   t))

	((-fix-successors)
	 ;; Step 1. If the zeroth successor is gone, replace it with the
	 ;; first following successor
	 (let ((succ-0 (successor-ref 0)))
	   (when (or (null succ-0) (not (succ-0 'joined?)))
	     (self '-debug "successor disappeared")
	     (let loop ((k 1))
	       (if (= k known-successors)
		   ;; XXX in this case we could try looking at our
		   ;; XXX predecessor's second successor..?
		   (successor-set! 0 nil)
		 (let ((succ-k (successor-ref k)))
		   (if (and succ-k (succ-k 'joined?))
		       (progn
			 (successor-set! 0 succ-k)
			 (succ-k '-notify-predecessor self)
			 (self 'successor-changed succ-k 0))
		     (loop (1+ k))))))))

	 ;; Step 2. If we have a first successor. Use it to find the next
	 ;; successors
	 (when (successor-ref 0)
	   (do ((k 1 (1+ k)))
	       ((= k known-successors))
	     (successor-set!
	      k (and (successor-ref (1- k))
		     ((successor-ref (1- k)) '-node-successor)))))

	 ;; Step 3. Look at our predecessor. Now that the successors are
	 ;; (hopefully) ok, we can do address lookups..
	 (when (or (null previous) (not (previous 'joined?)))
	   (self '-debug "predecessor disappeared")
	   (previous-set! (or (self 'predecessor self-address) self))
	   (previous '-notify-successor self)
	   (self 'predecessor-changed previous)))

	;; Check that our successor points back to us, and adjust things
	;; if not
	((-stabilize)
	 ;; try to repair our neighbour's links
	 (let loop ()
	   (let ((x ((successor-ref 0) '-node-predecessor))
		 (original-successor (successor-ref 0)))
	     (cond ((null x)
		    ;; our successor has no predecessor, suggest ourself
		    ((successor-ref 0) '-notify-predecessor self))

		   ((and x (/= (x 'address) self-address))
		    ;; X is between us and who we thought was our successor,
		    ;; so X is obviously a better successor for us
		    (when (between (x 'address)
				   self-address ((successor-ref 0) 'address))
		      (successor-set! 0 x)
		      (self '-debug "set successor to %s" (x 'address))
		      (self 'successor-changed x 0))

		    ;; tell our successor that we think we're its predecessor
		    ((successor-ref 0) '-notify-predecessor self)))

	     (unless (eq original-successor (successor-ref 0))
	       ;; we have a new successor now, so try again
	       (loop)))))

	((-fix-fingers)
	 ;; refresh a random finger
	 (let* ((i (random degree))
		(node (self 'successor (finger->address i))))
	   (unless (eq (finger-ref i) node)
	     (finger-set! i node)
	     (self '-debug "set finger %d (%s) to %s"
		   i (finger->address i) (node 'address))
	     (self 'successor-changed node i))))

	;; Broadcasts a request to all nodes on the ring in log N steps
	((broadcast . method-and-args)
	 (apply self '-broadcast-stage (1- degree)
		total-positions method-and-args))

	;; Does the actual work
	((-broadcast-stage k bound . method-and-args)
	 (let loop ((k k)
		    (bound bound))
	   (if (< k 0)
	       ;; totally fanned out
	       (apply self method-and-args)
	     (let* ((peer (self 'successor (finger->address k)))
		    (peer-dist (and peer (distance self-address
						   (peer 'address)))))
	       (if (and peer (< peer-dist bound))
		   (progn
		     (apply peer '-broadcast-stage
			    (1- k) (- bound peer-dist) method-and-args)
		     (loop (1- k) peer-dist))
		 (loop (1- k) bound))))))

	;; Debugging junk

	((-print-fingers)
	 (self '-debug "finger table:")
	 (self '-debug " (predecessor %s): %s"
	       self-address (and previous (previous 'address)))
	 (do ((i 0 (1+ i)))
	     ((= i known-successors))
	   (self '-debug " (successor %d): %s"
		 i (and (successor-ref i) ((successor-ref i) 'address))))
	 (do ((i 0 (1+ i)))
	     ((= i degree))
	   (when (finger-ref i)
	     (self '-debug " (finger %d): (successor %s): %s"
		   i (finger->address i)
		   ((finger-ref i) 'address)))))

	((-debug fmt . args)
	 (when debug-mode
	   (let ((print-escape t))
	     (apply format standard-error
		    (format nil "%s: %s\n" self-address fmt) args))))

	((-set-debugging state) (setq debug-mode state))

	;; Null defaults for abstract methods
	((successor-changed k)
	 (declare (unused k)))
	((predecessor-changed))
	((successors-changed))
	((left-network)))))

;;; rpc method hints

  (define-rpc-method 'address #:result t)
  (define-rpc-method 'successor #:result 'object)
  (define-rpc-method 'predecessor #:result 'object)
  (define-rpc-method '-closest-preceding-node #:result 'object)
  (define-rpc-method 'responsible-for? #:result t)
  (define-rpc-method 'responsibilities #:result t)
  (define-rpc-method 'join #:result t #:object-args 0)
  (define-rpc-method 'leave #:result t)
  (define-rpc-method 'joined? #:result t)
  (define-rpc-method 'broadcast #:arglist 0)
  (define-rpc-method '-broadcast-stage #:arglist 2)
  (define-rpc-method '-node-successor #:result 'object)
  (define-rpc-method '-node-predecessor #:result 'object)
  (define-rpc-method '-notify-predecessor #:object-args 0 #:result t)
  (define-rpc-method '-notify-successor #:object-args 0 #:result t)
  (define-rpc-method 'predecessor-changed #:object-args 0)
  (define-rpc-method 'successor-changed #:object-args 0)


  (define (self-test)
    (let* ((node-addresses '(170 10 50 210 100))
	   (nodes (mapcar make-consistent-hash-node node-addresses))

	   (tests			;(ADDR PRED-ADDR SUCC-ADDR)
	    '((9 210 10) (10 210 10) (11 10 50)
	      (49 10 50) (50 10 50) (51 50 100)
	      (99 50 100) (100 50 100) (101 100 170)
	      (169 100 170) (170 100 170) (171 170 210)
	      (209 170 210) (210 170 210) (211 210 10))))
	      
      (define (stabilize)
	(do ((i 0 (1+ i)))
	    ((= i 7))
	  (mapc (lambda (n) (n 'periodically)) nodes)))

      ;; test successor and predecessor methods
      (define (run-tests)
	(mapc (lambda (n)
		(mapc (lambda (x)
			(let ((pred (n 'predecessor (car x)))
			      (succ (n 'successor (car x))))
			  (n '-debug "predecessor %d: %s, successor %d: %s"
			     (car x) (and pred (pred 'address))
			     (car x) (and succ (succ 'address)))
			  (test pred)
			  (test succ)
			  (test (= (pred 'address) (cadr x)))
			  (test (= (succ 'address) (caddr x)))))
		      tests)) nodes))

      ;; insert the first node
      ((car nodes) 'join)

      ;; add all other nodes in a non-ordered order
      (mapc (lambda (n) (n 'join (car nodes))) (cdr nodes))

      ;; print routing tables
      (mapc (lambda (n) (n '-print-fingers)) nodes)

      ;; test without fingers, should get linear-time lookups
      (run-tests)

      ;; create finger tables then test again, lookups should
      ;; approach log N now..
      (stabilize)
      (mapc (lambda (n) (n '-print-fingers)) nodes)
      (run-tests)

      ;; now delete nodes one at a time
      (let loop ((rest nodes))
	(when (cdr rest)
	  ((car rest) 'leave)
	  (loop (cdr rest))))))

  ;;###autoload
  (define-self-test 'dfs.peer.consistent-hash self-test))
