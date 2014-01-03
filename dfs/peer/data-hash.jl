#| data-hash.jl -- implement key/value storage on top of consistent-hash-node
   $Id: data-hash.jl,v 1.19 2002/03/28 07:10:52 jsh Exp $
|#

(define-structure dfs.peer.data-hash

    (export make-data-hash-node)

    (open rep
	  dfs.peer.consistent-hash
	  dfs.common.transfer
	  dfs.common.util
	  dfs.common.rpc-hints
	  rep.data.objects
	  rep.data.tables
	  rep.io.files
	  rep.system
	  rep.util.time
	  rep.test.framework)

  ;; should match known-successors in consistent-hash?
  (define starting-ttl 3)

  ;; SUPER should be a consistent-hash object
  (define (make-data-hash-node #!key super address)

    ;; map from TOKEN -> (CALLBACK FILE)
    (define active-tokens (make-table string-hash string=))

    (define local-transfer/end
      (transfer/end
       (lambda (token key file)
	 (declare (unused key))
	 (let ((callback (or (table-ref active-tokens token)
			     (error "No callback for token: %s" token))))
	   (table-unset active-tokens token)
	   ;; FILE is false if an error occurred
	   (callback file)))))

    (define (new-incoming key callback)
      (let ((token (transfer/begin key (lambda () (make-temp-name)))))
	(table-set active-tokens token callback)
	token))

    (unless super
      (setq super (make-consistent-hash-node address)))
    
    (object super
      ((class) 'data-hash)

      ;; Abstract (private) methods:
      ;;  (-key-address KEY) -> ADDR
      ;;  (-key-defined? KEY) -> BOOL
      ;;  (-key-delete! KEY)
      ;;  (-key-import! KEY FILENAME)
      ;;  (-key-export KEY (CALLBACK FILENAME))
      ;;  (-key-foreach FUNCTION)
      ;;  (-key-lease-set! KEY LEASE)
      ;;  (-key-lease-ref KEY) -> LEASE
      ;;  (-key-dirty KEY)
      ;;  (-key-undirty KEY)
      ;;  (-key-dirty? kEY)
      ;;  (-key-dirty-foreach FUNCTION)
      ;;  [optional:]
      ;;  (-key-size KEY)
      ;;  (-total-size)
      ;;  (-target-size)

      ;; Globally set the value associated with KEY, returned a TOKEN
      ;; if the data needs to be sent
      ((define key lease)
       (or key (error "Invalid key: %s" key))
       (or (integerp lease) (error "Invalid lease: %s" lease))
       ;; Originally this was more complex, it would try to find the
       ;; primary storage of the data, and extend the lease there. But
       ;; my theory is that data files are usually small, and with
       ;; aggressive caching, often on the local node
       (if (self '-key-defined? key)
	   (progn
	     ;; just extend the lease, return no token
	     (self '-key-lease-set! key lease)
	     nil)
	 ;; get the data to our local filestore, return the token
	 (new-incoming key (lambda (file)
			     (when file
			       (self '-key-lease-set! key lease)
			       (self '-key-import! key file)
			       ;; mark that this can't be gc'd yet
			       (self '-key-dirty key))))))

      ;; Do a multi-key `define' operation, returns a list of (KEY . TOKEN)
      ((define* key-lease-pairs)
       (let loop ((rest key-lease-pairs)
		  (out '()))
	 (if (null rest)
	     (nreverse out)
	   (let ((token (self 'define (caar rest) (cdar rest))))
	     (if token
		 (loop (cdr rest) (cons (cons (caar rest) token) out))
	       (loop (cdr rest) out))))))

      ((defined? key)
       (or key (error "Invalid key: %s" key))
       (or (self '-key-defined? key)
	   (let ((succ (or (self 'successor (self '-key-address key))
			   (error "Can't locate successor of key %S" key))))
	     (succ '-key-defined? key))))

      ;; Fetch the value globally associated with KEY. TTL is optional
      ((fetch key object token ttl)
       (or key (error "Invalid key: %s" key))
       (or (functionp object) (error "Invalid object: %s" object))
       (if (self '-key-defined? key)
	   ;; we already have this data cached, so just send it back
	   (self '-key-export key
		 (lambda (file)
		   (transfer-file object token file)))
	 ;; else try to cache the data here before sending it back
	 (let ((succ (if (self 'responsible-for? (self '-key-address key))
			 ;; the data went missing somehow.. try our successor
			 ;; before failing
			 (self '-node-successor)
		       ;; find the real location
		       (self 'successor (self '-key-address key)))))
	   (if (and succ (succ 'joined?))
	       ;; create a token so the data gets sent to us, not our
	       ;; client (so we can cache it)
	       (let ((t2 (new-incoming
			  key
			  (lambda (file)
			    (if (not file)
				;; fetch failed
				(transfer-failed object token)
			      (let ((lease (succ '-key-lease-ref key)))
				(when lease
				  (self '-key-lease-set! key lease)))
			      (self '-key-import! key file)
			      (self '-key-export key
				    (lambda (file)
				      (transfer-file object token file))))))))
		 (if (self 'responsible-for? (self '-key-address key))
		     ;; wander a few steps around the ring in case
		     ;; any of our successors have the data (it may have
		     ;; been mirrored to them, or if we just joined, they
		     ;; may have been the previous owner of the data)
		     (succ '-fetch-next key self t2 starting-ttl)
		   ;; we're not the primary location, just pass it off..
		   (let ((new-ttl (if ttl (1- ttl) starting-ttl)))
		     (if (> new-ttl 0)
			 (succ 'fetch key self t2 new-ttl)
		       (transfer-failed object token)))))
	     (transfer-failed object token)))))

      ;; If a key isn't found at it's primary storage location, walk a few
      ;; steps around the ring looking for it
      ((-fetch-next key object token ttl)
       (if (self '-key-defined? key)
	   (self '-key-export key
		 (lambda (file)
		   (transfer-file object token file)))
	 (let ((succ (self '-node-successor)))
	   (if (and (> ttl 1) succ (succ 'joined?))
	       (succ '-fetch-next key object token (1- ttl))
	     (transfer-failed object token)))))

      ((relocate-data)
       ;; 1. Flush dirty data items to their primary nodes
       (let ((now (time))
	     (dirty '()))
	 (self '-key-dirty-foreach (lambda (k) (setq dirty (cons k dirty))))
	 (mapc (lambda (key)
		 (let ((lease (self '-key-lease-ref key)))
		   (when (and lease (> lease now))
		     (let ((addr (self '-key-address key)))
		       (if (self 'responsible-for? addr)
			   (self '-key-undirty key)
			 (let ((succ (self 'successor addr)))
			   (when (and succ (succ 'joined?))
			     (let ((token (succ 'define key lease)))
			       (when token
				 (self '-key-export key
				       (lambda (file)
					 (transfer-file succ token file))))
			       (self '-key-undirty key))))))))) dirty))

       ;; 2. Mirror our data to our immediate successors
       (let ((range (self 'responsibilities)))	;(START . END)
	 (self 'foreach-known-successor
	       (lambda (node)
		 (self 'relocate node (car range) (cdr range))))))

      ;; relocate any keys with addresses between START and END to DEST
      ((relocate dest start end)
       (let ((now (time))
	     (to-relocate '()))
	 (self '-key-foreach
	       (lambda (key)
		 (let ((lease (self '-key-lease-ref key)))
		   (when (and lease (> lease now)
			      (between (self '-key-address key) start end))
		     (setq to-relocate (cons (cons key lease) to-relocate))))))
	 ;; batch up the requests for good measure..
	 (let ((chunks (split-list! to-relocate 64)))
	   (do ((rest chunks (cdr chunks)))
	       ((null rest))
	     (let ((tokens (dest 'define* (car rest))))
	       (mapc (lambda (cell)
		       (let ((key (car cell))
			     (token (cdr cell)))
			 (self '-key-export key
			       (lambda (file)
				 (transfer-file dest token file)))))
		     tokens))))))

      ((garbage-collect)
       (let ((now (time))
	     (collected '())
	     (collectable '()))
	 ;; 1. generate a list of files that we can safely delete, and a
	 ;; list of files that should always be deleted
	 (self '-key-foreach
	       (lambda (key)
		 (let ((lease (self '-key-lease-ref key)))
		   (cond ((<= lease now)
			  (setq collected (cons key collected)))
			 ((not (or (self 'responsible-for?
					 (self '-key-address key))
				   (self '-key-dirty? key)))
			  (setq collectable (cons key collectable)))))))
	 ;; 2. delete all expired keys
	 (when collected
	   (self '-debug "GC: Expired: %s" collected))
	 (mapc (lambda (x)
		 (self '-key-delete! x)) collected)
	 ;; 3. randomly choose files that can be deleted until we're
	 ;; under our target size
	 (let* ((current-size (self '-total-size))
		(target-size (self '-target-size))
		(vec (list->vector collectable))
		(left (length vec)))
	   (while (and (> left 0) (> current-size target-size))
	     (let ((idx (random (length vec))))
	       (when (aref vec idx)
		 (setq current-size (- current-size
				       (self '-key-data-size (aref vec idx))))
		 (self '-debug "GC: Collected: %s" (aref vec idx))
		 (self '-key-delete! (aref vec idx))
		 (aset vec idx nil)
		 (setq left (1- left))))))))

      ((-print-keys)
       (self '-debug "keys:")
       (self '-key-foreach
	     (lambda (key)
	       (let ((lease (self '-key-lease-ref key)))
		 (self '-debug "%s: @%s (%s) %s"
		       key (mod (self '-key-address key) 65536) lease
		       (if (self 'responsible-for? (self '-key-address key))
			   "+" " "))))))

      ((successor-changed node k)
       (super 'successor-changed node k #:self self)
       (let ((range (self 'responsibilities)))
	 ;; ask our successor to send us any data it has that we should
	 ;; be storing..
	 (node 'relocate self (car range) (cdr range))))

      ((periodically)
       (super 'periodically #:self self)
       (self 'relocate-data)
       (self 'garbage-collect))

      ((left-network)
       (super 'left-network #:self self))

      (transfer/send transfer/send)
      (transfer/end local-transfer/end)
      (transfer/filesystem-id transfer/filesystem-id)
      (transfer/send-file transfer/send-file)

      ;; default values for optional methods
      ((-total-size) 0)
      ((-target-size) 1)
      ((-key-size key) (declare (unused key)) 0)))

;;; rpc method hints

  (define-rpc-method 'relocate #:object-args 0)
  (define-rpc-method 'fetch #:object-args 1)
  (define-rpc-method '-fetch-next #:object-args 1)
  (define-rpc-method 'define #:result t)
  (define-rpc-method 'define* #:result t)
  (define-rpc-method '-key-defined? #:result t)
  (define-rpc-method '-key-lease-ref #:result t))
