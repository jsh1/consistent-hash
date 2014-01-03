#| proxy-server.jl -- 
   $Id: proxy-server.jl,v 1.2 2002/03/28 07:10:53 jsh Exp $
|#

(define-structure dfs.peer.proxy-server

    (export make-proxy-server)

    (open rep
	  rep.data.objects
	  rep.data.tables
	  rep.test.framework
	  dfs.peer.consistent-hash
	  dfs.common.rpc-hints)

  ;; constructor for objects used to reflect callbacks from consistent
  ;; hash object to the client of the proxy
  (define (make-proxy-stub remote-node)
    (let ((base (make-consistent-hash-node (remote-node 'address))))
      (object-lambda (method . args)
	(apply (if (memq method consistent-hash-public-methods)
		   ;; This goes to the local consistent hash object..
		   base
		 ;; ..while this goes to the remote node we're proxying for
		 remote-node)
	       method #:self self args))))

  (define make-proxy-tag
    (let ((counter 0))
      (lambda ()
	(setq counter (1+ counter))
	counter)))

  (define (make-proxy-server sibling)

    (let ((proxy-table (make-table eq-hash eq)))	;TAG->LOCAL

      (define (proxy-ref tag) (table-ref proxy-table tag))

      (object ()
	((class) 'proxy-server)

	((proxy/join remote-node)
	 (let ((tag (make-proxy-tag))
	       (proxy (make-proxy-stub remote-node)))
	   ;; XXX what if SIBLING went away?
	   (sibling 'join proxy)
	   (table-set proxy-table tag proxy)
	   tag))

	((proxy/leave tag)
	 (let ((proxy (proxy-ref tag)))
	   (when (and proxy (proxy 'joined?))
	     (proxy 'leave))
	   (table-unset proxy-table tag)))

	((proxy/successor tag address)
	 ((proxy-ref tag) 'successor address))

	((proxy/responsible-for? tag address)
	 ((proxy-ref tag) 'responsible-for? address))

	((proxy/responsibilities tag)
	 ((proxy-ref tag) 'responsibilities))

	((proxy/broadcast tag . method-and-args)
	 (apply (proxy-ref tag) 'broadcast method-and-args))

	((proxy/periodically tag)
	 ((proxy-ref tag) 'periodically)))))

;;; rpc method hints

  (define-rpc-method 'proxy/join #:result t #:object-args 0)
  (define-rpc-method 'proxy/leave #:result t)
  (define-rpc-method 'proxy/responsible-for? #:result t)
  (define-rpc-method 'proxy/responsibilities #:result t)
  (define-rpc-method 'proxy/broadcast #:arglist 1)
  (define-rpc-method 'proxy/successor #:result 'object))
