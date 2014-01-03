#| proxy-client.jl -- 
   $Id: proxy-client.jl,v 1.2 2002/03/28 07:10:53 jsh Exp $
|#

(define-structure dfs.peer.proxy-client

    (export make-proxy-client)

    (open rep
	  rep.data.objects
	  rep.test.framework
	  ;; for rpc hints
	  dfs.peer.proxy-server)

  ;; This object is a replacement for the consistent-hash object. Pass
  ;; a reference to a proxy-server object to the 'join method to make
  ;; things work

  (define (make-proxy-client self-address)

    (let ((proxy nil)
	  (tag nil)
	  (debug-mode nil))

      ;; Need to handle any methods our subclasses may call on us,
      ;; they get reflected to the proxy server, with our tag so it
      ;; can see which of its clients the request came from

      (object ()
	((class) 'proxy-client)

	((address) self-address)

	((successor address)
	 (proxy 'proxy/successor tag address))

	((responsible-for? address)
	 ;; XXX cache what we're responsible for..?
	 (proxy 'proxy/responsible-for? tag address))

	((responsibilities)
	 ;; XXX cache what we're responsible for..?
	 (proxy 'proxy/responsibilities tag))

	((joined?) (and proxy t))

	((join node)
	 (when proxy
	   (error "Trying to insert a node twice"))
	 (setq tag (node 'proxy/join self))
	 (setq proxy node))

	((leave)
	 (unless proxy
	   (error "Trying to remove a node that hasn't been inserted"))
	 (proxy 'proxy/leave tag)
	 t)

	((broadcast . method-and-args)
	 (apply proxy 'proxy/broadcast tag method-and-args))

	((periodically)
	 (when proxy
	   (proxy 'proxy/periodically tag)))

	((-debug fmt . args)
	 (when debug-mode
	   (let ((print-escape t))
	     (apply format standard-error
		    (format nil "%s: %s\n" self-address fmt) args))))

	((-set-debugging state) (setq debug-mode state))

	;; Null default abstract methods
	((successor-changed k)
	 (declare (unused k)))
	((predecessor-changed))
	((left-network))))))
