
;; client-side access to the data-hash

(define-structure dfs.client.storage-layer

    (export storage-layer/define
	    storage-layer/defined?
	    storage-layer/fetch)

    (open rep
	  rep.system
	  rep.io.files
	  rep.io.processes
	  rep.data.tables
	  rep.data.objects
	  rep.net.rpc
	  rep.util.time
	  dfs.common.rpc-proxy
	  dfs.common.transfer
	  dfs.common.util
	  ;; for rpc hints
	  dfs.peer.data-hash)

  (define server-rpc-id (getenv "SERVER_RPC_ID"))

  (define callbacks (make-table string-hash string=))

  (define proxy
    (let ((obj nil))
      (lambda ()
	(unless obj
	  (or server-rpc-id (error "Set $SERVER_RPC_ID to point to your node"))
	  (setq obj (rpc-id->hash-object server-rpc-id)))
	obj)))

  (define (new-token key callback)
    (let ((token (transfer/begin key (lambda () (make-temp-name)))))
      (table-set callbacks token callback)
      token))

  (define client-servant
    (let (servant)
      (lambda ()
	(unless servant
	  (setq servant (make-rpc-servant
			 (object ()
			   (transfer/send transfer/send)
			   (transfer/end
			    (transfer/end
			     (lambda (token key file)
			       (let ((callback (table-ref callbacks token)))
				 (table-unset callbacks token)
				 (when (and callback file)
				   (callback key file))))))
			   (transfer/filesystem-id transfer/filesystem-id)
			   (transfer/send-file transfer/send-file)))))
	servant)))

  (define (tokens-outstanding tokens)
    (let loop ((rest tokens))
      (if (null rest)
	  nil
	(if (table-ref callbacks (car rest))
	    t
	  (loop (cdr rest))))))

  (define (storage-layer/define keys lease callback)
    (unless (listp keys)
      (error "Need a list of keys: %s" keys))
    (mapc (lambda (key)
	    (let ((token ((proxy) 'define key (+ (time) lease))))
	      (when token
		(let ((data-file (callback key)))
		  (transfer-file (proxy) token data-file))))) keys))

  (define (storage-layer/defined? key)
    ((proxy) 'defined? key))

  (define (storage-layer/fetch keys callback)
    (unless (listp keys)
      (error "Need a list of keys: %s" keys))
    (let ((tokens (mapcar (lambda (k) (new-token k callback)) keys)))
      (let loop ((rest-keys keys)
		 (rest-tokens tokens))
	(when rest-keys
	  ((proxy) 'fetch
		   (car rest-keys)
		   ;; pass a local servant-id. The receiving rpc
		   ;; proxy will do the right thing with it..
		   (client-servant)
		   (car rest-tokens))
	  (loop (cdr rest-keys) (cdr rest-tokens))))
      ;; wait for all keys to be received, since `fetch' may be called
      ;; asynchronously
      (while (tokens-outstanding tokens)
	(accept-process-output 60)))))
