#| dfs.peer.chat -- proof of concept distributed chat system
   $Id: chat.jl,v 1.2 2002/03/31 03:41:05 jsh Exp $
|#

(define-structure dfs.peer.chat

    (export make-chat-node)

    (open rep
	  rep.data.objects
	  dfs.peer.consistent-hash)

  (define (make-chat-node #!key super address nick)

    (unless super
      (setq super (make-consistent-hash-node address)))

    (object super
      ((class) 'chat-node)

      ((output fmt . args)
       (apply format standard-output fmt args))

      ;; connection management

      ((join sibling)
       (when (and (super 'join #:self self sibling) nick)
	 (self 'broadcast 'joined address nick)))

      ((leave)
       (when nick
	 (self 'broadcast 'left address nick))
       (super 'leave #:self self))

      ((joined addr nick)
       (self 'output "--> %s (%s) joined\n" nick addr))

      ((left addr nick)
       (self 'output "<-- %s (%s) left\n" nick addr))

      ;; nickname management

      ((nick-ref) nick)

      ((nick-set! new)
       (self 'broadcast 'nick-changed address nick new)
       (setq nick new))

      ((nick-changed addr old new)
       (self 'output "--- %s (%s) is now known as %s\n" old addr new))

      ;; message handling
      
      ((say msg)
       (self 'broadcast 'said (and nick address) nick msg))

      ((said addr nick msg)
       (declare (unused addr))
       (self 'output "<%s> %s\n" (or nick "anon") msg)))))
