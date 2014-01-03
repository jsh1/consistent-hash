
(define-structure dfs.common.util

    (export time
	    split-list!
	    read-string-from-file
	    write-string-to-file)

    (open rep
	  rep.system
	  rep.io.files
	  rep.util.time)

  ;; Return the current time in seconds from the unix epoch
  (define (time)
    (time->seconds (current-time)))

  ;; Split LST into a list of N sublists destructively
  (define (split-list! lst n)
    (let loop ((rest lst)
	       (out '()))
      (if (null rest)
	  (nreverse out)
	(let ((end (nthcdr (1- n) rest)))
	  (if (null end)
	      (nreverse (cons rest out))
	    (loop (prog1
		      (cdr end)
		    (rplacd end nil))
		  (cons rest out)))))))

  ;; Return the contents of FILENAME as a string
  (define (read-string-from-file filename)
    (and (file-exists-p filename)
         (let ((file (open-file filename 'read)))
           (unwind-protect
               (read-chars file (file-size filename))
             (close-file file)))))

  ;; Write STRING to FILENAME
  (define (write-string-to-file filename string)
    (let ((file (open-file filename 'write)))
      (unwind-protect
          (write file string)
        (close-file file)))))
