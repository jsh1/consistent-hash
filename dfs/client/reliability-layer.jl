#| reliability-layer -- file storage using FEC to add redundancy
   $Id: reliability-layer.jl,v 1.7 2002/03/25 01:10:42 jsh Exp $
|#

(define-structure dfs.client.reliability-layer

    (export reliability-layer/define
	    reliability-layer/defined?
	    reliability-layer/fetch)

    (open rep
	  rep.io.files
	  rep.regexp
	  rep.system
	  dfs.client.storage-layer
	  dfs.common.util
	  vandermonde)

  (define fec-n 16)			;total number of blocks
  (define fec-k 8)			;blocks needed to reconstruct

  (define (block-key key i) (format nil "%s:%02x" key i))

  (define (block-key/key k)
    (and (string-looking-at "^(.*):([0-9a-zA-Z]+)$" k)
	 (expand-last-match "\\1")))

  (define (block-key/block k)
    (and (string-looking-at "^(.*):([0-9a-zA-Z]+)$" k)
	 (string->number (expand-last-match "\\2") 16)))
  
  (define (copy-chars input output size)
    (do ((i 0 (+ i 1024)))
	((> i size))
      (let ((bytes (read-chars input (min 1024 (- size i)))))
	(write output bytes))))

  (define (delete-file* x)
    (condition-case nil
	(when (file-exists-p x)
	  (delete-file x))
      (file-error)))

  (define (randomize-list lst)
    (let loop ((rest (copy-sequence lst))
	       (left (length lst))
	       (out '()))
      (if (null rest)
	  out
	(let ((chosen (nth (random left) rest)))
	  (loop (delq chosen rest) (1- left) (cons chosen out))))))

  ;; The block header is a string terminated by a \012 character. It's
  ;; formatted as `(BLOCK-INDEX K N BLOCK-SIZE EXTRA-BYTES)' where all
  ;; parameters are decimal integers

  (define (write-block-header output index block-size extra)
    (format output "(%d %d %d %d %d)\n" index fec-k fec-n block-size extra))

  (define (parse-block-header string)
    (and (string-looking-at
	  "^\\((\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\)\n" string)
	 (mapcar (lambda (x) (string->number (expand-last-match x) 10))
		 '("\\1" "\\2" "\\3" "\\4" "\\5"))))

  (define (make-block-keys key n)
    (do ((i 0 (1+ i))
	 (out '() (cons (block-key key i) out)))
	((= i n) (nreverse out))))

  (define (define-1 key lease callback)
    (let ((encoded-file-name nil)
	  (encoded-file nil)
	  (block-size nil)
	  (extra-bytes nil)
	  (temp-files '()))

      (define (write-encoded-block output i)
	(unless encoded-file
	  (setq encoded-file-name (make-temp-name))
	  (let ((input-file (callback key)))
	    (setq block-size (or (vandermonde-encode-file
				  input-file encoded-file-name fec-k fec-n)
				 (error "Undefined block size!")))
	    (setq extra-bytes (- (* fec-k block-size) (file-size input-file)))
	    (setq encoded-file (open-file encoded-file-name 'read))))
	(seek-file encoded-file (* block-size i) 'start)
	(write-block-header output i block-size extra-bytes)
	(copy-chars encoded-file output block-size))

      (define (data-callback k)
	(let ((i (block-key/block k))
	      (output-file (make-temp-name)))
	  (setq temp-files (cons output-file temp-files))
	  (unwind-protect
	      (let ((output (open-file output-file 'write)))
		(unwind-protect
		    (progn
		      (write-encoded-block output i)
		      output-file)
		  (close-file output))))))

      (unwind-protect
	  (storage-layer/define
	   (make-block-keys key fec-n) lease data-callback)
	(mapc delete-file* temp-files)
	(when encoded-file
	  (close-file encoded-file)
	  (delete-file* encoded-file-name)))))

  (define (fetch-1 key callback)
    (let* ((block-file-name (make-temp-name))
	   (block-file nil)
	   (block-size nil)
	   (extra-bytes nil)
	   (block-list '()))

      (define (new-block header)
	(if block-size
	    (or (= block-size (nth 3 header))
		(error "Block sizes don't match!"))
	  (setq block-size (nth 3 header))
	  (setq extra-bytes (nth 4 header)))
	(if (memql (nth 0 header) block-list)
	    nil
	  (setq block-list (cons (nth 0 header) block-list))
	  t))

      (define (have-enough-blocks)
	(>= (length block-list) fec-k))

      (define (data-callback key data-file)
	(declare (unused key))
	(let ((file (open-file data-file 'read)))
	  (unwind-protect
	      (let ((header (parse-block-header (read-line file 'read))))
		(when (new-block header)
		  (copy-stream file block-file)))
	    (close-file file)
	    (delete-file* data-file)))
	(when (have-enough-blocks)
	  (throw 'done t)))

      (unwind-protect
	  (progn
	    (setq block-file (open-file block-file-name 'write))
	    (catch 'done
	      ;; XXX remove the randomization once we're happy it works..
	      (storage-layer/fetch
	       (randomize-list (make-block-keys key fec-n)) data-callback))
	    (close-file block-file)
	    (when (have-enough-blocks)
	      (let ((out-file (make-temp-name)))
		(unwind-protect
		    (if (vandermonde-decode-file fec-k fec-n
						 block-size extra-bytes
						 block-file-name
						 (reverse block-list)
						 out-file)
			(callback key out-file)
		      (error "Can't decode file"))
		  (delete-file* out-file)))))
	(delete-file* block-file-name))))

  (define (reliability-layer/define keys lease callback)
    (mapc (lambda (x) (define-1 x lease callback)) keys))

  (define (reliability-layer/defined? key)
    ;; XXX lame
    (catch 'out
      (fetch-1 key (lambda () (throw 'out t)))
      nil))

  (define (reliability-layer/fetch keys callback)
    (mapc (lambda (x) (fetch-1 x callback)) keys)))
