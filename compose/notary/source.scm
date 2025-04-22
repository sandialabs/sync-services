(lambda (seed periodicity cryptography promise-interval proof-interval)
  (define verifiable-structures
    '(begin
       ;; --- convenience functions
       ((macro ()
          "Define combinations of sync-car and sync-cdr"
	  (cons 'begin
		(let recurse ((name "") (body 'x))
		  (if (> (length name) 4) '()
		      (append (if (< (length name) 2) '()
				  `((define (,(string->symbol (append "sync-c" name "r")) x) ,body)))
			      (recurse (append "a" name) `(sync-car ,body))
			      (recurse (append "d" name) `(sync-cdr ,body))))))))

       (define (expr->word x) (expression->byte-vector x))

       (define (word->expr x) (byte-vector->expression x))

       (define (expr->str x) (byte-vector->string (expression->byte-vector x)))

       (define (str->expr x) (byte-vector->expression (string->byte-vector x)))

       ;; --- verifiable map ---

       (define (nth-bit x n)
	 (zero? (logand (byte-vector-ref (sync-hash x) (floor (/ n 8)))
			(ash 1 (modulo n 8)))))

       (define (sync-leaf? x) (and (not (sync-null? x)) (equal? (sync-car x) (sync-cdr x))))

       (define (split-list items test)
	 (let loop ((items (reverse items)) (a '()) (b '()))
	   (if (null? items) (cons a b)
	       (if (test (car items))
		   (loop (cdr items) (cons (car items) a) b)
		   (loop (cdr items) a (cons (car items) b))))))

       (define (sync-map-new . args)
	 (if (null? args) (sync-null)
	     (sync-map-batch-set (sync-null) (car args))))

       (define (sync-map-batch-set root pairs)
	 (let recurse ((node root) (pairs pairs) (depth 0))
	   (let* ((is-leaf (sync-leaf? node))
		  (pairs (if is-leaf (cons (cons (sync-caar node) (sync-cdar node)) pairs) pairs))
		  (node (if is-leaf (sync-null) node))
		  (pairs (if (and (> (length pairs) 1) (equal? (caar pairs) (caadr pairs))) (cdr pairs) pairs)))
	     (cond
	      ((= (length pairs) 0) node)
	      ((and (= (length pairs) 1) (sync-null? node))
	       (if (sync-null? (cdar pairs)) (sync-null)
		   (let ((leaf (sync-cons (caar pairs) (cdar pairs))))
		     (sync-cons leaf leaf))))
	      (else
	       (let* ((split (split-list pairs (lambda (x) (nth-bit (car x) depth))))
		      (left-old (if (sync-null? node) (sync-null) (sync-car node)))
		      (right-old (if (sync-null? node) (sync-null) (sync-cdr node)))
		      (left-new (recurse left-old (car split) (+ depth 1)))
		      (right-new (recurse right-old (cdr split) (+ depth 1))))
		 (cond
		  ((and (sync-null? left-new) (sync-null? right-new)) (sync-null))
		  ((and (sync-null? right-new) (sync-leaf? left-new)) left-new)
		  ((and (sync-null? left-new) (sync-leaf? right-new)) right-new)
		  (else (sync-cons left-new right-new)))))))))

       (define (sync-map-batch-get root keys)
	 (let recurse ((node root) (keys keys) (depth 0))
	   (cond
	    ((= (length keys) 0) '())
	    ((sync-null? node)
	     (let loop ((keys keys) (pairs '()))
	       (map (lambda (x) (cons x (sync-null))) keys)))
	    ((sync-leaf? node)
	     (let ((key (sync-caar node)) (value (sync-cdar node)))
	       (map (lambda (x) (cons x (if (equal? x key) value (sync-null)))) keys)))
	    (else
	     (let ((split (split-list keys (lambda (x) (nth-bit x depth)))))
	       (let loop ((keys (reverse keys))
			  (left-pairs (recurse (sync-car node) (car split) (+ depth 1)))
			  (right-pairs (recurse (sync-cdr node) (cdr split) (+ depth 1)))
			  (pairs '()))
		 (cond
		  ((null? keys) pairs)
		  ((and (not (null? left-pairs)) (equal? (car keys) (caar left-pairs)))
		   (loop (cdr keys) (cdr left-pairs) right-pairs (cons (car left-pairs) pairs)))
		  ((and (not (null? right-pairs)) (equal? (car keys) (caar right-pairs)))
		   (loop (cdr keys) left-pairs (cdr right-pairs) (cons (car right-pairs) pairs))))))))))

       (define (sync-map-batch-prove root keys)
	 (let recurse ((node root) (keys keys) (path '()))
	   (cond
	    ((= (length keys) 0) '())
	    ((sync-null? node)
	     (let loop ((keys keys) (pairs '()))
	       (map (lambda (x) (cons x (cons (sync-null) path))) keys)))
	    ((sync-leaf? node)
	     (let ((key (sync-caar node)) (value (sync-cdar node)))
	       (map (lambda (x) (cons x (cons (if (equal? x key) value (sync-null))
					      (map sync-pair->byte-vector path)))) keys)))
	    (else
	     (let ((split (split-list keys (lambda (x) (nth-bit x (length path))))))
	       (let ((left (sync-car node)) (right (sync-cdr node)))
		 (let loop ((keys (reverse keys))
			    (left-pairs (recurse left (car split) (cons right path)))
			    (right-pairs (recurse right (cdr split) (cons left path)))
			    (pairs '()))
		   (cond
		    ((null? keys) pairs)
		    ((and (not (null? left-pairs)) (equal? (car keys) (caar left-pairs)))
		     (loop (cdr keys) (cdr left-pairs) right-pairs (cons (car left-pairs) pairs)))
		    ((and (not (null? right-pairs)) (equal? (car keys) (caar right-pairs)))
		     (loop (cdr keys) left-pairs (cdr right-pairs) (cons (car right-pairs) pairs)))))))))))

       (define (sync-map-set root key value)
	 (sync-map-batch-set root `((,key . ,value))))

       (define (sync-map-get root key)
	 (cdar (sync-map-batch-get root `(,key))))

       (define (sync-map-prove root key)
	 (cdar (sync-map-batch-prove root `(,key))))

       (define (sync-map-infer key value proof)
	 (let ((leaf (sync-hash (append (sync-hash (sync-hash key)) (sync-hash (sync-hash value))))))
	   (let loop ((node (sync-hash (append leaf leaf))) (proof proof))
	     (if (null? proof) node
		 (if (nth-bit key (- (length proof) 1))
		     (loop (sync-hash (append node (car proof))) (cdr proof))
		     (loop (sync-hash (append (car proof) node)) (cdr proof)))))))

       (define (sync-map-all root)
	 (let recurse ((node root))
	   (cond
	    ((sync-null? node) '())
	    ((sync-leaf? node) (list (cons (sync-cadr node) (sync-cddr node))))
	    (else (append (recurse (sync-car node)) (recurse (sync-cdr node)))))))

       ;; --- verifiable log chain ---

       ;; TODO: add logithmic hopping logic

       (define (sync-chain-new . args)
	 (if (null? args)
	     (sync-cons (expr->word 0) (sync-cons (sync-null) (sync-null)))
	     (sync-cons (expr->word 1) (sync-cons (car args) (sync-null)))))

       (define (sync-chain-length chain)
	 (word->expr (sync-car chain)))

       (define (sync-chain-data chain)
	 (sync-cadr chain))

       (define (sync-chain-previous chain)
	 (sync-cddr chain))

       (define (sync-chain-push chain data)
	 (sync-cons (expr->word (+ (word->expr (sync-car chain)) 1))
		    (sync-cons data chain)))

       (define (sync-chain-sub chain index)
	 (if (or (< index 0) (>= index (sync-chain-length chain)))
	     (error 'structure-error "Invalid chain index")
	     (let loop ((chain chain))
	       (if (= index (- (word->expr (sync-car chain)) 1)) chain
		   (loop (sync-cddr chain))))))))

  (define transition
    `(lambda (*sync-state* query)
       ,verifiable-structures

       (define current (sync-chain-length (sync-cdddr *sync-state*)))

       (define (adjust periodicity offset index)
	 (+ (* (ceiling (/ index (expt 2 periodicity))) (expt 2 periodicity))
	    (* offset (expt 2 periodicity))))

       (define* (handle-block (periodicity 0) (offset 0) (index current) )
	 (let ((index (adjust periodicity offset index)))
	   (cond 
	    ((< index 0) (error 'negative-index-error "Index is less than zero"))
	    ((> index current) (cons `((index . ,index)) *sync-state*))
	    ((= index current) (cons `((index . ,index)
				       (previous . ,(sync-chain-previous (sync-cdddr *sync-state*))))
				     *sync-state*))
	    (else (let ((chain (sync-chain-sub (sync-cdddr *sync-state*) index)))
		    (cons `((index . ,index)
			    (previous . ,(sync-chain-previous chain))
			    (state . ,(sync-chain-data chain)))
			  *sync-state*))))))

       (define* (handle-promise key value (periodicity 0) (offset 0) (index current))
	 (let ((index (adjust periodicity offset index))
	       (interval (word->expr (sync-map-get (sync-cadr *sync-state*) (expr->word 'promise-interval)))))
	   (cond
	    ((< index 0) (error 'negative-index-error "Index is less than zero"))
	    ((< index current) (error 'past-index-error "Index is in the past"))
	    ((>= index (+ current interval)) (error 'future-index-error "Index is too far in the future"))
	    (else (let* ((old (sync-map-get (sync-caddr *sync-state*) (expr->word index)))
			 (new (sync-map-set old key value)))
		    (cons `((index . ,index) (key . ,key) (value . ,value))
			  (sync-cons (sync-car *sync-state*)
				     (sync-cons (sync-cadr *sync-state*)
						(sync-cons (sync-map-set (sync-caddr *sync-state*)
									 (expr->word index) new)
							   (sync-cdddr *sync-state*))))))))))

       (define (handle-proof index key)
	 (let ((interval (word->expr (sync-map-get (sync-cadr *sync-state*) (expr->word 'proof-interval)))))
	   (cond
	    ((< index 0) (error 'negative-index-error "Index is less than zero"))
	    ((>= index current) (error 'future-index-error "Index is in the future"))
	    ((< index (- current interval)) (error 'past-index-error "Index is too far in the past"))
	    (else (let* ((state (sync-map-get (sync-caddr *sync-state*) (expr->word index)))
			 (result (sync-map-prove state key)))
		    (cons `((index . ,index) (key . ,key)
			    (value . ,(car result))
			    (proof . ,(cdr result)))
			  *sync-state*))))))

       (define (handle-step credential) 
	 (let ((seed (word->expr (sync-map-get (sync-cadr *sync-state*) (expr->word 'seed))))
	       (interval (word->expr (sync-map-get (sync-cadr *sync-state*) (expr->word 'proof-interval)))))
	   (if (not (equal? credential seed)) (error 'authentication-error "Invalid credentials")
	       (let ((state (sync-pair->byte-vector (sync-map-get (sync-caddr *sync-state*)
								  (expr->word current))))
		     (store (sync-map-set (sync-caddr *sync-state*)
					  (expr->word (- current interval))
					  (sync-null))))
		 (cons #t (sync-cons (sync-car *sync-state*)
				     (sync-cons (sync-cadr *sync-state*)
						(sync-cons store
							   (sync-chain-push (sync-cdddr *sync-state*)
									    state)))))))))

       (define (handle-evaluate credential command)
	 (let ((seed (word->expr (sync-map-get (sync-cadr *sync-state*) (expr->word 'seed)))))
	   (if (not (equal? credential seed)) (error 'authentication-error "Invalid credentials")
	       (cons (eval command) *sync-state*))))

       (case (car query)
	 ((block) (apply handle-block (cdr query)))
	 ((promise) (apply handle-promise (cdr query)))
	 ((proof) (apply handle-proof (cdr query)))
	 ((step) (apply handle-step (cdr query)))
	 ((evaluate) (apply handle-evaluate (cdr query)))
	 (else (error 'query-error "Invalid query")))))

  (define initial-state
    (begin (eval verifiable-structures)
	   (sync-cons (sync-map-new `((,(expr->word 'seed) . ,(expr->word seed))
				      (,(expr->word 'periodicity) . ,(expr->word periodicity))
				      (,(expr->word 'cryptography) . ,(expr->word cryptography))
				      (,(expr->word 'promise-interval) . ,(expr->word promise-interval))
				      (,(expr->word 'proof-interval) . ,(expr->word proof-interval))))
		      (sync-cons (sync-map-new) (sync-chain-new)))))
  
  (define transition-function
    (expression->byte-vector transition))

  (set! *sync-state*
	(sync-cons transition-function initial-state))

  "Installed notary interface")
