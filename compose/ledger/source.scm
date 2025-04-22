(lambda (seed periodicity cryptography notary)

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
	 (sync-cons (expr->word 0)
		    (sync-cons (if (null? args) (sync-null) (car args)) (sync-null))))

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

       (define (crypto-call . args)
	 (let* ((service (sync-map-get (sync-caadr *sync-state*) (expr->word 'cryptography)))
		(path (apply append (let loop ((ls args))
				      (if (null? ls) '()
					  (cons "/" (cons (car ls) (loop (cdr ls)))))))))
	   (str->expr (sync-http 'GET (append (word->expr service) "/signature" path)))))

       (define (notary-call message)
	 (let ((notary (sync-map-get (sync-caadr *sync-state*) (expr->word 'notary))))
	   (str->expr (sync-http 'POST (word->expr notary) (expr->str message)))))

       (define (peer-call peer message)
	 (str->expr (sync-http 'POST peer (expr->str message))))

       (define (authenticate credential)
	 (if (not (equal? credential
			  (word->expr (sync-map-get (sync-caadr *sync-state*) (expr->word 'password)))))
	     (error 'authentication-error "Invalid credentials")))

       (define* (handle-info credential)
	 (authenticate credential)
	 (let* ((words (sync-map-all (sync-caadr *sync-state*)))
		(info (map (lambda (x) (cons (word->expr (car x)) (word->expr (cdr x)))) words)))
	   (cons info *sync-state*)))

       (define (read-local key)
	 (let ((promise-state (sync-chain-data (sync-cdaddr *sync-state*))))
	   (word->expr (sync-map-get promise-state (expr->word key)))))
       
       (define (read-remote key peer public-key)
	 (let ((response (peer-call peer `(claim ,key))))
	   (if (eq? (car response) 'error) (error 'ledger-exception (expr->str response))
	       (let* ((index (cdr (assoc 'index response)))
		      (content (cdr (assoc 'content response)))
		      (signature (cdr (assoc 'signature response)))
		      (local-proof (cdr (assoc 'local-proof response)))
		      (global-proof (cdr (assoc 'global-proof response)))
		      (block-state (cdr (assoc 'state (notary-call `(block 0 0 ,index)))))
		      (local-root (sync-map-infer (expr->word key) (expr->word content) local-proof))
		      (global-key (sync-hash signature))
		      (global-value local-root)
		      (global-root (sync-map-infer global-key global-value global-proof))
		      (sig-hex (byte-vector->hex-string signature))
		      (index-hex (byte-vector->hex-string (expr->word index)))
		      (verify-signature (eq? #t (crypto-call "verify" public-key sig-hex index-hex)))
		      (verify-structure (equal? global-root block-state)))
		 (if (and verify-structure verify-signature) content
		     (error 'verification-error "Peer response is invalid"))))))

       (define* (handle-read key peer public-key)
	 (cons (if peer (read-remote key peer public-key) (read-local key)) *sync-state*))

       (define (handle-write credential key value)
	 (authenticate credential)
	 (let ((state-new (sync-map-set (sync-caaddr *sync-state*) (expr->word key) (expr->word value))))
	   (cons #t (sync-cons (sync-car *sync-state*)
			       (sync-cons (sync-cadr *sync-state*)
					  (sync-cons (sync-cons state-new
								(sync-cdaddr *sync-state*))
						     (sync-cdddr *sync-state*)))))))

       (define (handle-claim key)
	 (let* ((proof-chain (sync-cddddr *sync-state*))
	  	(local-index (- (sync-chain-length proof-chain) 1)))
	   (if (< local-index 0) (error 'patience-exception "Ledger does not have any proofs yet")
	       (let* ((state-chain (sync-chain-sub (sync-cdaddr *sync-state*) local-index))
		      (promise-chain (sync-chain-sub (sync-cadddr *sync-state*) local-index))
		      (pk (word->expr (sync-map-get (sync-caadr *sync-state*) (expr->word 'public-key))))
	  	      (global-proof (cdr (assoc 'proof (word->expr (sync-chain-data proof-chain)))))
		      (signature (cdr (assoc 'signature (word->expr (sync-chain-data promise-chain)))))
		      (index (cdr (assoc 'index (word->expr (sync-chain-data promise-chain)))))
        	      (result (sync-map-prove (sync-chain-data state-chain) (expr->word key))))
		 (if (sync-null? (car result)) (error 'ledger-error "No such value")
		     (cons `((index . ,index)
			     (public-key . ,(hex-string->byte-vector pk))
			     (content . ,(word->expr (car result)))
			     (signature . ,signature)
			     (global-proof . ,global-proof)
			     (local-proof . ,(cdr result)))
			   *sync-state*))))))

       (define (handle-tick credential)
	 (authenticate credential)
	 (let ((chain (sync-chain-push (sync-cdaddr *sync-state*) (sync-caaddr *sync-state*))))
	   (cons #t (sync-cons (sync-car *sync-state*)
			       (sync-cons (sync-cadr *sync-state*)
					  (sync-cons (sync-cons (sync-caaddr *sync-state*) chain)
						     (sync-cdddr *sync-state*)))))))

       (define (handle-promise credential)
	 (authenticate credential)
	 (let* ((periodicity (word->expr (sync-map-get (sync-caadr *sync-state*) (expr->word 'periodicity))))
		(secret (word->expr (sync-map-get (sync-caadr *sync-state*) (expr->word 'secret-key))))
		(value (sync-chain-data (sync-cdaddr *sync-state*))))
	   (let loop ((offset 0))
	     (let* ((index (cdr (assoc 'index (notary-call `(block ,periodicity ,offset)))))
		    (message (byte-vector->hex-string (expr->word index)))
		    (signature (hex-string->byte-vector (crypto-call "sign" secret message)))
        	    (promise (notary-call `(promise ,(sync-hash signature) ,(sync-pair->byte-vector value) ,periodicity ,offset)))
		    (promise-chain (sync-cadddr *sync-state*)))
	       (if (eq? (car promise) 'error) (loop (+ offset 1))
		   (let* ((promise (expr->word (cons (cons 'signature signature) promise)))
			  (promise-chain (sync-chain-push (sync-cadddr *sync-state*) promise)))
		     (cons #t
			   (sync-cons (sync-car *sync-state*)
				      (sync-cons (sync-cadr *sync-state*)
						 (sync-cons (sync-caddr *sync-state*)
							    (sync-cons promise-chain
								       (sync-cddddr *sync-state*))))))))))))

       (define (handle-proof credential)
	 (authenticate credential)
	 (let* ((periodicity (word->expr (sync-map-get (sync-caadr *sync-state*) (expr->word 'periodicity))))
		(secret (word->expr (sync-map-get (sync-caadr *sync-state*) (expr->word 'secret-key))))
		(value (sync-chain-data (sync-cdaddr *sync-state*)))
		(promise-chain (sync-cadddr *sync-state*))
		(promise-length (sync-chain-length promise-chain)))
	   (let loop ((proof-chain (sync-cddddr *sync-state*)))
	     (let* ((proof-length (sync-chain-length proof-chain))
		    (promise-sub (sync-chain-sub promise-chain proof-length))
		    (promise (word->expr (sync-chain-data promise-sub)))
		    (notary-index (cdr (assoc 'index promise)))
		    (notary-key (cdr (assoc 'key promise)))
		    (proof (notary-call `(proof ,notary-index ,notary-key))))
	       (if (eq? (car proof) 'error)
		   (cond ((equal? (cadr proof) ''past-index-error)
			  (loop (sync-chain-push proof-chain (sync-null))))
			 ((equal? (cadr proof) ''future-index-error)
			  (cons #t (sync-cons (sync-car *sync-state*)
					      (sync-cons (sync-cadr *sync-state*)
							 (sync-cons (sync-caddr *sync-state*)
								    (sync-cons promise-chain
									       proof-chain))))))
			 (else (error 'remote-error (expr->str proof))))
		   (loop (sync-chain-push proof-chain (expr->word proof))))))))

       (define (handle-step credential)
	 (let ((call-or-err (lambda (x)
			      (let ((result (sync-call x (make-byte-vector 32 0) )))
				(if (eq? #t result) #t (error (car x) (expr->str result)))))))
	   (call-or-err `(*tick* ,credential))
	   (call-or-err `(*promise* ,credential))
	   (call-or-err `(*proof* ,credential))
	   (cons #t *sync-state*)))

       (define (evaluate query)
	 (cons (eval query) *sync-state*))
       
       (case (car query)
	 ((evaluate) (apply evaluate (cdr query)))
	 ((info) (apply handle-info (cdr query)))
	 ((read) (apply handle-read (cdr query)))
	 ((write) (apply handle-write (cdr query)))
	 ((claim) (apply handle-claim (cdr query)))
	 ((step) (apply handle-step (cdr query)))
	 ((*tick*) (apply handle-tick (cdr query)))
	 ((*promise*) (apply handle-promise (cdr query)))
	 ((*proof*) (apply handle-proof (cdr query)))
	 (else (error 'query-error "Invalid query")))))

  (eval verifiable-structures)

  (set! *sync-state*  ; todo: loop until service is up
	(let* ((keys (str->expr (sync-http 'GET (append cryptography "/signature/key/" seed))))
	       (info `((,(expr->word 'password) . ,(expr->word seed))
		       (,(expr->word 'periodicity) . ,(expr->word periodicity))
		       (,(expr->word 'secret-key) . ,(expr->word (cadar keys)))
		       (,(expr->word 'public-key) . ,(expr->word (cadadr keys)))
		       (,(expr->word 'notary) . ,(expr->word notary))
		       (,(expr->word 'cryptography) . ,(expr->word cryptography)))))
	  (sync-cons (expression->byte-vector transition)
		     (sync-cons (sync-cons (sync-map-new info)
					   (sync-map-new))
				(sync-cons (sync-cons (sync-map-new)
						      (sync-chain-new))
					   (sync-cons (sync-chain-new)
						      (sync-chain-new)))))))

  "Installed ledger interface")
