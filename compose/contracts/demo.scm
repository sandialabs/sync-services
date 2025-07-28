; make acct
(*local* "password" (create-account "divya" "passwd" #f))

; define bill2 
(*local* "password" 
    (contract-deploy "divya" "passwd" (*state* contracts bill2 code) 
                     (begin (define vote2 (lambda () 
                                           
                                                (if (eq? (vars 'voters username) #f) (begin (set! (vars 'voters username) #t) (set! (vars 'votes) (+ 1 (vars 'votes))) ) (error "you already voted"))
                                                  )) 
                            ) 
                     (*state* contracts bill2 vars) 
                     (define vars (hash-table 'votes 0 'voters (hash-table)))))

; define cross call fxn

(*local* "password" 
    (contract-deploy "divya" "passwd" (*state* contracts bill1 code) 
                     (begin (define vote (lambda () 
                                           
                                                (if (eq? (vars 'voters username) #f) (begin (set! (vars 'voters username) #t) (set! (vars 'votes) (+ 1 (vars 'votes))) ) (error "you already voted"))
                                                  ))
                            (define vote-similar (lambda ()
                                                        (cross-call username password '(*state* contracts bill2 code) '(*state* contracts bill2 vars) #f '(vote2)))) 
                            ) 
                     (*state* contracts bill1 vars) 
                     (define vars (hash-table 'votes 0 'voters (hash-table)))))

; run out of tokens
(*local* "password" 
    (contract-deploy "divya" "passwd" (*state* contracts test1 code) 
                     (begin (define vote (lambda () (set! (vars 'votes) (+ 1 (vars 'votes))))) 
                            (define foo (lambda () 
                                (begin (set! (vars 'votes) (+ 1 (vars 'votes)))
                                       (set! (vars 'foovar) (* 2 (vars 'foovar))))))) 
                     (*state* contracts test1 vars) 
                     (define vars (hash-table 'votes 0 'foovar 1))))

(*local* "password" (ledger-get (*state* contracts test1 code)))

; while waiting for tokens

(*local* "password" 
    (contract-call "divya1" "passwd"    
                   (*state* contracts bill1 code) 
                   (*state* contracts bill1 vars) 
                   #f 
                   (vote-similar)))

(*local* "password"  
    (contract-call "divya" "passwd" 
                   (*state* contracts bill2 code) 
                   (*state* contracts bill2 vars) 
                   #f 
                   (vote2)))

(*local* "password"  
    (contract-call "divya2" "passwd" 
                   (*state* contracts bill2 code) 
                   (*state* contracts bill2 vars) 
                   #f 
                   (vote2)))  ; acct not exist

(*local* "password"  
    (contract-call "divya" "passwd1" 
                   (*state* contracts bill2 code) 
                   (*state* contracts bill2 vars) 
                   #f 
                   (vote2))) ; wrong pass

; token replenishment
(*local* "password" 
    (contract-deploy "divya" "passwd" (*state* contracts test code) 
                     (begin (define vote (lambda () (set! (vars 'votes) (+ 1 (vars 'votes))))) 
                            (define foo (lambda () 
                                (begin (set! (vars 'votes) (+ 1 (vars 'votes)))
                                       (set! (vars 'foovar) (* 2 (vars 'foovar))))))) 
                     (*state* contracts test vars) 
                     (define vars (hash-table 'votes 0 'foovar 1)) ))


(*local* "password" 
    (contract-call "divya" "passwd"
                        (*state* contracts test code) 
                           (*state* contracts test vars) 
                           #f 
                           (foo)))

(*local* "password" (ledger-get (*state* contracts test code)))


