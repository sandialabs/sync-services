; make acct
(*local* "password" (create-account "divya" "passwd" #f))

; define bill2 
(*local* "password" 
    (contract-deploy "divya" "passwd" (*state* contracts bill2) 
                     (begin (define vars (hash-table 'votes 0 'voters (hash-table))) (define vote2 (lambda () 
                                           
                                                (if (eq? (vars 'voters username) #f) (begin (set! (vars 'voters username) #t) (set! (vars 'votes) (+ 1 (vars 'votes))) ) (error "you already voted"))
                                                  )) 
                            ) 
                     ))

; define cross call fxn

(*local* "password" 
    (contract-deploy "divya" "passwd" (*state* contracts bill1) 
                     (begin (define vars (hash-table 'votes 0 'voters (hash-table))) (define vote (lambda () 
                                           
                                                (if (eq? (vars 'voters username) #f) (begin (set! (vars 'voters username) #t) (set! (vars 'votes) (+ 1 (vars 'votes))) ) (error "you already voted"))
                                                  ))
                            (define vote-similar (lambda ()
                                                        (cross-call '(*state* contracts bill2) #f '(vote2)))) 
                            ) 
                     ))

; run out of tokens
(*local* "password" 
    (contract-deploy "divya" "passwd" (*state* contracts test1) 
                     (begin (define vars (hash-table 'votes 0 'foovar 1)) (define vote (lambda () (set! (vars 'votes) (+ 1 (vars 'votes))))) 
                            (define foo (lambda () 
                                (begin (set! (vars 'votes) (+ 1 (vars 'votes)))
                                       (set! (vars 'foovar) (* 2 (vars 'foovar))))))) ))

(*local* "password" (ledger-get (*state* contracts test1 code)))

(*local* "password" (ledger-get (*state* contracts test1 vars)))

; while waiting for tokens

(*local* "password" 
    (contract-call "divya1" "passwd"    
                   (*state* contracts bill1) 
                   #f 
                   (vote-similar)))

(*local* "password"  
    (contract-call "divya" "passwd" 
                   (*state* contracts bill2) 
                   #f 
                   (vote2)))

(*local* "password"  
    (contract-call "divya2" "passwd" 
                   (*state* contracts bill2)
                   #f 
                   (vote2)))  ; acct not exist

(*local* "password"  
    (contract-call "divya" "passwd1" 
                   (*state* contracts bill2)
                   #f 
                   (vote2))) ; wrong pass

; token replenishment
(*local* "password" 
    (contract-deploy "divya" "passwd" (*state* contracts test) 
                     (begin (define vars (hash-table 'votes 0 'foovar 1)) (define vote (lambda () (set! (vars 'votes) (+ 1 (vars 'votes))))) 
                            (define foo (lambda () 
                                (begin (set! (vars 'votes) (+ 1 (vars 'votes)))
                                       (set! (vars 'foovar) (* 2 (vars 'foovar))))))) 
                      ))


(*local* "password" 
    (contract-call "divya" "passwd"
                        (*state* contracts test)
                           #f 
                           (foo)))

(*local* "password" (ledger-get (*state* contracts test code)))


