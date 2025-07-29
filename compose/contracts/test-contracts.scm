; User must:
; define global variables in a hash map called "vars" (hardcoded currently)
; when defining a function that uses a global variable, refer to it in the hashmap, like if the var is called foo, refer to it as (vars 'foo)
; no need to include global variables in function parameters 

; THESE ARE THE POSTER EXAMPLE ONES 

(*local* "password" 
    (contract-deploy (*state* contracts bill1) 
                     (begin (define vars (hash-table 'votes 0)) 
                            (define vote (lambda () (set! (vars 'votes) (+ 1 (vars 'votes))))) )))


(*local* "password" 
    (contract-call (*state* contracts bill1 code) 
                   (*state* contracts bill1 vars) 
                   #f 
                   (vote)))


; CURRENT TEST - VOTE FOR BILL1 AND BILL2 AT THE SAME TIME 

(*local* "password" (create-account "divya" "passwd" #f))

; just define bill2 
(*local* "password" 
    (contract-deploy "divya" "passwd" (*state* contracts bill2) 
                     (begin (define vars (hash-table 'votes 0 'voters (hash-table))) 
                            (define vote2 (lambda () 
                                    (if (eq? (vars 'voters username) #f) (begin (set! (vars 'voters username) #t) (set! (vars 'votes) (+ 1 (vars 'votes))) ) (error "you already voted"))
                                        )) 
                            ) ))

; define a function where u can vote for this and similar bills 

(*local* "password" 
    (contract-deploy "divya" "passwd" (*state* contracts bill1) 
                     (begin (define vars (hash-table 'votes 0 'voters (hash-table)))
                            (define vote (lambda () 
                                                (if (eq? (vars 'voters username) #f) (begin (set! (vars 'voters username) #t) (set! (vars 'votes) (+ 1 (vars 'votes))) ) (error "you already voted"))
                                                  ))
                            (define vote-similar (lambda ()
                                                        (cross-call '(*state* contracts bill2) #f '(vote2)))) 
                            ) ))



(*local* "password" 
    (contract-call "divya" "passwd"    
                   (*state* contracts bill1) 
                   #f 
                   (vote-similar)))

(*local* "password"  
    (contract-call "divya" "passwd" 
                   (*state* contracts bill2) 
                   #f 
                   (vote2)))

; OLD TEST 3 YOU CAN ONLY VOTE ONCE 

(*local* "password" (create-account "divya" "passwd" #f))

(*local* "password" 
    (contract-deploy (*state* contracts bill1 code) 
                     (begin (define vote (lambda () 
                                           
                                                (if (eq? (vars 'voters username) #f) (begin (set! (vars 'voters username) #t) (set! (vars 'votes) (+ 1 (vars 'votes))) ) (error "you already voted"))
                                                  )) 
                            ) 
                     (*state* contracts bill1 vars) 
                     (define vars (hash-table 'votes 0 'voters (hash-table)))))


(*local* "password" 
    (contract-call "divya" "passwd" 
                   (*state* contracts bill1 code) 
                   (*state* contracts bill1 vars) 
                   #f 
                   (vote)))


; SINGLE PARAMETER

(*local* "password" 
    (contract-dep-debug (*state* contracts bill1 code) 
                     (begin (define vars (hash-table 'votes 0)) 
                     (define vote (lambda (user pass) (if (eq? (contract-auth user pass) #t) (set! (vars 'votes) (+ 1 (vars 'votes))) (display "authentication error")))) 
                            ) ))

                     (begin 
                     (define vote (lambda (user pass) (if (eq? (contract-auth user pass) #t) (set! (vars 'votes) (+ 1 (vars 'votes))) (display "authentication error")))))


(*local* "password" 
    (contract-call (*state* contracts bill1 code) 
                   (*state* contracts bill1 vars) 
                   #f 
                   (vote "divya" "password")))

; SINGLE PARAMETER
(*local* "password" 
    (contract-dep-debug "divya" "passwd" (*state* contracts test code) 
                     (begin (define vars (hash-table 'votes 0 'foovar 1)) 
                        (define vote (lambda () (set! (vars 'votes) (+ 1 (vars 'votes))))) 
                            (define foo (lambda () 
                                (begin (set! (vars 'votes) (+ 1 (vars 'votes)))
                                       (set! (vars 'foovar) (* 2 (vars 'foovar))))))) ))

                     


(*local* "password" 
    (contract-call "divya" "passwd"
                        (*state* contracts test)  
                           #f 
                           (foo)))

; OTHER STUFF                         

(define (outer-function x) 
  (let ((inner-function
         (lambda (y)
           (+ x y)))) ; Define inner-function using lambda
    (inner-function 10))) ; Call inner-function with an argument

;; Example usage
(display (outer-function 5)) ; This will display 15 (5 + 10)