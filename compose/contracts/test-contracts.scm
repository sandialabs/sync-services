; User must:
; define global variables in a hash map called "vars" (hardcoded currently)
; when defining a function that uses a global variable, refer to it in the hashmap, like if the var is called foo, refer to it as (vars 'foo)
; no need to include global variables in function parameters 

; THESE ARE THE POSTER EXAMPLE ONES 

(*local* "password" 
    (contract-deploy (*state* contracts bill1 code) 
                     (begin (define vote (lambda () (set! (vars 'votes) (+ 1 (vars 'votes))))) ) 
                     (*state* contracts bill1 vars) 
                     (define vars (hash-table 'votes 0))))


(*local* "password" 
    (contract-call (*state* contracts bill1 code) 
                   (*state* contracts bill1 vars) 
                   #f 
                   (vote)))


; CURRENT TEST 

(*local* "password" 
    (contract-deploy (*state* contracts bill1 code) 
                     (begin (define contract-auth
                            (lambda (username password index)
                            (let ((ledger ((eval (cadr ((record 'get) '(record library ledger)))) record)))
                                    (let ((table (car ((ledger 'get) `(*state* data accounts) index))))
                                        (if (eq? table 'nothing)
                                            ; (display table)
                                            (display "account does not exist")
                                            (let ((table ((ledger 'get) `(*state* data accounts) index))) 
                                                    (begin (eval (cadr table))
                                                    ; (format #f "table: ~a current pass: ~a" accounts (accounts username))
                                                    (if (string=? (accounts username) password)
                                                        #t
                                                        (error #f "~a does not equal ~a" (accounts username) password))
                                        
                                                        )))))))
                            (define vote (lambda (user pass) (if (eq? (contract-auth user pass #f) #t) (set! (vars 'votes) (+ 1 (vars 'votes))) (display "authentication error")))) 
                            ) 
                     (*state* contracts bill1 vars) 
                     (define vars (hash-table 'votes 0))))

(*local* "password" 
    (contract-deploy (*state* contracts bill1 code) 
                     (begin (define vote (lambda (user pass) 
                                            (if (and (eq? (contract-auth user pass) #t) (eq? (vars 'voters user) #f)) 
                                                (begin (set! (vars 'voters user) #t) (set! (vars 'votes) (+ 1 (vars 'votes))) )
                                                (display "authentication error")))) 
                            ) 
                     (*state* contracts bill1 vars) 
                     (define vars (hash-table 'votes 0 'voters (hash-table)))))

; OLD TEST 3 YOU CAN ONLY VOTE ONCE 

(*local* "password" (create-account "divya" "passwd" #f))

(*local* "password" 
    (contract-deploy (*state* contracts bill1 code) 
                     (begin (define vote (lambda (user) 
                                           
                                                (if (eq? (vars 'voters user) #f) (begin (set! (vars 'voters user) #t) (set! (vars 'votes) (+ 1 (vars 'votes))) ) (error "you already voted"))
                                                  )) 
                            ) 
                     (*state* contracts bill1 vars) 
                     (define vars (hash-table 'votes 0 'voters (hash-table)))))


(*local* "password" 
    (contract-call "divya" "passwd" 
                   (*state* contracts bill1 code) 
                   (*state* contracts bill1 vars) 
                   #f 
                   (vote "divya")))


; OLD TEST 2 - HAS AUTH BUT NOT STORAGE 

(*local* "password" 
    (contract-deploy (*state* contracts bill1 code) 
                     (begin (define vote (lambda (user pass) (if (eq? (contract-auth user pass) #t) (set! (vars 'votes) (+ 1 (vars 'votes))) (display "authentication error")))) 
                            ) 
                     (*state* contracts bill1 vars) 
                     (define vars (hash-table 'votes 0))))


(*local* "password" 
    (contract-call (*state* contracts bill1 code) 
                   (*state* contracts bill1 vars) 
                   #f 
                   (vote "divya" "password")))

; OLD TEST 1

(*local* "password" 
    (contract-deploy (*state* contracts test code) 
                     (begin (define vote (lambda () (set! (vars 'votes) (+ 1 (vars 'votes))))) 
                            (define foo (lambda () 
                                (begin (set! (vars 'votes) (+ 1 (vars 'votes)))
                                       (set! (vars 'foovar) (* 2 (vars 'foovar))))))) 
                     (*state* contracts test vars) 
                     (define vars (hash-table 'votes 0 'foovar 1))))


(*local* "password" 
    (contract-call "divya" "passwd"
                        (*state* contracts test code) 
                           (*state* contracts test vars) 
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