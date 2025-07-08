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
                     (begin (define vote (lambda (user pass) (if (eq? (,contract-auth user pass) #t) (set! (vars 'votes) (+ 1 (vars 'votes))) (display "authentication error")))) ) 
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
    (contract-call (*state* contracts test code) 
                           (*state* contracts test vars) 
                           #f 
                           (foo)))

                           