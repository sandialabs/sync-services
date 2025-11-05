(macro (secret blocking? window)
  "> blocking? (bool): if #t, then all network connections block until success or timeout
  > window (uint): number of indices (steps) to keep old state data or #f if keep forever
  < return (fnc): configured ledger setup function"

  `(lambda (record)
     "Extend the record interface to include ledger functionality. The
     ledger extension provides logic for version-controlling stateful.
     Optionally, ledgers be configured to delete older states while
     persisting explicitly 'pinned' exceptions. Finally, ledgers support
     peering, syncing, and reading historical state from other ledgers
     across transitive peer-to-peer connections.

     > record (fnc): library to access record commands
     > secret (str): the root secret used to generate cryptographic materials
     < return (str): success message"
     (define signature-sign
       '(lambda (message)
          "Accept a message and return an assertion containing the ledger's public key and signature"
          (let* ((config (cadr ((record 'get) '(ledger meta config))))
                 (public-key (cadr (assoc 'public-key config)))
                 (secret-key (cadr (assoc 'secret-key config)))
                 (signature (crypto-sign secret-key (expression->byte-vector message))))
            (append message `(((public-key ,public-key) (signature ,signature)))))))

     (define signature-verify
       '(lambda (message assertion)
          "Verify the signature of a message given the message and assertion containing a public key"
          (let* ((public-key (cadr (assoc 'public-key assertion)))
                 (signature (cadr (assoc 'signature assertion))))
            (crypto-verify public-key signature (expression->byte-vector message)))))

     (define call-peer
       `(lambda (name)
          (sync-call 
           `(*record*
             ,,,secret
             (lambda (record)
               (let* ((config (cadr ((record 'get) '(ledger meta peers ,name))))
                      (messenger (eval (cadr (assoc 'messenger config))))
                      (message (,',signature-sign '(ledger-synchronize)))
                      (result (messenger message)))
                 ((record 'deserialize!) '(ledger stage *peers* ,name) result))))
           ,,blocking?)))

     (define call-step
       `(lambda (record)
          (let* ((config (cadr ((record 'get) '(ledger meta config))))
                 (window (cadr (assoc 'window config)))
                 (index (cadr ((record 'get) '(ledger chain index)))))
            (let loop ((periodicity 0))
              (let ((period (expt 2 periodicity)))
                (if (or (< (- (+ index 1) period) 0) (not (= (modulo index period) 0))) #t
                    (begin ((record 'copy!) '(ledger chain)
                            (append '(ledger previous) (make-list periodicity 'rest) '(first)))
                           (loop (+ periodicity 1))))))
            (if window ((record 'set!) `(ledger states ,(- (+ index 1) window)) #f))
            ((record 'copy!) '(ledger stage) '(control scratch))
            ((record 'copy!) '(ledger stage *state*) `(ledger states ,(+ index 1)))
            ((record 'copy!) '(ledger previous) '(control scratch previous))
            ((record 'set!) '(control scratch index) (+ index 1))
            ((record 'infer!) '(control scratch))
            ((record 'prune!) '(control scratch) '(*state*) #t)
            ((record 'copy!) '(control scratch) '(ledger chain))
            (+ index 1))))

     (define step!
       `(lambda (record)
          "Synchronize with all active peers and increment the state"
          (let loop ((names (cadr ((record 'get) '(ledger meta peers)))))
            (if (null? names) 'done
                (begin (,call-peer (car names))
                       (loop (cdr names)))))
          (sync-call '(*record* ,,secret ,call-step) #t)))

     (define ledger-config-local
       '(lambda (record)
          (cadr ((record 'get) '(ledger meta config)))))

     (define ledger-config-remote
       '(lambda (record)
          (let ((config (cadr ((record 'get) '(ledger meta config)))))
            `(,(assoc 'public-key config)))))

     (define ledger-path
       `(lambda*
         (record index)
         (let* ((len (+ (cadr ((record 'get) '(ledger chain index))) 1))
                (index (cond ((not index) (- len 1))
                             ((not (integer? index))
                              (error 'invalid-index "Index must be an integer"))
                             ((and (>= index 0) (< index len)) index)
                             ((>= index 0) 
                              (error 'invalid-index "Index cannot exceed chain length"))
                             ((and (< index 0) (>= (+ len index) 0)) (+ len index))
                             ((< index 0)
                              (error 'invalid-index "Index cannot exceed chain length"))
                             (else (error 'logic-error "Unhandled case")))))
           (let loop-1 ((i (cadr ((record 'get) '(ledger chain index)))) (prev '()))
             (if (> i index)
                 (let loop-2 ((periodicity 0) (prev (append prev '(previous))))
                   (let ((period (expt 2 periodicity)))
                     (if (and (>= (- i (* period 2)) index)
                              (= (modulo i (* period 2)) 0)
                              (<= period i))
                         (loop-2 (+ periodicity 1) (append prev '(rest)))
                         (loop-1 (- i period) (append prev '(first))))))
                 (append '(ledger chain) prev))))))

     (define ledger-index
       `(lambda (record)
          "Return the current index (i.e., the lowest index that has not been finalized)

          > record (fnc): library to access record commands
          < return (uint): the current index"
          (let ((chain-path (,ledger-path record -1)))
            (cadr ((record 'get) (append chain-path '(index)))))))

     (define peer-prove
       `(lambda (record name chain-path remote-path)
          (let ((config ((record 'get) `(ledger meta peers ,name))))
            (if (eq? (car config) 'nothing) (error 'peer-error "Peer not found")
                (let ((messenger (eval (cadr (assoc 'messenger (cadr config)))))
                      (index (cadr ((record 'get) (append chain-path '(index)))))
                      (i-remote (cadr ((record 'get) (append chain-path `(*peers* ,name index))))))
                  (messenger (,signature-sign `(ledger-prove ,remote-path ,i-remote))))))))

     (define ledger-fetch
       `(lambda (record path index store)
          (let* ((chain-path (,ledger-path record index))
                 (index (cadr ((record 'get) (append chain-path '(index))))))
            (if (not (and (eq? (car path) '*peers*) (> (length path) 1)))
                (error 'path-error "Invalid query path")
                (let ((result (,peer-prove record (cadr path) chain-path (list-tail path 2)))
                      (path-peer (append chain-path `(*peers* ,(cadr path)))))
                  ((record 'deserialize!) '(control scratch fetch) result)
                  (if (not ((record 'equivalent?) path-peer '(control scratch fetch)))
                      (error 'integrity-error "Data does not verify"))
                  ((record 'copy!) '(control scratch fetch) store))))))

     (define ledger-pin!
       `(lambda (record path index)
          "Pin a historical state to prevent automatic deletion or remote state for caching

          > record (fnc): library to access record commands
          > path (list sym|vec): path to the data to pin
          > index (int): step number of the data to pin
          < return (bool): boolean indicating success of the operation"
          (let* ((chain-path (,ledger-path record index))
                 (index (cadr ((record 'get) (append chain-path '(index))))))
            ((record 'copy!) chain-path '(control scratch local))
            (cond ((and (> (length path) 1) (eq? (car path) '*state*))
                   ((record 'merge!) `(ledger states ,index) '(control scratch local *state*))
                   ((record 'slice!) '(control scratch local) path)
                   (if (eq? (car ((record 'get) `(ledger pinned ,index))) 'nothing)
                       ((record 'copy!) '(control scratch local) `(ledger pinned ,index))
                       ((record 'merge!) '(control scratch local) `(ledger pinned ,index))))
                  ((and (> (length path) 1) (eq? (car path) '*peer*))
                   (let ((store '(control scratch pin)))
                     (,ledger-fetch record path index store)
                     ((record 'merge!) '(control scratch pin) store)))
                  (else
                   (error 'path-error "Path must start with *state* or *peer* have length > 1"))))))

     (define ledger-unpin!
       `(lambda (record path index)
          "Unpin previously pinned data

          > record (fnc): library to access record commands
          > path (list sym|vec): path to the content to unpin
          > index (int): step number of the content to unpin
          < return (bool): boolean indicating success of the operation"
          (let* ((chain-path (,ledger-path record index))
                 (index (cadr ((record 'get) (append chain-path '(index))))))
            (cond ((and (> (length path) 1) (eq? (car path) '*state*))
                   ((record 'prune!) `(ledger pinned ,index) path))
                  ((and (> (length path) 1) (eq? (car path) '*peer*))
                   ((record 'prune!) `(ledger pinned ,index) path))
                  (else (error 'path-error "Path must start with *state* or *peer* and have length > 1"))))))

     (define ledger-operate
       `(lambda (operate path index)
          (if (and (not index) (not (null? path)) (eq? (car path) '*state*))
              (operate (append '(ledger stage) path))
              (let* ((chain-path (,ledger-path record index))
                     (index (cadr (operate (append chain-path '(index)))))
                     (pinned (operate (append `(ledger pinned ,index) path))))
                (if (or (eq? (car pinned) 'object) (and (eq? (car pinned) 'directory) (caddr pinned))) pinned
                    (cond ((null? path) '(directory (*state* *peers*) #t))
                          ((eq? (car pinned) 'object) pinned)
                          ((and (eq? (car path) '*peers*) (null? (cdr path)))
                           (operate (append chain-path '(*peers*))))
                          ((and (eq? (car path) '*state*))
                           (operate (append `(ledger states ,index) (cdr path))))
                          ((and (eq? (car path) '*peers*))
                           (let ((store '(control scratch get)))
                             (,ledger-fetch record path index store)
                             (operate (append store (list-tail path 2)))))
                          (else (error 'path-error "Path must start with *state* or *peer*"))))))))

     (define ledger-get
       `(lambda*
         (record path index)
          "Retrieve the data at the given path and index, potentially from other peers

          > record (fnc): library to access record commands
          > path (list sym|vec): path to the content to retrieve
          > index (int): step number of the content to retrieve
          < return (sym . (list exp)): list containing the type and value of the data
              - 'object type indicates a simple lisp-serializable value
              - 'structure type indicates a complex value represented by sync-pair?
              - 'directory type indicates an intermediate directory node
                - the second item is a list of known subpath segments
                - the third item is a bool indicating whether the directory is complete
                  (i.e., none of its underlying data has been pruned)
              - 'nothing type indicates that no data is found at the path
              - 'unknown type indicates the path has been pruned"
         (,ledger-operate (lambda (x) ((record 'get) x)) path index)))

     (define ledger-set!
       `(lambda (record path value)
          "Write the value to the path. Recursively generate parent
          directories if necessary.

          > record (fnc): library to access record commands
          > path (list sym|vec): path to the specified value
          > value (exp|sync-pair): data to be stored at the path
          < return (bool): boolean indicating success of the operation"
          (if (or (null? path) (not (eq? (car path) '*state*)))
              (error 'path-error "first path segment must be *state*")
              ((record 'set!) (append '(ledger stage) path) value))))

     (define ledger-copy!
       `(lambda*
         (record path target index)
          "Copy the value from the specified path. Recursively generate parent
          directories if necessary.

          > record (fnc): library to access record commands
          > path (list sym|vec): path to the source location
          > target (list sym|vec): path to the target location
          < return (bool): boolean indicating success of the operation"
         (if (or (null? target) (not (eq? (car target) '*state*)))
             (error 'path-error "First segment of target path must be *state*")
             (,ledger-operate
              (lambda (x) ((record 'copy!) x (append '(ledger stage) target))) path index))))

     (define ledger-peer!
       '(lambda (record name messenger)
          "Establish a persistent connection with another peer

          > record (fnc): library to access record commands
          > name (sym): unique symbol to refer to the peer
          > messenger (fnc): function to message (single arg) to send to the peer
          < return (bool): boolean indicating success of the operation"
          ((record 'set!) `(ledger stage *peers* ,name) #f)
          (if (not messenger) ((record 'set!) `(ledger meta peers ,name) #f)
              ((record 'set!) `(ledger meta peers ,name)
               `((messenger ,messenger)
                 ,(assoc 'public-key ((eval messenger) '(ledger-config))))))))

     (define ledger-peers
       '(lambda (record)
          "List all currently active peers

          > record (fnc): library to access record commands
          < return (list sym): list of peer names"
          (let ((names (cadr ((record 'get) '(ledger meta peers)))))
            (let loop ((names names) (result '()))
              (if (null? names) (reverse result)
                  (let ((info (cadr ((record 'get) `(ledger meta peers ,(car names))))))
                    (loop (cdr names) (cons (cons (car names) info) result))))))))

     (define ledger-prove
       `(lambda (record path index assertion)
          (if (not (,signature-verify `(ledger-prove ,path ,index) assertion))
              (error 'peer-error "Could not verify assertion")
              (let* ((chain-path (,ledger-path record index))
                     (index (cadr ((record 'get) (append chain-path '(index)))))
                     (pinned ((record 'get) (append `(ledger pinned ,index) path))))
                ((record 'copy!) chain-path '(control scratch local))
                ((record 'infer!) '(control scratch local))
                (if (or (eq? (car pinned) 'object) (and (eq? (car pinned) 'directory) (caddr pinned)))
                    ((record 'merge!) `(ledger pinned ,index) '(control scratch local))
                    (begin
                      ((record 'merge!) `(ledger states ,index) '(control scratch local *state*))
                           (if (and (> (length path) 1) (eq? (car path) '*peers*))
                               (let* ((path-remote (list-tail path 2))
                                      (result (,peer-prove record (cadr path) chain-path path-remote))
                                      (path-root `(control scratch local *peers* ,(cadr path))))
                                 ((record 'deserialize!) '(control scratch remote) result)
                                 (if (not ((record 'equivalent?) '(control scratch remote) path-root))
                                     (error 'peer-error "Ledger integrity error")
                                     ((record 'merge!) '(control scratch remote) path-root))))
                           ((record 'slice!) '(control scratch local) path)
                           (let* ((path-abs (append '(control scratch local) path))
                                  (value ((record 'get) path-abs)))
                             (if (eq? (car value) 'directory)
                                 (let loop ((names (cadr value)))
                                   (if (null? names) #t
                                       (begin
                                         ((record 'prune!) path-abs `(,(car names)) #t)
                                         (loop (cdr names)))))))))
                ((record 'serialize) '(control scratch local))))))

     (define ledger-synchronize
       `(lambda (record assertion)
          (if (not (,signature-verify `(ledger-synchronize) assertion))
              (error 'peer-error "Could not verify assertion")
              (let ((chain-path (,ledger-path record -1)))
                ((record 'copy!) chain-path '(control scratch))
                ((record 'infer!) '(control scratch))
                ((record 'slice!) '(control scratch) '(index))
                ((record 'serialize) '(control scratch))))))

     (define ledger-library
       `(lambda (record)
          (lambda (function)
            (case function
              ((get) (lambda args (apply ,ledger-get (cons record args))))
              ((set!) (lambda args (apply ,ledger-set! (cons record args))))
              ((copy!) (lambda args (apply ,ledger-copy! (cons record args))))
              ((index) (lambda args (apply ,ledger-index (cons record args))))
              ((peers) (lambda args (apply ,ledger-peers (cons record args))))
              (else (error 'missing-function "Function not found"))))))

     (let* ((key-pair (crypto-generate (expression->byte-vector ,secret)))
            (public-key (car key-pair))
            (secret-key (cdr key-pair)))
       ((record 'set!) '(ledger meta config)
        (list (list 'window ,window)
              (list 'public-key public-key)
              (list 'secret-key secret-key))))

     ((record 'set!) '(ledger chain index) 0) 
     ((record 'set!) '(control step 0) step!)
     ((record 'set!) '(control local ledger-config) ledger-config-local)
     ((record 'set!) '(control local ledger-get) ledger-get)
     ((record 'set!) '(control local ledger-set!) ledger-set!)
     ((record 'set!) '(control local ledger-copy!) ledger-copy!)
     ((record 'set!) '(control local ledger-pin!) ledger-pin!)
     ((record 'set!) '(control local ledger-unpin!) ledger-unpin!)
     ((record 'set!) '(control local ledger-index) ledger-index)
     ((record 'set!) '(control local ledger-peer!) ledger-peer!)
     ((record 'set!) '(control local ledger-peers) ledger-peers)
     ((record 'set!) '(control remote ledger-config) ledger-config-remote)
     ((record 'set!) '(control remote ledger-prove) ledger-prove)
     ((record 'set!) '(control remote ledger-synchronize) ledger-synchronize)
     ((record 'set!) '(record library ledger) ledger-library)

     "Installed ledger"))
