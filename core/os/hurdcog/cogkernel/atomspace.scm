(define-module (cogkernel atomspace)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-atom
            make-link
            atom?
            link?
            atom-type
            atom-name
            atom-value
            atom-confidence
            link-type
            link-outgoing
            atomspace-add!
            atomspace-get
            atomspace-query
            atomspace-get-atoms
            atomspace-tensor-shape
            make-atomspace
            atomspace?
            *global-atomspace*))
(define atom-types
  '(CONCEPT
    PREDICATE
    VARIABLE
    NUMBER
    STRING
    ISSUE
    CAPABILITY
    BUILD
    TASK
    AGENT
    RULE
    SCRIPT
    EXPERIENCE
    PATTERN
    STATE-VALUE
    POLICY
    META-PATTERN
    DEPLOYMENT
    NODE))
(define link-types
  '(INHERITANCE
    SIMILARITY
    EVALUATION
    EXECUTION
    IMPLICATION
    AND
    OR
    NOT
    LIST
    SET))
(define-record-type <atom>
  (make-atom-record type name value tensor-coords confidence)
  atom?
  (type atom-type)
  (name atom-name)
  (value atom-value)
  (tensor-coords atom-tensor-coords)
  (confidence atom-confidence))
(define-record-type <link>
  (make-link-record type outgoing tensor-coords confidence)
  link?
  (type link-type)
  (outgoing link-outgoing)
  (tensor-coords link-tensor-coords)
  (confidence link-confidence))
(define-record-type <atomspace>
  (make-atomspace-record atoms links tensor-shape)
  atomspace?
  (atoms atomspace-atoms)
  (links atomspace-links)
  (tensor-shape atomspace-tensor-shape))
(define* (make-atom type name #:optional (value #f) (confidence 1.0))
  "Create a new atom with specified type, name, and optional value"
  (unless (member type atom-types)
    (error "Invalid atom type:" type))
  (make-atom-record type name value '(0 0 0 0) confidence))
(define* (make-link type outgoing #:optional (confidence 1.0))
  "Create a new link connecting atoms"
  (unless (member type link-types)
    (error "Invalid link type:" type))
  (make-link-record type outgoing '(0 0 0 0) confidence))
(define* (make-atomspace #:optional (initial-tensor-shape '(100 100 50 10)))
  "Create a new atomspace with specified tensor dimensions [n_atoms x n_links x n_features x n_contexts]"
  (make-atomspace-record (make-hash-table) (make-hash-table) initial-tensor-shape))
(define *global-atomspace* (make-atomspace))
(define (atomspace-add! atomspace item)
  "Add an atom or link to the atomspace"
  (cond
    ((atom? item)
     (hash-set! (atomspace-atoms atomspace) (atom-name item) item))
    ((link? item)
     (let ((link-id (string-append (symbol->string (link-type item))
                                   "-" 
                                   (number->string (object-address item)))))
       (hash-set! (atomspace-links atomspace) link-id item)))
    (else
     (error "Invalid item type for atomspace"))))
(define (atomspace-get atomspace name)
  "Retrieve an atom by name from the atomspace"
  (hash-ref (atomspace-atoms atomspace) name))
(define (atomspace-query atomspace predicate)
  "Query atomspace for atoms matching the given predicate"
  (filter predicate 
          (hash-map->list (lambda (k v) v) (atomspace-atoms atomspace))))
(define (atomspace-get-atoms atomspace)
  "Get all atoms from the atomspace"
  (hash-map->list (lambda (k v) v) (atomspace-atoms atomspace)))
(define (atomspace-tensor-shape atomspace)
  "Get the tensor shape of the atomspace [n_atoms x n_links x n_features x n_contexts]"
  (let ((n-atoms (hash-count (const #t) (atomspace-atoms atomspace)))
        (n-links (hash-count (const #t) (atomspace-links atomspace)))
        (base-shape (atomspace-tensor-shape atomspace)))
    (list n-atoms n-links (third base-shape) (fourth base-shape))))
(define (initialize-hurd-atoms! atomspace)
  "Initialize basic atoms for GNU Hurd cognitive kernel"
  (let ((hurd-concept (make-atom 'CONCEPT "GNU-Hurd"))
        (microkernel-concept (make-atom 'CONCEPT "Microkernel"))
        (translator-concept (make-atom 'CONCEPT "Translator"))
        (server-concept (make-atom 'CONCEPT "Server")))
    (atomspace-add! atomspace hurd-concept)
    (atomspace-add! atomspace microkernel-concept)
    (atomspace-add! atomspace translator-concept)
    (atomspace-add! atomspace server-concept)
    (let ((inheritance-link (make-link 'INHERITANCE 
                                       (list translator-concept hurd-concept)))
          (server-link (make-link 'INHERITANCE 
                                  (list server-concept hurd-concept))))
      (atomspace-add! atomspace inheritance-link)
      (atomspace-add! atomspace server-link))))