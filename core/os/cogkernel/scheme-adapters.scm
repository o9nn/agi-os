(define-module (cogkernel scheme-adapters)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format) 
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (cogkernel atomspace)
  #:use-module (cogkernel cognitive-primitives)
  #:export (make-translation-adapter
            adapter-translate-to-atomspace
            adapter-translate-from-atomspace
            register-gnumach-primitive
            create-cognitive-grammar
            validate-translation-integrity
            *gnumach-primitive-registry*
            *cognitive-grammar-rules*
            run-translation-microservice
            test-adapter-round-trip))
(define-record-type <translation-adapter>
  (make-translation-adapter-record name from-type to-type translator validator metadata)
  translation-adapter?
  (name adapter-name)
  (from-type adapter-from-type)
  (to-type adapter-to-type)
  (translator adapter-translator)
  (validator adapter-validator)
  (metadata adapter-metadata set-adapter-metadata!))
(define *gnumach-primitive-registry* (make-hash-table))
(define *cognitive-grammar-rules* '())
(define (register-gnumach-primitive name properties grammar-rule)
  "Register a GNUMach primitive with its cognitive translation pattern"
  (hash-set! *gnumach-primitive-registry* name
             `((properties . ,properties)
               (grammar-rule . ,grammar-rule)
               (timestamp . ,(get-timestamp)))))
(define (make-translation-adapter name from-type to-type translator validator)
  "Create a bidirectional translation adapter"
  (make-translation-adapter-record name from-type to-type translator validator
                                   `((created . ,(get-timestamp))
                                     (translations . 0)
                                     (errors . 0))))
(define (adapter-translate-to-atomspace adapter primitive)
  "Translate a GNUMach primitive to AtomSpace representation using adapter"
  (match primitive
    ((primitive-name properties)
     (let* ((registry-entry (hash-ref *gnumach-primitive-registry* primitive-name))
            (translator (adapter-translator adapter)))
       (if registry-entry
           (let* ((grammar-rule (assoc-ref registry-entry 'grammar-rule))
                  (cognitive-fragment (apply translator (list primitive-name properties grammar-rule)))
                  (atoms (cognitive-fragment-atoms cognitive-fragment)))
             (let ((metadata (adapter-metadata adapter)))
               (set-adapter-metadata! adapter 
                                      (assoc-set! metadata 'translations 
                                                  (+ 1 (assoc-ref metadata 'translations)))))
             `((success . #t)
               (atoms . ,atoms)
               (fragment . ,cognitive-fragment)
               (grammar-rule . ,grammar-rule)))
           `((success . #f)
             (error . "Primitive not registered")
             (primitive . ,primitive-name)))))
    (_ `((success . #f)
         (error . "Invalid primitive format")
         (primitive . ,primitive)))))
(define (adapter-translate-from-atomspace adapter atoms)
  "Translate AtomSpace atoms back to GNUMach primitive using adapter"
  (let ((translator (adapter-translator adapter)))
    (catch 'translation-error
      (lambda ()
        (let* ((primitive-atoms (filter atom? atoms))
               (link-atoms (filter link? atoms))
               (primitive-type (and (not (null? primitive-atoms))
                                    (atom-name (car primitive-atoms))))
               (decoded-primitive (translator atoms)))
          `((success . #t)
            (primitive . ,decoded-primitive)
            (atom-count . ,(length atoms)))))
      (lambda (key . args)
        `((success . #f)
          (error . ,(format #f "Translation error: ~a" args))
          (atoms . ,atoms))))))
(define (create-cognitive-grammar)
  "Create cognitive grammar rules for GNUMach-AtomSpace translation"
  (set! *cognitive-grammar-rules*
    (list
      (list 'PORT_ALLOCATE 
       (cons 'pattern (lambda (name props rule)
                    (encode-gnumach-primitive name props)))
       (cons 'inverse (lambda (atoms)
                    (decode-atomspace-to-primitive atoms 'PORT_ALLOCATE))))
      (list 'VM_ALLOCATE
       (cons 'pattern (lambda (name props rule)
                    (encode-gnumach-primitive name props)))
       (cons 'inverse (lambda (atoms)
                    (decode-atomspace-to-primitive atoms 'VM_ALLOCATE))))
      (list 'FILE_OPEN
       (cons 'pattern (lambda (name props rule)
                    (encode-gnumach-primitive name props)))
       (cons 'inverse (lambda (atoms)
                    (decode-atomspace-to-primitive atoms 'FILE_OPEN))))
      (list 'THREAD_CREATE
       (cons 'pattern (lambda (name props rule)
                    (encode-gnumach-primitive name props)))
       (cons 'inverse (lambda (atoms)
                    (decode-atomspace-to-primitive atoms 'THREAD_CREATE))))
      (list 'NETWORK_SEND
       (cons 'pattern (lambda (name props rule)
                    (encode-gnumach-primitive name props)))
       (cons 'inverse (lambda (atoms)
                    (decode-atomspace-to-primitive atoms 'NETWORK_SEND))))
      (list 'SIGNAL_POST
       (cons 'pattern (lambda (name props rule)
                    (encode-gnumach-primitive name props)))
       (cons 'inverse (lambda (atoms)
                    (decode-atomspace-to-primitive atoms 'SIGNAL_POST)))))))
(define (decode-atomspace-to-primitive atoms primitive-type)
  "Decode AtomSpace atoms back to GNUMach primitive"
  (let* ((concept-atoms (filter (lambda (atom) 
                                  (and (atom? atom)
                                       (eq? (atom-type atom) 'CONCEPT))) atoms))
         (modality-atom (find (lambda (atom)
                                (member (atom-name atom) 
                                        (map car *cognitive-modalities*))) concept-atoms))
         (context-atom (find (lambda (atom)
                               (member (atom-name atom)
                                       (map car *cognitive-contexts*))) concept-atoms)))
    (if (and modality-atom context-atom)
        `(,primitive-type
          ,(atom-name modality-atom)
          2
          ,(atom-name context-atom) 
          7
          2)
        (throw 'translation-error "Cannot decode atoms to primitive"))))
(define (validate-translation-integrity original translated)
  "Validate that translation preserves semantic integrity"
  (match (list original translated)
    (((original-name . original-props) (translated-name . translated-props))
     (let ((name-match? (eq? original-name translated-name))
           (modality-match? (eq? (car original-props) (cadr translated-props))))
       `((valid . ,(and name-match? modality-match?))
         (name-match . ,name-match?)
         (modality-match . ,modality-match?)
         (confidence . ,(if (and name-match? modality-match?) 1.0 0.5)))))
    (_ `((valid . #f)
         (error . "Invalid format for validation")))))
(define (run-translation-microservice primitives)
  "Run the translation microservice on a list of primitives"
  (format #t "=== Scheme Cognitive Grammar Microservice ===~%")
  (format #t "Processing ~a primitives...~%~%" (length primitives))
  (create-cognitive-grammar)
  (for-each (lambda (prim-def)
              (match prim-def
                ((name props)
                 (register-gnumach-primitive name props 
                                             (assoc-ref *cognitive-grammar-rules* name)))))
            gnumach-primitives-examples)
  (let ((to-atomspace-adapter 
         (make-translation-adapter 
          "GNUMach-to-AtomSpace" 'gnumach 'atomspace
          (lambda (name props rule)
            (let ((pattern-fn (assoc-ref rule 'pattern)))
              (pattern-fn name props rule)))
          validate-translation-integrity))
        (from-atomspace-adapter
         (make-translation-adapter
          "AtomSpace-to-GNUMach" 'atomspace 'gnumach  
          (lambda (atoms)
            (decode-atomspace-to-primitive atoms 'GENERIC))
          validate-translation-integrity)))
    (map (lambda (primitive)
           (format #t "Processing primitive: ~a~%" (car primitive))
           (let* ((forward-result (adapter-translate-to-atomspace to-atomspace-adapter primitive))
                  (success? (assoc-ref forward-result 'success)))
             (if success?
                 (let* ((atoms (assoc-ref forward-result 'atoms))
                        (fragment (assoc-ref forward-result 'fragment))
                        (backward-result (adapter-translate-from-atomspace from-atomspace-adapter atoms))
                        (backward-success? (assoc-ref backward-result 'success)))
                   (format #t "  ✅ Forward translation successful~%")
                   (format #t "     Generated ~a atoms~%" (length atoms))
                   (if backward-success?
                       (let* ((recovered-primitive (assoc-ref backward-result 'primitive))
                              (validation (validate-translation-integrity primitive recovered-primitive)))
                         (format #t "  ✅ Backward translation successful~%")
                         (format #t "     Validation: ~a~%" (assoc-ref validation 'valid))
                         `((primitive . ,primitive)
                           (forward . ,forward-result)
                           (backward . ,backward-result) 
                           (validation . ,validation)))
                       (begin
                         (format #t "  ❌ Backward translation failed~%")
                         `((primitive . ,primitive)
                           (forward . ,forward-result)
                           (backward . ,backward-result)))))
                 (begin
                   (format #t "  ❌ Forward translation failed: ~a~%" 
                           (assoc-ref forward-result 'error))
                   `((primitive . ,primitive)
                     (forward . ,forward-result))))))
         primitives)))
(define (test-adapter-round-trip)
  "Test round-trip translation through adapters"
  (format #t "=== Adapter Round-Trip Test ===~%")
  (create-cognitive-grammar)
  (let ((test-primitive '(PORT_ALLOCATE (IPC 1 SERVER 9 2))))
    (register-gnumach-primitive 'PORT_ALLOCATE '(IPC 1 SERVER 9 2)
                                (assoc-ref *cognitive-grammar-rules* 'PORT_ALLOCATE))
    (let ((results (run-translation-microservice (list test-primitive))))
      (format #t "~%Round-trip test completed.~%")
      (car results))))
(define gnumach-primitives-examples
  '((VM_ALLOCATE (MEMORY 2 KERNEL 8 3))
    (PORT_ALLOCATE (IPC 1 SERVER 9 2))
    (THREAD_CREATE (SCHEDULER 3 SYSTEM 7 4))
    (FILE_OPEN (FILESYSTEM 1 TRANSLATOR 6 1))
    (NETWORK_SEND (NETWORK 2 USER 5 2))
    (SIGNAL_POST (SIGNAL 1 SYSTEM 8 3))))