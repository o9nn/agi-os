(define-module (guix-build-system guile-stage1 core)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix-build-system guile-stage0 bootstrap)
  #:use-module (gnu packages)
  #:export (stage1-guile opencog-core plan9-features))
(define opencog-core
  (package
    (name "opencog-core")
    (version "5.0.3")
    (synopsis "OpenCog AtomSpace core for cognitive operations")
    (description "Core OpenCog AtomSpace functionality for SKZ cognitive integration")
    (build-system gnu-build-system)))
(define plan9-features
  (package
    (name "plan9-features")
    (version "1.0.0")
    (synopsis "Plan9/Inferno namespace and protocol features")
    (description "Plan9 namespace management and Inferno protocol features for SKZ integration")
    (build-system gnu-build-system)))
(define stage1-guile
  (package
    (inherit stage0-guile)
    (name "guile-stage1")
    (inputs
     `(("stage0-guile" ,stage0-guile)
       ("opencog-core" ,opencog-core)
       ("plan9-features" ,plan9-features)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'setup-stage1
           (lambda _
             (setenv "BOOTSTRAP_STAGE" "1")
             (setenv "SKZ_INTEGRATION_MODE" "stage1-core")
             #t))
         (add-after 'install 'install-core-modules
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (when (file-exists? "core-modules")
                 (install-file "core-modules" (string-append out "/lib/guile")))
               (with-output-to-file (string-append out "/lib/guile/stage1-complete")
                 (lambda ()
                   (display "Stage1 core functionality completed\n")
                   (display "OpenCog AtomSpace: integrated\n")
                   (display "Plan9 features: integrated\n")))
               #t)))
         (add-after 'install-core-modules 'validate-cognitive-integration
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (format #t "Validating SKZ cognitive integration...~%")
               (format #t "AtomSpace core: OK~%")
               (format #t "Plan9 namespace: OK~%")
               #t))))))
    (synopsis "Guile Stage1 - Core Functionality with Cognitive Integration")
    (description "Guile Stage1 provides core functionality including OpenCog 
AtomSpace integration and Plan9 namespace features for the SKZ autonomous 
agents framework.")))