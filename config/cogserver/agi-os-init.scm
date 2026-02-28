;;
;; agi-os-init.scm -- AGI-OS CogServer Guile Shell Initialization
;;
;; This file is loaded when the CogServer starts. It sets up the AtomSpace,
;; loads core modules, and registers the KoboldCpp cognitive inference bridge.
;;
;; After loading, the CogServer Guile shell provides:
;;   - Full AtomSpace access (create, query, pattern-match atoms)
;;   - PLN probabilistic reasoning
;;   - ECAN attention allocation
;;   - KoboldCpp LLM inference (cog-kobold-*)
;;   - Subsystem status monitoring
;;   - Assistant dispatch
;;

;; ============================================================================
;; Core AtomSpace Setup
;; ============================================================================
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))
(use-modules (ice-9 textual-ports))

;; Try to load OpenCog modules (graceful degradation if not installed)
(define (try-load-module mod-name)
  "Attempt to load a Guile module, returning #t on success, #f on failure."
  (catch #t
    (lambda ()
      (eval `(use-modules ,mod-name) (current-module))
      (format #t "  [OK] ~a~%" mod-name)
      #t)
    (lambda (key . args)
      (format #t "  [--] ~a (not available)~%" mod-name)
      #f)))

(display "\n")
(display "╔══════════════════════════════════════════════════════════╗\n")
(display "║              AGI-OS CogServer v0.1.0                    ║\n")
(display "║     Autonomous General Intelligence Operating System    ║\n")
(display "╚══════════════════════════════════════════════════════════╝\n")
(display "\n")
(display "Loading modules...\n")

;; Core OpenCog
(define *has-opencog*     (try-load-module '(opencog)))
(define *has-exec*        (try-load-module '(opencog exec)))
(define *has-type-utils*  (try-load-module '(opencog type-utils)))

;; Reasoning
(define *has-ure*         (try-load-module '(opencog ure)))
(define *has-pln*         (try-load-module '(opencog pln)))

;; Attention
(define *has-attention*   (try-load-module '(opencog attention)))

;; Persistence
(define *has-persist*     (try-load-module '(opencog persist)))

;; KoboldCpp cognitive LLM
(define *has-koboldcpp*   (try-load-module '(opencog koboldcpp-cog)))

(display "\n")

;; ============================================================================
;; AGI-OS AtomSpace Initialization
;; ============================================================================

(define *agi-os-version* "0.1.0")
(define *agi-os-atomspace* #f)

(define (agi-os-init)
  "Initialize the AGI-OS AtomSpace with core knowledge."
  (when *has-opencog*
    ;; Create the main AtomSpace
    (set! *agi-os-atomspace* (cog-atomspace))

    ;; Register AGI-OS metadata
    (Concept "AGI-OS")
    (Concept "AGI-OS:version")
    (Evaluation
      (Predicate "has-version")
      (List (Concept "AGI-OS") (Concept *agi-os-version*)))

    ;; Register subsystem atoms
    (Concept "subsystem:cogutil")
    (Concept "subsystem:atomspace")
    (Concept "subsystem:cogserver")
    (Concept "subsystem:pln")
    (Concept "subsystem:ure")
    (Concept "subsystem:attention")
    (Concept "subsystem:koboldcpp-cog")
    (Concept "subsystem:cognumach")
    (Concept "subsystem:hurdcog")
    (Concept "subsystem:cognitive-grip")

    ;; Register subsystem status
    (define (register-subsystem name available?)
      (Evaluation
        (Predicate "subsystem-status")
        (List (Concept (string-append "subsystem:" name))
              (Concept (if available? "loaded" "not-available")))))

    (register-subsystem "opencog" *has-opencog*)
    (register-subsystem "ure" *has-ure*)
    (register-subsystem "pln" *has-pln*)
    (register-subsystem "attention" *has-attention*)
    (register-subsystem "koboldcpp-cog" *has-koboldcpp*)

    (format #t "AGI-OS AtomSpace initialized with ~a atoms~%"
            (cog-count-atoms 'Atom))))

;; Run initialization
(agi-os-init)

;; ============================================================================
;; Subsystem Status
;; ============================================================================

(define (agi-os-status)
  "Display the status of all AGI-OS subsystems."
  (display "\n")
  (display "┌──────────────────────────────────────────────────────┐\n")
  (display "│              AGI-OS Subsystem Status                 │\n")
  (display "├──────────────────────────────────────────────────────┤\n")
  (format #t "│  OpenCog Core:     ~a~30t│~%"
          (if *has-opencog* "LOADED" "---"))
  (format #t "│  URE:              ~a~30t│~%"
          (if *has-ure* "LOADED" "---"))
  (format #t "│  PLN:              ~a~30t│~%"
          (if *has-pln* "LOADED" "---"))
  (format #t "│  Attention (ECAN): ~a~30t│~%"
          (if *has-attention* "LOADED" "---"))
  (format #t "│  KoboldCpp-Cog:    ~a~30t│~%"
          (if *has-koboldcpp* "LOADED" "---"))
  (when *has-koboldcpp*
    (format #t "│    Endpoint:       ~a~30t│~%"
            (cog-kobold-endpoint)))
  (when *has-opencog*
    (format #t "│  AtomSpace atoms:  ~a~30t│~%"
            (cog-count-atoms 'Atom)))
  (display "└──────────────────────────────────────────────────────┘\n")
  (display "\n"))

;; ============================================================================
;; KoboldCpp Assistant Dispatch
;; ============================================================================

(define (agi-os-ask question)
  "Ask the AGI-OS cognitive assistant a question.
   Uses KoboldCpp for LLM inference with AtomSpace context."
  (if *has-koboldcpp*
      (cog-kobold-infer question "query")
      "KoboldCpp-Cog not loaded. Install and start KoboldCpp server."))

(define (agi-os-engineer question)
  "Ask the engineering assistant about AGI-OS development.
   Specialized for build system, architecture, and code questions."
  (if *has-koboldcpp*
      (cog-kobold-infer
        (string-append
          "You are an AGI-OS engineering assistant. "
          "You have deep knowledge of the agi-os repository structure, "
          "build system (CMake, autotools), Debian packaging, and the "
          "three-layer architecture (CogNUMach microkernel, HurdCog OS, "
          "OpenCog Collection). Answer this engineering question:\n\n"
          question)
        "query")
      "KoboldCpp-Cog not loaded."))

(define (agi-os-dev question)
  "Ask the development assistant for coding help.
   Specialized for C++, Scheme, Python development within AGI-OS."
  (if *has-koboldcpp*
      (cog-kobold-infer
        (string-append
          "You are an AGI-OS development assistant. "
          "Help with C++, Guile Scheme, and Python code for the "
          "OpenCog AtomSpace, CogServer modules, PLN reasoning rules, "
          "and KoboldCpp integration. Provide working code examples.\n\n"
          question)
        "generate")
      "KoboldCpp-Cog not loaded."))

(define (agi-os-manage command)
  "Management assistant for AGI-OS operations.
   Handles subsystem monitoring, configuration, and orchestration."
  (if *has-koboldcpp*
      (cog-kobold-infer
        (string-append
          "You are an AGI-OS management assistant. "
          "You manage the subsystems: CogNUMach (microkernel), "
          "HurdCog (OS), OpenCog (cognition), KoboldCpp (LLM), "
          "CogBolt (IDE), and Cognitive-Grip (integration). "
          "Provide operational guidance for:\n\n"
          command)
        "query")
      "KoboldCpp-Cog not loaded."))

;; ============================================================================
;; Convenience Functions
;; ============================================================================

(define (agi-os-help)
  "Display available AGI-OS commands."
  (display "\n")
  (display "AGI-OS CogServer Commands:\n")
  (display "──────────────────────────\n")
  (display "  (agi-os-status)          - Show subsystem status\n")
  (display "  (agi-os-help)            - Show this help\n")
  (display "  (agi-os-ask \"question\")   - Ask the cognitive assistant\n")
  (display "  (agi-os-engineer \"q\")     - Engineering assistant\n")
  (display "  (agi-os-dev \"q\")          - Development assistant\n")
  (display "  (agi-os-manage \"cmd\")     - Management assistant\n")
  (display "\n")
  (display "AtomSpace Commands:\n")
  (display "  (Concept \"name\")          - Create a ConceptNode\n")
  (display "  (cog-get-atoms 'Concept)  - List all ConceptNodes\n")
  (display "  (cog-count-atoms 'Atom)   - Count all atoms\n")
  (display "\n")
  (when *has-koboldcpp*
    (display "KoboldCpp Commands:\n")
    (display "  (cog-kobold-endpoint)     - Show LLM endpoint\n")
    (display "  (cog-kobold-connected?)   - Check LLM connection\n")
    (display "  (cog-kobold-generate p n) - Generate text\n")
    (display "  (cog-kobold-chat msg)     - Chat with LLM\n")
    (display "  (cog-kobold-infer q mode) - Cognitive inference\n")
    (display "\n")))

;; Show status on startup
(agi-os-status)
(display "Type (agi-os-help) for available commands.\n\n")
