(define-module (cogkernel security security-config)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (make-security-config
            security-config-get
            security-config-set!
            validate-security-config
            load-security-policy
            *default-security-config*
            *security-policies*))
(format #t "Initializing HurdCog Security Configuration System...~%")
(define *default-security-config*
  `((authentication
     (method . 'capability-based)
     (require-mfa . #t)
     (session-timeout . 3600))
    (access-control
     (model . 'rbac)
     (default-permissions . '(read))
     (privilege-escalation . #f)
     (audit-actions . #t))
    (encryption
     (algorithm . 'aes-256-gcm)
     (key-rotation-interval . 86400)
     (encrypt-at-rest . #t)
     (encrypt-in-transit . #t))
    (network-security
     (tls-version . 'tls-1.3-minimum)
     (certificate-validation . 'strict)
     (allowed-protocols . '(https secure-ipc))
     (firewall-enabled . #t))
    (audit-logging
     (enabled . #t)
     (level . 'detailed)
     (retention-days . 365)
     (real-time-alerts . #t))
    (vulnerability-management
     (scan-frequency . 'daily)
     (auto-patching . #f)
     (severity-threshold . 'medium)
     (notification-channels . '(log alert)))
    (microkernel-security
     (isolated-atomspaces . #t)
     (secure-ipc . #t)
     (memory-protection . #t)
     (sandboxed-agents . #t))
    (cognitive-security
     (trusted-learning . #t)
     (bias-detection . #t)
     (decision-auditing . #t)
     (knowledge-integrity . #t))))
(define *security-policies* '())
(define (make-security-config)
  "Create a new security configuration instance"
  (let ((config (copy-tree *default-security-config*))
        (timestamp (current-time))
        (version "1.0"))
    `((config . ,config)
      (timestamp . ,timestamp)
      (version . ,version)
      (checksum . ,(compute-config-checksum config)))))
(define (security-config-get config section key)
  "Get a security configuration value"
  (let ((cfg (assoc-ref config 'config)))
    (let ((section-data (assoc-ref cfg section)))
      (if section-data
          (assoc-ref section-data key)
          #f))))
(define (security-config-set! config section key value)
  "Set a security configuration value"
  (let ((cfg (assoc-ref config 'config)))
    (let ((section-data (assoc-ref cfg section)))
      (if section-data
          (begin
            (assoc-set! section-data key value)
            (assoc-set! config 'timestamp (current-time))
            (assoc-set! config 'checksum 
                        (compute-config-checksum cfg))
            #t)
          #f))))
(define (compute-config-checksum config)
  "Compute checksum for configuration integrity"
  (string-hash (format #f "~a" config)))
(define (validate-security-config config)
  "Validate security configuration against policies and best practices"
  (format #t "🔒 Validating security configuration...~%")
  (let ((violations '())
        (warnings '())
        (cfg (assoc-ref config 'config)))
    (unless (security-config-get config 'authentication 'require-mfa)
      (set! violations (cons 'mfa-required violations)))
    (let ((timeout (security-config-get config 'authentication 'session-timeout)))
      (when (> timeout 7200)
        (set! warnings (cons 'session-timeout-long warnings))))
    (unless (security-config-get config 'encryption 'encrypt-at-rest)
      (set! violations (cons 'encryption-at-rest-required violations)))
    (unless (security-config-get config 'encryption 'encrypt-in-transit)
      (set! violations (cons 'encryption-in-transit-required violations)))
    (unless (security-config-get config 'audit-logging 'enabled)
      (set! violations (cons 'audit-logging-required violations)))
    (unless (security-config-get config 'microkernel-security 'isolated-atomspaces)
      (set! violations (cons 'atomspace-isolation-required violations)))
    (unless (security-config-get config 'microkernel-security 'sandboxed-agents)
      (set! violations (cons 'agent-sandboxing-required violations)))
    (format #t "  Violations: ~a~%" violations)
    (format #t "  Warnings: ~a~%" warnings)
    (if (null? violations)
        (begin
          (format #t "✅ Security configuration validation passed~%")
          `((status . 'valid)
            (warnings . ,warnings)))
        (begin
          (format #t "❌ Security configuration validation failed~%")
          `((status . 'invalid)
            (violations . ,violations)
            (warnings . ,warnings))))))
(define (load-security-policy policy-name)
  "Load a predefined security policy"
  (format #t "🔒 Loading security policy: ~a~%" policy-name)
  (match policy-name
    ('high-security
     (append *default-security-config*
             '((authentication (require-mfa . #t)
                              (session-timeout . 1800))
               (access-control (privilege-escalation . #f)
                              (default-permissions . '()))
               (vulnerability-management (auto-patching . #t)
                                       (severity-threshold . 'low)))))
    ('development
     (append *default-security-config*
             '((authentication (require-mfa . #f)
                              (session-timeout . 7200))
               (vulnerability-management (scan-frequency . 'weekly)
                                       (severity-threshold . 'high)))))
    ('production
     (append *default-security-config*
             '((authentication (require-mfa . #t)
                              (session-timeout . 3600))
               (audit-logging (level . 'comprehensive)
                             (real-time-alerts . #t))
               (vulnerability-management (scan-frequency . 'daily)
                                       (auto-patching . #f)
                                       (severity-threshold . 'medium)))))
    (else
     (format #t "⚠️ Unknown security policy: ~a, using default~%" policy-name)
     *default-security-config*)))
(define (validate-authentication-config auth-config)
  "Validate authentication configuration"
  (let ((violations '()))
    (unless (assoc-ref auth-config 'require-mfa)
      (set! violations (cons 'mfa-required violations)))
    (let ((timeout (assoc-ref auth-config 'session-timeout)))
      (when (< timeout 300)
        (set! violations (cons 'session-timeout-too-short violations)))
      (when (> timeout 86400)
        (set! violations (cons 'session-timeout-too-long violations))))
    violations))
(define (initialize-security-config)
  "Initialize the security configuration system"
  (format #t "🔒 Initializing HurdCog Security Configuration System~%")
  (set! *security-policies*
        (list (cons 'default *default-security-config*)
              (cons 'high-security (load-security-policy 'high-security))
              (cons 'development (load-security-policy 'development))
              (cons 'production (load-security-policy 'production))))
  (format #t "✅ Security configuration system initialized with ~a policies~%"
          (length *security-policies*))
  #t)
(define (test-security-config)
  "Test the security configuration system"
  (format #t "~%=== Testing Security Configuration System ===~%")
  (let ((config (make-security-config)))
    (format #t "Created security config with checksum: ~a~%"
            (assoc-ref config 'checksum))
    (let ((mfa-required (security-config-get config 'authentication 'require-mfa)))
      (format #t "MFA required: ~a~%" mfa-required))
    (let ((validation (validate-security-config config)))
      (format #t "Validation result: ~a~%" validation))
    (let ((prod-policy (load-security-policy 'production)))
      (format #t "Production policy loaded with ~a sections~%"
              (length prod-policy)))
    #t))
(initialize-security-config)
(format #t "✅ HurdCog Security Configuration System ready~%")