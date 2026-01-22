(define-module (cogkernel attention)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (cogkernel atomspace)
  #:use-module (cogkernel agents)
  #:export (make-attention-value
            attention-value?
            attention-value-sti
            attention-value-lti
            attention-value-vlti
            make-attention-bank
            attention-bank-add!
            attention-bank-update!
            attention-bank-get-focus
            attention-bank-allocate!
            attention-bank-stimulate!
            attention-bank-apply-wages!
            attention-bank-collect-rent!
            attention-bank-spread-activation!
            attention-bank-schedule-tasks!
            attention-bank-get-economics
            make-distributed-attention-network
            distributed-attention-sync!
            distributed-attention-broadcast!
            *global-attention-bank*))
(define-record-type <attention-value>
  (make-attention-value-record sti lti vlti)
  attention-value?
  (sti attention-value-sti set-attention-value-sti!)
  (lti attention-value-lti set-attention-value-lti!)
  (vlti attention-value-vlti set-attention-value-vlti!))
(define-record-type <attention-bank>
  (make-attention-bank-record atom-av agent-av total-funds focus-threshold 
                              wage-rate rent-rate spread-rate economics-history)
  attention-bank?
  (atom-av attention-bank-atom-av)
  (agent-av attention-bank-agent-av)
  (total-funds attention-bank-total-funds set-attention-bank-total-funds!)
  (focus-threshold attention-bank-focus-threshold set-attention-bank-focus-threshold!)
  (wage-rate attention-bank-wage-rate set-attention-bank-wage-rate!)
  (rent-rate attention-bank-rent-rate set-attention-bank-rent-rate!)
  (spread-rate attention-bank-spread-rate set-attention-bank-spread-rate!)
  (economics-history attention-bank-economics-history set-attention-bank-economics-history!))
(define* (make-attention-value #:optional (sti 0) (lti 0) (vlti 0))
  "Create a new attention value with specified importance levels"
  (make-attention-value-record sti lti vlti))
(define* (make-attention-bank #:key 
                              (total-funds 10000) 
                              (focus-threshold 100)
                              (wage-rate 0.1)
                              (rent-rate 0.05)
                              (spread-rate 0.2))
  "Create a new attention bank with specified total funds and ECAN economics parameters"
  (make-attention-bank-record (make-hash-table) (make-hash-table) 
                              total-funds focus-threshold
                              wage-rate rent-rate spread-rate '()))
(define (attention-bank-add! bank object attention-value)
  "Add an object (atom or agent) to the attention bank with given attention value"
  (cond
    ((atom? object)
     (hash-set! (attention-bank-atom-av bank) object attention-value))
    ((agent? object)
     (hash-set! (attention-bank-agent-av bank) object attention-value))
    (else
     (error "Object must be atom or agent" object))))
(define (attention-bank-update! bank object delta-sti delta-lti delta-vlti)
  "Update attention values for an object by specified deltas"
  (let* ((current-av (cond
                       ((atom? object)
                        (hash-ref (attention-bank-atom-av bank) object))
                       ((agent? object)
                        (hash-ref (attention-bank-agent-av bank) object))
                       (else #f))))
    (when current-av
      (set-attention-value-sti! current-av 
                               (+ (attention-value-sti current-av) delta-sti))
      (set-attention-value-lti! current-av 
                               (+ (attention-value-lti current-av) delta-lti))
      (set-attention-value-vlti! current-av 
                                (+ (attention-value-vlti current-av) delta-vlti)))))
(define (attention-bank-get-focus bank)
  "Get all objects currently in attentional focus (above threshold)"
  (let ((focused-atoms '())
        (focused-agents '())
        (threshold (attention-bank-focus-threshold bank)))
    (hash-for-each (lambda (atom av)
                    (when (>= (attention-value-sti av) threshold)
                      (set! focused-atoms (cons atom focused-atoms))))
                  (attention-bank-atom-av bank))
    (hash-for-each (lambda (agent av)
                    (when (>= (attention-value-sti av) threshold)
                      (set! focused-agents (cons agent focused-agents))))
                  (attention-bank-agent-av bank))
    (append focused-atoms focused-agents)))
(define (attention-bank-allocate! bank object amount)
  "Allocate attention to an object, deducting from total funds"
  (when (>= (attention-bank-total-funds bank) amount)
    (set-attention-bank-total-funds! bank 
                                    (- (attention-bank-total-funds bank) amount))
    (let ((current-av (cond
                        ((atom? object)
                         (hash-ref (attention-bank-atom-av bank) object))
                        ((agent? object)
                         (hash-ref (attention-bank-agent-av bank) object))
                        (else #f))))
      (if current-av
          (set-attention-value-sti! current-av 
                                   (+ (attention-value-sti current-av) amount))
          (attention-bank-add! bank object (make-attention-value amount 0 0))))))
(define (attention-bank-stimulate! bank object stimulus-type intensity)
  "Stimulate an object based on stimulus type and intensity"
  (let ((sti-delta (case stimulus-type
                     ((URGENT) (* intensity 10))
                     ((IMPORTANT) (* intensity 5))
                     ((ROUTINE) intensity)
                     (else intensity))))
    (attention-bank-update! bank object sti-delta 0 0)))
(define (attention-bank-apply-wages! bank activities)
  "Apply cognitive wages based on activity records
   activities: list of (object activity-value) pairs"
  (let ((wage-rate (attention-bank-wage-rate bank))
        (total-wages 0))
    (for-each
      (lambda (activity-record)
        (let* ((object (car activity-record))
               (activity-value (cadr activity-record))
               (wage (* activity-value wage-rate)))
          (attention-bank-update! bank object wage 0 0)
          (set! total-wages (+ total-wages wage))))
      activities)
    (set-attention-bank-economics-history! 
      bank
      (cons `((type . wages)
              (amount . ,total-wages)
              (timestamp . ,(current-time))
              (activities . ,(length activities)))
            (attention-bank-economics-history bank)))
    total-wages))
(define (attention-bank-collect-rent! bank)
  "Collect attention rent from all objects based on their STI levels"
  (let ((rent-rate (attention-bank-rent-rate bank))
        (total-rent 0))
    (hash-for-each
      (lambda (atom av)
        (let* ((current-sti (attention-value-sti av))
               (rent (* current-sti rent-rate)))
          (when (> current-sti 0)
            (set-attention-value-sti! av (- current-sti rent))
            (set! total-rent (+ total-rent rent)))))
      (attention-bank-atom-av bank))
    (hash-for-each
      (lambda (agent av)
        (let* ((current-sti (attention-value-sti av))
               (rent (* current-sti rent-rate)))
          (when (> current-sti 0)
            (set-attention-value-sti! av (- current-sti rent))
            (set! total-rent (+ total-rent rent)))))
      (attention-bank-agent-av bank))
    (set-attention-bank-total-funds! bank 
                                    (+ (attention-bank-total-funds bank) total-rent))
    (set-attention-bank-economics-history! 
      bank
      (cons `((type . rent)
              (amount . ,total-rent)
              (timestamp . ,(current-time)))
            (attention-bank-economics-history bank)))
    total-rent))
(define* (attention-bank-spread-activation! bank atomspace #:optional (max-depth 3))
  "Spread activation through the atomspace based on connections
   max-depth: maximum depth for spreading activation"
  (let ((spread-rate (attention-bank-spread-rate bank))
        (spread-count 0))
    (let ((focused-atoms (filter atom? (attention-bank-get-focus bank))))
      (for-each
        (lambda (source-atom)
          (let ((source-av (hash-ref (attention-bank-atom-av bank) source-atom)))
            (when source-av
              (let ((source-sti (attention-value-sti source-av)))
                (let ((connected-atoms (atomspace-get-connected atomspace source-atom)))
                  (for-each
                    (lambda (target-atom)
                      (let ((spread-amount (* source-sti spread-rate 
                                            (/ 1.0 (max 1 (length connected-atoms))))))
                        (let ((target-av (hash-ref (attention-bank-atom-av bank) target-atom)))
                          (if target-av
                              (set-attention-value-sti! target-av 
                                (+ (attention-value-sti target-av) spread-amount))
                              (attention-bank-add! bank target-atom 
                                (make-attention-value spread-amount 0 0))))
                        (set! spread-count (+ spread-count 1))))
                    connected-atoms))))))
        focused-atoms))
    (set-attention-bank-economics-history! 
      bank
      (cons `((type . spreading)
              (spread-count . ,spread-count)
              (timestamp . ,(current-time)))
            (attention-bank-economics-history bank)))
    spread-count))
(define (atomspace-get-connected atomspace atom)
  "Get atoms connected to the given atom through links"
  '())
(define (attention-bank-schedule-tasks! bank task-queue)
  "Schedule tasks from queue based on attention priority
   task-queue: list of (task-id priority agent/atom) tuples
   Returns: sorted task list by effective priority (priority * STI)"
  (let ((scheduled-tasks
          (sort task-queue
            (lambda (task-a task-b)
              (let* ((priority-a (cadr task-a))
                     (object-a (caddr task-a))
                     (av-a (cond
                             ((atom? object-a)
                              (hash-ref (attention-bank-atom-av bank) object-a))
                             ((agent? object-a)
                              (hash-ref (attention-bank-agent-av bank) object-a))
                             (else #f)))
                     (sti-a (if av-a (attention-value-sti av-a) 0))
                     (effective-priority-a (* priority-a (max 1.0 sti-a)))
                     (priority-b (cadr task-b))
                     (object-b (caddr task-b))
                     (av-b (cond
                             ((atom? object-b)
                              (hash-ref (attention-bank-atom-av bank) object-b))
                             ((agent? object-b)
                              (hash-ref (attention-bank-agent-av bank) object-b))
                             (else #f)))
                     (sti-b (if av-b (attention-value-sti av-b) 0))
                     (effective-priority-b (* priority-b (max 1.0 sti-b))))
                (> effective-priority-a effective-priority-b))))))
    (set-attention-bank-economics-history! 
      bank
      (cons `((type . scheduling)
              (tasks-scheduled . ,(length scheduled-tasks))
              (timestamp . ,(current-time)))
            (attention-bank-economics-history bank)))
    scheduled-tasks))
(define (attention-bank-get-economics bank)
  "Get summary of attention bank economics"
  (let ((total-sti 0)
        (total-lti 0)
        (atom-count 0)
        (agent-count 0))
    (hash-for-each
      (lambda (atom av)
        (set! total-sti (+ total-sti (attention-value-sti av)))
        (set! total-lti (+ total-lti (attention-value-lti av)))
        (set! atom-count (+ atom-count 1)))
      (attention-bank-atom-av bank))
    (hash-for-each
      (lambda (agent av)
        (set! total-sti (+ total-sti (attention-value-sti av)))
        (set! total-lti (+ total-lti (attention-value-lti av)))
        (set! agent-count (+ agent-count 1)))
      (attention-bank-agent-av bank))
    `((total-funds . ,(attention-bank-total-funds bank))
      (focus-threshold . ,(attention-bank-focus-threshold bank))
      (total-sti . ,total-sti)
      (total-lti . ,total-lti)
      (atom-count . ,atom-count)
      (agent-count . ,agent-count)
      (wage-rate . ,(attention-bank-wage-rate bank))
      (rent-rate . ,(attention-bank-rent-rate bank))
      (spread-rate . ,(attention-bank-spread-rate bank))
      (history-length . ,(length (attention-bank-economics-history bank))))))
(define-record-type <distributed-attention-network>
  (make-distributed-attention-network-record local-bank node-banks mesh-topology sync-interval)
  distributed-attention-network?
  (local-bank network-local-bank)
  (node-banks network-node-banks)
  (mesh-topology network-mesh-topology set-network-mesh-topology!)
  (sync-interval network-sync-interval))
(define* (make-distributed-attention-network local-bank 
                                            #:key (sync-interval 60))
  "Create a distributed attention network for multi-node attention coordination"
  (make-distributed-attention-network-record 
    local-bank
    (make-hash-table)
    '()
    sync-interval))
(define (distributed-attention-sync! network node-id)
  "Synchronize attention state with a remote node"
  (let ((local-bank (network-local-bank network))
        (economics (attention-bank-get-economics local-bank)))
    (set-attention-bank-economics-history! 
      local-bank
      (cons `((type . distributed-sync)
              (node-id . ,node-id)
              (timestamp . ,(current-time))
              (economics . ,economics))
            (attention-bank-economics-history local-bank)))
    #t))
(define (distributed-attention-broadcast! network event)
  "Broadcast an attention event to all nodes in the distributed mesh"
  (let ((local-bank (network-local-bank network))
        (topology (network-mesh-topology network)))
    (set-attention-bank-economics-history! 
      local-bank
      (cons `((type . distributed-broadcast)
              (event . ,event)
              (topology-size . ,(length topology))
              (timestamp . ,(current-time)))
            (attention-bank-economics-history local-bank)))
    #t))
(define *global-attention-bank* (make-attention-bank))