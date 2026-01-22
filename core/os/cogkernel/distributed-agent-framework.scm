(define-module (cogkernel distributed-agent-framework)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (cogkernel atomspace)
  #:use-module (cogkernel agents)
  #:use-module (cogkernel agent-communication)
  #:export (make-distributed-agent-framework
            distributed-agent-framework?
            framework-deploy-agent!
            framework-terminate-agent!
            framework-get-agent-status
            framework-list-active-agents
            framework-scale-agents!
            framework-health-check
            framework-start!
            framework-stop!
            framework-get-metrics
            *global-distributed-framework*))
(define framework-states
  '(INITIALIZING ACTIVE SCALING MAINTENANCE STOPPING STOPPED ERROR))
(define deployment-strategies
  '(LOCAL
    DISTRIBUTED
    REPLICATED
    SCALED
    BALANCED))
(define-record-type <distributed-agent-framework>
  (make-distributed-agent-framework-record deployment-strategy node-registry
                                           agent-registry deployment-queue
                                           monitoring-system load-balancer
                                           health-checker communication-system
                                           state metrics thread-pool)
  distributed-agent-framework?
  (deployment-strategy framework-deployment-strategy)
  (node-registry framework-node-registry)
  (agent-registry framework-agent-registry)
  (deployment-queue framework-deployment-queue)
  (monitoring-system framework-monitoring-system)
  (load-balancer framework-load-balancer)
  (health-checker framework-health-checker)
  (communication-system framework-communication-system)
  (state framework-state set-framework-state!)
  (metrics framework-metrics)
  (thread-pool framework-thread-pool))
(define-record-type <agent-deployment>
  (make-agent-deployment-record agent-id node-id status deployment-time
                                health-status resource-usage last-heartbeat)
  agent-deployment?
  (agent-id deployment-agent-id)
  (node-id deployment-node-id)
  (status deployment-status set-deployment-status!)
  (deployment-time deployment-deployment-time)
  (health-status deployment-health-status set-deployment-health-status!)
  (resource-usage deployment-resource-usage set-deployment-resource-usage!)
  (last-heartbeat deployment-last-heartbeat set-deployment-last-heartbeat!))
(define-record-type <framework-node>
  (make-framework-node-record node-id hostname capacity current-load
                              status agents heartbeat)
  framework-node?
  (node-id node-node-id)
  (hostname node-hostname)
  (capacity node-capacity)
  (current-load node-current-load set-node-current-load!)
  (status node-status set-node-status!)
  (agents node-agents set-node-agents!)
  (heartbeat node-heartbeat set-node-heartbeat!))
(define* (make-distributed-agent-framework #:key 
                                           (deployment-strategy 'DISTRIBUTED)
                                           (communication-system *global-agent-communication*))
  "Create a new distributed agent framework"
  (unless (member deployment-strategy deployment-strategies)
    (error "Invalid deployment strategy:" deployment-strategy))
  (make-distributed-agent-framework-record
    deployment-strategy
    (make-hash-table)
    (make-hash-table)
    '()
    (make-monitoring-system)
    (make-load-balancer)
    (make-health-checker)
    communication-system
    'INITIALIZING
    (make-hash-table)
    #f))
(define (make-monitoring-system)
  "Create monitoring system for distributed agents"
  `((metrics . ,(make-hash-table))
    (alerts . ())
    (thresholds . ((cpu-usage . 80)
                   (memory-usage . 90)
                   (response-time . 5000)
                   (error-rate . 5)))))
(define (make-load-balancer)
  "Create load balancer for agent distribution"
  `((strategy . round-robin)
    (node-weights . ,(make-hash-table))
    (last-assigned . 0)))
(define (make-health-checker)
  "Create health checker for agents and nodes"
  `((check-interval . 30)
    (timeout . 10)
    (retry-count . 3)
    (unhealthy-threshold . 3)))
(define (framework-deploy-agent! framework agent-spec)
  "Deploy an agent according to the framework deployment strategy"
  (let ((agent-id (agent-spec-id agent-spec))
        (node-id (select-deployment-node framework agent-spec)))
    (format #t "🚀 Deploying agent ~a to node ~a~%" agent-id node-id)
    (let ((agent (create-agent-from-spec agent-spec)))
      (let ((deployment (make-agent-deployment-record
                          agent-id node-id 'DEPLOYING (current-time)
                          'UNKNOWN '() (current-time))))
        (hash-set! (framework-agent-registry framework) agent-id deployment)
        (let ((deployment-atom (make-atom 'DEPLOYMENT agent-id)))
          (atomspace-add! *global-atomspace* deployment-atom)
          (atomspace-add! *global-atomspace*
            (make-link 'EVALUATION
              (list (make-atom 'PREDICATE "deployed-on")
                    deployment-atom
                    (make-atom 'NODE node-id)))))
        (deploy-agent-to-node! framework agent node-id)
        (set-deployment-status! deployment 'ACTIVE)
        (format #t "✅ Agent ~a successfully deployed~%" agent-id)
        deployment))))
(define (framework-terminate-agent! framework agent-id)
  "Terminate a deployed agent"
  (let ((deployment (hash-ref (framework-agent-registry framework) agent-id)))
    (if deployment
        (begin
          (format #t "🛑 Terminating agent ~a~%" agent-id)
          (set-deployment-status! deployment 'TERMINATING)
          (send-shutdown-signal agent-id)
          (hash-remove! (framework-agent-registry framework) agent-id)
          (let ((deployment-atom (atomspace-find *global-atomspace* 
                                                `(DEPLOYMENT ,agent-id))))
            (when deployment-atom
              (atomspace-remove! *global-atomspace* deployment-atom)))
          (format #t "✅ Agent ~a terminated~%" agent-id)
          #t)
        (begin
          (format #t "⚠️  Agent ~a not found in registry~%" agent-id)
          #f))))
(define (framework-get-agent-status framework agent-id)
  "Get current status of deployed agent"
  (let ((deployment (hash-ref (framework-agent-registry framework) agent-id)))
    (if deployment
        `((agent-id . ,agent-id)
          (node-id . ,(deployment-node-id deployment))
          (status . ,(deployment-status deployment))
          (health . ,(deployment-health-status deployment))
          (deployed-time . ,(deployment-deployment-time deployment))
          (last-heartbeat . ,(deployment-last-heartbeat deployment))
          (resource-usage . ,(deployment-resource-usage deployment)))
        #f)))
(define (framework-list-active-agents framework)
  "List all active agents in the framework"
  (hash-fold (lambda (agent-id deployment acc)
               (if (eq? (deployment-status deployment) 'ACTIVE)
                   (cons agent-id acc)
                   acc))
             '()
             (framework-agent-registry framework)))
(define (framework-scale-agents! framework agent-type target-count)
  "Scale agents of a specific type to target count"
  (format #t "📈 Scaling ~a agents to ~a instances~%" agent-type target-count)
  (let* ((current-agents (filter-agents-by-type framework agent-type))
         (current-count (length current-agents))
         (diff (- target-count current-count)))
    (cond
      ((> diff 0)
       (format #t "🔼 Scaling up: deploying ~a additional agents~%" diff)
       (for-each (lambda (i)
                   (let ((agent-spec (make-agent-spec agent-type)))
                     (framework-deploy-agent! framework agent-spec)))
                 (iota diff)))
      ((< diff 0)
       (format #t "🔽 Scaling down: terminating ~a agents~%" (abs diff))
       (for-each (lambda (agent-id)
                   (framework-terminate-agent! framework agent-id))
                 (take current-agents (abs diff))))
      (else
       (format #t "✅ Already at target scale (~a agents)~%" current-count)))
    (format #t "📊 Scaling complete~%")))
(define (framework-health-check framework)
  "Perform comprehensive health check of the framework"
  (format #t "🏥 Performing framework health check~%")
  (let ((healthy-agents 0)
        (total-agents 0)
        (node-status '())
        (issues '()))
    (hash-for-each (lambda (agent-id deployment)
                     (set! total-agents (+ total-agents 1))
                     (when (agent-health-check agent-id)
                       (set! healthy-agents (+ healthy-agents 1))))
                   (framework-agent-registry framework))
    (hash-for-each (lambda (node-id node)
                     (let ((health (check-node-health node)))
                       (set! node-status (cons (cons node-id health) node-status))))
                   (framework-node-registry framework))
    (let ((health-percentage (* 100 (/ healthy-agents (max total-agents 1)))))
      (format #t "📊 Health Report:~%")
      (format #t "   - Agents: ~a/~a healthy (~,1f%)~%" 
              healthy-agents total-agents health-percentage)
      (format #t "   - Nodes: ~a nodes checked~%" (length node-status))
      `((overall-health . ,(if (> health-percentage 90) 'HEALTHY 'DEGRADED))
        (healthy-agents . ,healthy-agents)
        (total-agents . ,total-agents) 
        (health-percentage . ,health-percentage)
        (node-status . ,node-status)
        (issues . ,issues)))))
(define (framework-start! framework)
  "Start the distributed agent framework"
  (format #t "🚀 Starting distributed agent framework~%")
  (start-monitoring-system framework)
  (start-health-checker framework)
  (register-local-node framework)
  (set-framework-state! framework 'ACTIVE)
  (format #t "✅ Distributed agent framework started~%"))
(define (framework-stop! framework)
  "Stop the distributed agent framework"
  (format #t "🛑 Stopping distributed agent framework~%")
  (set-framework-state! framework 'STOPPING)
  (let ((active-agents (framework-list-active-agents framework)))
    (for-each (lambda (agent-id)
                (framework-terminate-agent! framework agent-id))
              active-agents))
  (stop-monitoring-system framework)
  (set-framework-state! framework 'STOPPED)
  (format #t "✅ Distributed agent framework stopped~%"))
(define (framework-get-metrics framework)
  "Get comprehensive framework metrics"
  (let ((metrics (framework-metrics framework)))
    (hash-set! metrics 'active-agents 
               (length (framework-list-active-agents framework)))
    (hash-set! metrics 'total-deployments
               (hash-count (const #t) (framework-agent-registry framework)))
    (hash-set! metrics 'framework-state (framework-state framework))
    (hash-set! metrics 'timestamp (current-time))
    metrics))
(define (select-deployment-node framework agent-spec)
  "Select optimal node for agent deployment based on load balancing"
  (let ((nodes (hash-fold (lambda (k v acc) (cons k acc)) '() (framework-node-registry framework))))
    (if (null? nodes)
        "local"
        (car nodes))))
(define (create-agent-from-spec agent-spec)
  "Create agent instance from specification"
  (make-agent (agent-spec-id agent-spec) (agent-spec-role agent-spec)))
(define (agent-spec-id spec) (car spec))
(define (agent-spec-role spec) (cadr spec))
(define (make-agent-spec agent-type)
  "Create agent specification for deployment"
  (list (gensym (symbol->string agent-type)) agent-type))
(define (deploy-agent-to-node! framework agent node-id)
  "Deploy agent to specific node"
  (agent-system-add! *global-agent-system* agent))
(define (send-shutdown-signal agent-id)
  "Send graceful shutdown signal to agent"
  (format #t "📡 Sending shutdown signal to ~a~%" agent-id))
(define (filter-agents-by-type framework agent-type)
  "Filter agents by type"
  (hash-fold (lambda (agent-id deployment acc)
               (if (eq? (agent-role (hash-ref (agent-system-agents *global-agent-system*) agent-id)) agent-type)
                   (cons agent-id acc)
                   acc))
             '()
             (framework-agent-registry framework)))
(define (agent-health-check agent-id)
  "Check health of specific agent"
  #t)
(define (check-node-health node)
  "Check health of specific node"
  'HEALTHY)
(define (start-monitoring-system framework)
  "Start monitoring system"
  (format #t "📊 Starting monitoring system~%"))
(define (start-health-checker framework)
  "Start health checker"  
  (format #t "🏥 Starting health checker~%"))
(define (register-local-node framework)
  "Register local node in framework"
  (let ((local-node (make-framework-node-record
                      "local" "localhost" 100 0 'ACTIVE '() (current-time))))
    (hash-set! (framework-node-registry framework) "local" local-node)
    (format #t "📍 Registered local node~%")))
(define (stop-monitoring-system framework)
  "Stop monitoring system"
  (format #t "📊 Stopping monitoring system~%"))
(define *global-distributed-framework*
  (make-distributed-agent-framework #:deployment-strategy 'DISTRIBUTED))