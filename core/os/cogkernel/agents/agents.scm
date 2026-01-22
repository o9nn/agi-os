(define-module (cogkernel agents)
#:use-module (ice-9 hash-table)
#:use-module (ice-9 match)
#:use-module (ice-9 threads)
#:use-module (srfi srfi-1)
#:use-module (srfi srfi-9)
#:use-module (cogkernel atomspace)
#:export (make-agent
agent?
agent-id
agent-role
agent-actions
agent-environment
agent-state
agent-execute!
agent-register-action!
make-action
action?
action-type
action-procedure
make-agent-system
agent-system-add!
agent-system-get
agent-system-execute-all!
agent-system-tensor-shape
*global-agent-system*))
(define agent-roles
'(MONITOR
REPAIR
BUILD
ANALYZE
OPTIMIZE
AUDIT
META
SYNTHESIZE))
(define agent-states
'(IDLE ACTIVE BLOCKED COMPLETED ERROR))
(define action-types
'(DETECT ANALYZE REPAIR BUILD TEST OPTIMIZE AUDIT SYNTHESIZE LEARN))
(define-record-type <agent>
(make-agent-record id role actions environment state tensor-coords last-execution)
agent?
(id agent-id)
(role agent-role)
(actions agent-actions set-agent-actions!)
(environment agent-environment set-agent-environment!)
(state agent-state set-agent-state!)
(tensor-coords agent-tensor-coords set-agent-tensor-coords!)
(last-execution agent-last-execution set-agent-last-execution!))
(define-record-type <action>
(make-action-record type procedure condition priority)
action?
(type action-type)
(procedure action-procedure)
(condition action-condition)
(priority action-priority))
(define-record-type <agent-system>
(make-agent-system-record agents scheduler tensor-dims)
agent-system?
(agents agent-system-agents)
(scheduler agent-system-scheduler set-agent-system-scheduler!)
(tensor-dims agent-system-tensor-dims))
(define* (make-agent id role #:optional (environment '()) (tensor-coords '(0 0 0 0)))
"Create a new agent with specified id and role"
(unless (member role agent-roles)
(error "Unknown agent role" role))
(make-agent-record id role '() environment 'IDLE tensor-coords #f))
(define* (make-action type procedure #:optional (condition (const #t)) (priority 1))
"Create a new action with specified type and procedure"
(unless (member type action-types)
(error "Unknown action type" type))
(make-action-record type procedure condition priority))
(define* (make-agent-system #:optional (tensor-dims '(100 8 10 4)))
"Create a new agent system with specified tensor dimensions"
(make-agent-system-record (make-hash-table) #f tensor-dims))
(define (agent-register-action! agent action)
"Register an action with an agent"
(set-agent-actions! agent (cons action (agent-actions agent))))
(define (agent-execute! agent action-type . args)
"Execute an action of specified type for the agent"
(set-agent-state! agent 'ACTIVE)
(let ((actions (filter (lambda (action)
(eq? (action-type action) action-type))
(agent-actions agent))))
(if (null? actions)
(begin
(set-agent-state! agent 'ERROR)
#f)
(let* ((action (car actions))
(result (apply (action-procedure action) args)))
(set-agent-state! agent 'COMPLETED)
(set-agent-last-execution! agent (current-time))
result))))
(define (agent-system-add! system agent)
"Add an agent to the agent system"
(hash-set! (agent-system-agents system) (agent-id agent) agent))
(define (agent-system-get system agent-id)
"Get an agent by id from the system"
(hash-ref (agent-system-agents system) agent-id))
(define (agent-system-execute-all! system)
"Execute all agents in the system"
(hash-for-each (lambda (id agent)
(when (eq? (agent-state agent) 'IDLE)
(set-agent-state! agent 'ACTIVE)))
(agent-system-agents system)))
(define (agent-system-tensor-shape system)
"Get the tensor shape representing this agent system"
(let ((num-agents (hash-count (const #t) (agent-system-agents system))))
(list num-agents
(length agent-roles)
(length action-types)
(length agent-states))))
(define *global-agent-system* (make-agent-system))