(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog exec))
(use-modules (opencog generate))
(use-modules (ice-9 textual-ports))
(define susceptible (Concept "susceptible"))
(define exposed (Concept "exposed"))
(define infected (Concept "infected"))
(define recovered (Concept "recovered"))
(define died (Concept "died"))
(define seir-state (Predicate "SEIR state"))
(define susceptibility (Predicate "Susceptibility weight"))
(define infirmity (Predicate "Infirmity weight"))
(define recovery (Predicate "Recovery weight"))
(Define
(DefinedSchema "transmission")
(Lambda
(VariableList (Variable "$A") (Variable "$B") (Variable "$REL"))
(Cond
(And
(Equal (ValueOf (Variable "$A") seir-state) susceptible)
(Equal (ValueOf (Variable "$B") seir-state) infected)
(Or
(And
(Equal (Variable "$REL") (Concept "friend"))
(GreaterThan
(RandomNumber (Number 0) (Number 1))
(Number 0.3)))
(And
(Equal (Variable "$REL") (Concept "stranger"))
(GreaterThan
(RandomNumber (Number 0) (Number 1))
(Number 0.7)))))
(SetValue (Variable "$A") seir-state exposed)
)))
(Define
(DefinedSchema "state transition")
(Lambda
(Variable "$A")
(Cond
(And
(Equal (ValueOf (Variable "$A") seir-state) exposed)
(GreaterThan
(ValueOf (Variable "$A") susceptibility)
(RandomNumber (Number 0) (Number 1))))
(SetValue (Variable "$A") seir-state infected)
(And
(Equal (ValueOf (Variable "$A") seir-state) exposed)
(GreaterThan
(Minus (Number 1)
(ValueOf (Variable "$A") susceptibility))
(RandomNumber (Number 0) (Number 1))))
(SetValue (Variable "$A") seir-state susceptible)
(And
(Equal (ValueOf (Variable "$A") seir-state) infected)
(GreaterThan
(ValueOf (Variable "$A") infirmity)
(RandomNumber (Number 0) (Number 1))))
(SetValue (Variable "$A") seir-state died)
(And
(Equal (ValueOf (Variable "$A") seir-state) infected)
(GreaterThan
(ValueOf (Variable "$A") recovery)
(RandomNumber (Number 0) (Number 1))))
(SetValue (Variable "$A") seir-state recovered)
))
)
(define (condition CURRENT-STATE NEXT-STATE DISTRIBUTION)
(list
(And
(Equal (ValueOf (Variable "$A") seir-state) CURRENT-STATE)
(GreaterThan
(ValueOf (Variable "$A") DISTRIBUTION)
(RandomNumber (Number 0) (Number 1))))
(SetValue (Variable "$A") seir-state NEXT-STATE)))
(define (inverted CURRENT-STATE NEXT-STATE DISTRIBUTION)
(list
(And
(Equal (ValueOf (Variable "$A") seir-state) CURRENT-STATE)
(GreaterThan
(Minus (Number 1) (ValueOf (Variable "$A") DISTRIBUTION))
(RandomNumber (Number 0) (Number 1))))
(SetValue (Variable "$A") seir-state NEXT-STATE)))
(Define
(DefinedSchema "alt version of state transition")
(Lambda
(Variable "$A")
(Cond
(condition exposed  infected    susceptibility)
(inverted  exposed  susceptible susceptibility)
(condition infected died        infirmity)
(condition infected recovered   recovery))))
(define node-weight (Predicate "node likelihood"))
(define prototypes (Concept "Prototype Individual Anchor Point"))
(define (make-person-type Nfriends Nstrangers)
(define label (format #f "person-~D-~D" Nfriends Nstrangers))
(Section
(Concept label)
(ConnectorSeq
(make-list Nfriends
(Connector (Concept "friend") (ConnectorDir "*")))
(make-list Nstrangers
(Connector (Concept "stranger") (ConnectorDir "*"))))))
(for-each (lambda (num-friends)
(for-each (lambda (num-strangers)
(define person-type (make-person-type num-friends num-strangers))
(define weight (/ 1.0
(* (+ num-friends num-strangers) num-friends num-strangers)))
(cog-set-value! person-type node-weight (FloatValue weight))
(Member person-type prototypes)
)
(iota 8 3)))
(iota 6 1))
(define pole-set (Concept "any to any"))
(Member (Set (ConnectorDir "*") (ConnectorDir "*")) pole-set)
(define max-solutions (Predicate "*-max-solutions-*"))
(define close-fraction (Predicate "*-close-fraction-*"))
(define max-steps (Predicate "*-max-steps-*"))
(define max-depth (Predicate "*-max-depth-*"))
(define max-network-size (Predicate "*-max-network-size-*"))
(define point-set-anchor (Predicate "*-point-set-anchor-*"))
(define params (Concept "Simple Covid net parameters"))
(State (Member max-solutions params) (Number 12))
(State (Member max-solutions params) (Number 1))
(State (Member close-fraction params) (Number 1.0))
(State (Member max-steps params) (Number 345))
(State (Member max-depth params) (Number 100))
(State (Member max-network-size params) (Number 2000))
(define anchor (Anchor "Covid Sim Individuals"))
(State (Member point-set-anchor params) anchor)
(define seed (gar (make-person-type 2 3)))
(format #t "Start creating the network!\n")
(define start-time (get-internal-real-time))
(define (try-to-make-network)
(define net
(cog-random-aggregate pole-set prototypes node-weight params seed))
(if (< 0 (cog-arity net)) net
(try-to-make-network)))
(define network-set (try-to-make-network))
(define end-time (get-internal-real-time))
(format #t "Created ~D networks in ~6F seconds\n"
(cog-arity network-set)
(* 1.0e-9 (- end-time start-time)))
(define just-one (Set (gar network-set)))
(define just-one-gml (export-to-gml just-one))
(let ((outport (open-file "/tmp/social-network.gml" "w")))
(put-string outport just-one-gml)
(close outport))
(format #t "Found a network of ~D individuals\n" (cog-arity (gar just-one)))
(define (exec-unwrap ATOMESE)
(define set-link (cog-execute! ATOMESE))
(define contents (cog-outgoing-set set-link))
(cog-delete set-link)
contents)
(define (initialize-state)
(exec-unwrap
(Bind
(TypedVariable (Variable "$person") (Type "ConceptNode"))
(Present (Member (Variable "$person") anchor))
(SetValue (Variable "$person") seir-state susceptible)
(SetValue (Variable "$person") susceptibility
(RandomNumber (Number 0.2) (Number 0.8)))
(SetValue (Variable "$person") infirmity
(RandomNumber (Number 0.01) (Number 0.55)))
(SetValue (Variable "$person") recovery
(RandomNumber (Number 0.6) (Number 0.95)))
))
*unspecified*)
(initialize-state)
(define (get-relations-of-type RELATION)
(exec-unwrap
(Get
(TypedVariable (Variable "$pair") (Type "SetLink"))
(Present (Evaluation RELATION (Variable "$pair"))))))
(format #t "The social network consists of ~D friend-pairs\n"
(length (get-relations-of-type (Concept "friend"))))
(format #t "The social network consists of ~D stranger-pairs\n"
(length (get-relations-of-type (Concept "stranger"))))
(define (get-individuals-in-state STATE)
(exec-unwrap
(Get
(TypedVariable (Variable "$indiv") (Type "ConceptNode"))
(And
(Present (Member (Variable "$indiv") anchor))
(Equal (ValueOf (Variable "$indiv") seir-state) STATE)))))
(define all-individuals (get-individuals-in-state susceptible))
(define (report-stats)
(format #t
"Exposed: ~D    Infected: ~D   Recovered: ~D  Died: ~D  of Total: ~D\n"
(length (get-individuals-in-state exposed))
(length (get-individuals-in-state infected))
(length (get-individuals-in-state recovered))
(length (get-individuals-in-state died))
(length all-individuals))
*unspecified*)
(define (do-transmission)
(exec-unwrap
(Bind
(VariableList
(TypedVariable (Variable "$pers-a") (Type "ConceptNode"))
(TypedVariable (Variable "$pers-b") (Type "ConceptNode"))
(TypedVariable (Variable "$relation") (Type "ConceptNode")))
(Present
(Evaluation
(Variable "$relation")
(Set (Variable "$pers-a") (Variable "$pers-b"))))
(Put (DefinedSchema "transmission")
(List
(Variable "$pers-a")
(Variable "$pers-b")
(Variable "$relation")))))
*unspecified*)
(define (do-state-transition)
(exec-unwrap
(Bind
(TypedVariable (Variable "$person") (Type "ConceptNode"))
(Present (Member (Variable "$person") anchor))
(Put (DefinedSchema "state transition")
(Variable "$person"))))
*unspecified*)
(define everyone (get-individuals-in-state susceptible))
(define one-person (first everyone))
(cog-execute! (SetValue one-person seir-state infected))
(do-transmission)     (report-stats)
(do-state-transition) (report-stats)
(do-transmission)     (report-stats)
(do-state-transition) (report-stats)
(define (loop)
(do-transmission)     (report-stats)
(do-state-transition) (report-stats)
(if (and
(= 0 (length (get-individuals-in-state exposed)))
(= 0 (length (get-individuals-in-state infected))))
(format #t "Finished simulation\n")
(loop)))
(display "Now say `(loop)` to run the rest of the simulation automatically\n")
*unspecified*