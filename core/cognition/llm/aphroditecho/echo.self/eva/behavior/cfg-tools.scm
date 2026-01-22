(define (emo-expr-set emo-state expression)
(EvaluationLink
(PredicateNode "Emotion-expression")
(ListLink (ConceptNode emo-state) (ConceptNode expression))))
(define (emo-expr-map emo-state expression param value)
(StateLink (ListLink
(ConceptNode emo-state) (ConceptNode expression) (SchemaNode param))
(NumberNode value)))
(define-public (emo-expr-spec emo-state expression
prob int-min int-max dur-min dur-max)
(emo-expr-set emo-state expression)
(emo-expr-map emo-state expression "probability" prob)
(emo-expr-map emo-state expression "intensity-min" int-min)
(emo-expr-map emo-state expression "intensity-max" int-max)
(emo-expr-map emo-state expression "duration-min" dur-min)
(emo-expr-map emo-state expression "duration-max" dur-max))
(define (emo-gest-set emo-state gesture)
(EvaluationLink
(PredicateNode "Emotion-gesture")
(ListLink (ConceptNode emo-state) (ConceptNode gesture))))
(define (emo-gest-map emo-state gesture param value)
(StateLink (ListLink
(ConceptNode emo-state) (ConceptNode gesture) (SchemaNode param))
(NumberNode value)))
(define-public (emo-gest-spec emo-state gesture prob
int-min int-max rep-min rep-max spd-min spd-max)
(emo-gest-set emo-state gesture)
(emo-gest-map emo-state gesture "gest probability" prob)
(emo-gest-map emo-state gesture "gest intensity-min" int-min)
(emo-gest-map emo-state gesture "gest intensity-max" int-max)
(emo-gest-map emo-state gesture "repeat-min" rep-min)
(emo-gest-map emo-state gesture "repeat-max" rep-max)
(emo-gest-map emo-state gesture "speed-min" spd-min)
(emo-gest-map emo-state gesture "speed-max" spd-max))
(define-public (dice-roll action probability)
(define prob-name (string-append action " probability"))
(State (Schema prob-name) (Number probability))
(DefineLink
(DefinedPredicateNode (string-append "dice-roll: " action))
(GreaterThan
(Get
(TypedVariable (Variable "$x") (Type "NumberNode"))
(State (Schema prob-name) (Variable "$x")))
(RandomNumber (Number 0) (Number 1)))))