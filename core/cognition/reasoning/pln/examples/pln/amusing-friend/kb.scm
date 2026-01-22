(Predicate "is-honest" (stv 0.8 0.9))
(Lambda (stv 0.64 0.9)
(VariableList
(TypedVariable
(Variable "$X")
(Type "ConceptNode"))
(TypedVariable
(Variable "$Y")
(Type "ConceptNode")))
(And
(Evaluation
(Predicate "is-honest")
(Variable "$X"))
(Evaluation
(Predicate "is-honest")
(Variable "$Y"))))
(Predicate "told-the-truth" (stv 0.00001 0.7))
(Evaluation (stv 0.00001 0.7)
(Predicate "told-the-truth-about")
(List
(Variable "$X")
(Variable "$Y")
(Variable "$Z")))
(define people-telling-the-truth-are-honest
(ImplicationScope (stv 0.95 0.9)
(VariableList
(TypedVariable
(Variable "$X")
(Type "ConceptNode"))
(TypedVariable
(Variable "$Y")
(Type "ConceptNode"))
(TypedVariable
(Variable "$Z")
(Type "ConceptNode")))
(Evaluation
(Predicate "told-the-truth-about")
(List
(Variable "$X")
(Variable "$Y")
(Variable "$Z")))
(Evaluation
(Predicate "is-honest")
(Variable "$X"))))
(Lambda (stv 0.0002 0.9)
(VariableList
(TypedVariable
(Variable "$X")
(Type "ConceptNode"))
(TypedVariable
(Variable "$Y")
(Type "ConceptNode")))
(And
(Inheritance
(Variable "$X")
(Concept "human"))
(Inheritance
(Variable "$Y")
(Concept "human"))
(Evaluation
(Predicate "acquainted")
(List
(Variable "$X")
(Variable "$Y")))))
(Inheritance (stv 1 1)
(Concept "Bob")
(Concept "human"))
(Inheritance (stv 1 1)
(Concept "Self")
(Concept "human"))
(Evaluation (stv 0.9 0.9)
(Predicate "is-honest")
(Concept "Self"))
(Evaluation (stv 1 1)
(Predicate "acquainted")
(List
(Concept "Self")
(Concept "Bob")))
(Predicate "will-be-friends" (stv 0.0001 0.9))
(Lambda
(VariableList
(TypedVariable
(Variable "$X")
(Type "ConceptNode"))
(TypedVariable
(Variable "$Y")
(Type "ConceptNode")))
(Evaluation
(Predicate "will-be-friends")
(List
(Variable "$X")
(Variable "$Y"))))
(ImplicationScope (stv 1 1)
(VariableList
(TypedVariable
(Variable "$X")
(Type "ConceptNode"))
(TypedVariable
(Variable "$Y")
(Type "ConceptNode")))
(Evaluation
(Predicate "will-be-friends")
(List
(Variable "$X")
(Variable "$Y")))
(Evaluation
(Predicate "will-be-friends")
(List
(Variable "$Y")
(Variable "$X"))))
(define human-acquainted-tend-to-become-friends
(ImplicationScope (stv 0.1 0.5)
(VariableList
(TypedVariable
(Variable "$X")
(Type "ConceptNode"))
(TypedVariable
(Variable "$Y")
(Type "ConceptNode")))
(And
(Inheritance
(Variable "$X")
(Concept "human"))
(Inheritance
(Variable "$Y")
(Concept "human"))
(Evaluation
(Predicate "acquainted")
(List
(Variable "$X")
(Variable "$Y"))))
(Evaluation
(Predicate "will-be-friends")
(List
(Variable "$X")
(Variable "$Y")))))
(define friends-tend-to-be-honest
(ImplicationScope (stv 0.85 0.5)
(VariableList
(TypedVariable
(Variable "$X")
(Type "ConceptNode"))
(TypedVariable
(Variable "$Y")
(Type "ConceptNode")))
(Evaluation
(Predicate "will-be-friends")
(List
(Variable "$X")
(Variable "$Y")))
(And
(Evaluation
(Predicate "is-honest")
(Variable "$X"))
(Evaluation
(Predicate "is-honest")
(Variable "$Y")))))
(Predicate "told-a-joke-at" (stv 0.000001 0.6))
(Evaluation (stv 0.000001 0.6)
(Predicate "told-a-joke-at")
(List
(Variable "$X")
(Variable "$Y")
(Variable "$Z")))
(Predicate "is-funny" (stv 0.69 0.7))
(Evaluation (stv 0.69 0.7)
(Predicate "is-funny")
(Variable "$X"))
(define people-telling-jokes-are-funny
(ImplicationScope (stv 0.8 0.9)
(VariableList
(TypedVariable
(Variable "$X")
(Type "ConceptNode"))
(TypedVariable
(Variable "$Y")
(Type "ConceptNode"))
(TypedVariable
(Variable "$Z")
(Type "ConceptNode")))
(Evaluation
(Predicate "told-a-joke-at")
(List
(Variable "$X")
(Variable "$Y")
(Variable "$Z")))
(Evaluation
(Predicate "is-funny")
(Variable "$X"))))
(define funny-is-loosely-equivalent-to-amusing
(Equivalence (stv 0.7 0.9)
(TypedVariable
(Variable "$X")
(Type "ConceptNode"))
(Evaluation
(Predicate "is-funny")
(Variable "$X"))
(Evaluation
(Predicate "is-amusing")
(Variable "$X"))))
(Evaluation (stv 1 1)
(Predicate "told-the-truth-about")
(List
(Concept "Bob")
(Concept "Jill")
(Concept "Party")))
(Evaluation (stv 1 1)
(Predicate "told-a-joke-at")
(List
(Concept "Bob")
(Concept "Jim")
(Concept "Party")))
(define hack (And (stv 1 0.9)
(Evaluation
(Predicate "is-honest")
(Concept "Self")
)
(Evaluation
(Predicate "is-honest")
(Concept "Bob")
)
(Inheritance
(Concept "Self")
(Concept "human")
)
(Inheritance
(Concept "Bob")
(Concept "human")
)
(Evaluation
(Predicate "acquainted")
(List
(Concept "Self")
(Concept "Bob")
)
)
)
)
(And (stv 0.000128 0.89999998)
(Evaluation
(Predicate "is-honest")
(Variable "$X")
)
(Evaluation
(Predicate "is-honest")
(Variable "$Y")
)
(Inheritance
(Variable "$X")
(Concept "human")
)
(Inheritance
(Variable "$Y")
(Concept "human")
)
(Evaluation
(Predicate "acquainted")
(List
(Variable "$X")
(Variable "$Y")
)
)
)
(Lambda
(VariableList
(TypedVariable
(Variable "$X")
(Type "ConceptNode")
)
(TypedVariable
(Variable "$Y")
(Type "ConceptNode")
)
)
(And
(Evaluation
(Predicate "is-honest")
(Variable "$X")
)
(Evaluation
(Predicate "is-honest")
(Variable "$Y")
)
(Inheritance
(Variable "$X")
(Concept "human")
)
(Inheritance
(Variable "$Y")
(Concept "human")
)
(Evaluation
(Predicate "acquainted")
(List
(Variable "$X")
(Variable "$Y")
)
)
)
)