(use-modules (opencog) (opencog exec))
(State (Anchor "fruit") (Concept "apple"))
(cog-incoming-set (Anchor "fruit"))
(State (Anchor "fruit") (Concept "banana"))
(cog-incoming-set (Anchor "fruit"))
(State (Anchor "fruit") (Concept "apple"))
(cog-incoming-set (Anchor "fruit"))
(cog-execute! (Get (State (Anchor "fruit") (Variable "$x"))))
(cog-incoming-set (Anchor "fruit"))
(cog-evaluate!
(EqualLink
(Set (Concept "apple"))
(Get (State (Anchor "fruit") (Variable "$x")))))
(cog-evaluate!
(EqualLink
(Set (Concept "banana"))
(Get (State (Anchor "fruit") (Variable "$x")))))
(cog-execute!
(Put
(State (Anchor "fruit") (Variable "$x"))
(Concept "strawberry")))
(cog-execute!
(Get (State (Anchor "fruit") (Variable "$x"))))
(Evaluation (Predicate "fruit") (List (Concept "apple")))
(Evaluation (Predicate "fruit") (List (Concept "banana")))
(Evaluation (Predicate "fruit") (List (Concept "strawberry")))
(cog-execute! (Get (Evaluation (Predicate "fruit") (Variable "$x"))))
(Evaluation (Predicate "Is A") (List (Concept "fruit") (Concept "apple")))
(Evaluation (Predicate "Is A") (List (Concept "fruit") (Concept "banana")))
(Evaluation (Predicate "Is A") (List (Concept "fruit") (Concept "strawberry")))
(cog-execute! (Get
(Evaluation (Predicate "Is A") (List (Concept "fruit") (Variable "$x")))))
(Inheritance (Concept "fruit") (Concept "apple"))
(Inheritance (Concept "fruit") (Concept "banana"))
(Inheritance (Concept "fruit") (Concept "strawberry"))
(cog-execute! (Get (Inheritance (Concept "fruit") (Variable "$x"))))
(Member (Concept "apple")     (Concept "fruit"))
(Member (Concept "banana")   (Concept "fruit"))
(Member (Concept "strawberry") (Concept "fruit"))
(cog-execute! (Get (Member (Variable "$x") (Concept "fruit"))))