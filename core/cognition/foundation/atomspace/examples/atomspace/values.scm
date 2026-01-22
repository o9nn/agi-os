(use-modules (opencog))
(define f (FloatValue 0.1 0.2 3.3 4.5678))
(define s (StringValue "asdf" "gh" "jkl;"))
(define l (LinkValue
(Concept "foobar") (StringValue "property") (FloatValue 42)))
(cog-value->list f)
(cog-value->list s)
(cog-value->list l)
(cog-value-ref f 2)
(cog-value-ref s 0)
(cog-value-ref l 1)
,d cog-value->list
,d cog-value-ref
(define a (Concept "some atom"))
(define k1 (PredicateNode "first key"))
(cog-set-value! a k1 f)
(cog-value a k1)
(cog-set-value! a k1 l)
(cog-value a k1)
(define k2 (PredicateNode "second key"))
(cog-set-value! a k2 s)
(cog-value a k2)
(cog-value a k1)
(cog-keys a)
(cog-set-tv! a (stv 0.9 0.8))
(cog-keys a)
(define ktv (PredicateNode "*-TruthValueKey-*"))
(cog-value a ktv)
(cog-tv a)
(equal? (cog-value a ktv) (cog-tv a))
(use-modules (opencog attention-bank))
(define l2 (LinkValue
(stv 0.1 0.2) (stv 0.3 0.4) (Concept "foobar") (av 3 2 1) (av 4 5 0)))
(cog-set-value! a k2 l2)
(cog-value a k2)
(cog-set-av! a (av 3 2 1))
(cog-keys a)
(define kav (PredicateNode "*-AttentionValueKey-*"))
(cog-value a kav)
(cog-av a)
(equal? (cog-value a kav) (cog-av a))
(cog-set-value! (Concept "Fido the Dog")
(Predicate "weight_in_kg") (FloatValue 12.5))
(use-modules (opencog exec))
(cog-execute!
(Get
(GreaterThan
(FloatValueOf (Variable "dog_node") (Predicate "weight_in_kg"))
(Number "10"))))
(Member (Concept "Fido the Dog") (Concept "things that have weight"))
(cog-execute!
(Get
(And
(Member (Variable "dog_node") (Concept "things that have weight"))
(GreaterThan
(FloatValueOf (Variable "dog_node") (Predicate "weight_in_kg"))
(Number "10")))))
(Member (Concept "Fido the Dog") (Concept "things that weigh more than 10 kg"))