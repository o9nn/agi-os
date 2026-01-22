(use-modules (opencog))
(ConceptNode "asdf")
(Concept "foo")
(Concept "foo")
(ListLink (Concept "foo") (Concept "bar"))
(ListLink (Concept "foo") (ListLink (Concept "bar") (Concept "bar")))
(ListLink (Concept "foo") (Concept "bar"))
(cog-node 'ConceptNode "asdf")
(cog-node 'ConceptNode "qwerty")
(cog-link 'ListLink (cog-node 'ConceptNode "Oh no!"))
(define f (Concept "foo"))
f
(format #t "Let us print it out: ~A\n" f)
(define fff (ListLink f f f))
fff
(format #t "Here is a bunch: ~A\n" fff)
(symbol? 'foo)
(symbol? "bar")
(cog-get-types)
(FloatValue 0 1 2 3.14159)
(cog-set-value! (Concept "asdf") (Predicate "some key") (FloatValue 4 5 6))
(cog-value (Concept "asdf") (Predicate "some key"))
(use-modules (opencog exec))
(cog-execute! (ValueOf (Concept "asdf") (Predicate "some key")))
(cog-execute! (SetValue
(Concept "asdf") (Predicate "some key")
(Node "this is the new thing")))
(cog-execute! (ValueOf (Concept "asdf") (Predicate "some key")))
,apropos cog
,a cog
,describe cog-new-node
,describe cog-node
,d cog-link
,d ConceptNode