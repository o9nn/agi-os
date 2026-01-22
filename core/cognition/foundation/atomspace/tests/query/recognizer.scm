(use-modules (opencog) (opencog exec))
(define star-you
(ListLink
(ConceptNode "I")
(GlobNode "$star")
(ConceptNode "you")))
(BindLink
star-you
(ListLink
(ConceptNode "I")
(GlobNode "$star")
(ConceptNode "you")
(ConceptNode "too")))
(define love-star
(ListLink
(ConceptNode "I")
(ConceptNode "love")
(GlobNode "$star")))
(BindLink
love-star
(ListLink
(ConceptNode "I")
(ConceptNode "like")
(GlobNode "$star")
(ConceptNode "a")
(ConceptNode "lot!")))
(define sent
(ListLink (ConceptNode "I") (ConceptNode "love") (ConceptNode "you")))
(define adv-sent
(ListLink
(ConceptNode "I")
(ConceptNode "really")
(ConceptNode "truly")
(ConceptNode "love")
(ConceptNode "you")))
(define a-hate-b
(ListLink
(GlobNode "$A")
(ConceptNode "hates")
(GlobNode "$B")))
(BindLink
a-hate-b
(ListLink
(ConceptNode "I'm")
(ConceptNode "sure")
(ConceptNode "that")
(GlobNode "$A")
(ConceptNode "hates")
(GlobNode "$B")))
(define hate-speech
(ListLink
(ConceptNode "Mike")
(ConceptNode "really")
(ConceptNode "hates")
(ConceptNode "Sue")
(ConceptNode "a")
(ConceptNode "lot")))
(Implication
(And (Variable "$x") (Concept "B"))
(Concept "C"))
(Implication
(And (Concept "A") (Variable "$x"))
(Concept "C"))
(define a-and-b (And (Concept "A") (Concept "B")))
(List (Concept "A") (Glob "$x"))
(List (Glob "$y") (Concept "B"))
(List (Concept "A") (Glob "$z") (Concept "B"))
(List (Glob "$a") (Concept "A") (Glob "$b") (Concept "B") (Glob "$c"))
(List (Glob "$d") (Concept "A") (Concept "B") (Glob "$e"))
(List (Glob "$f") (Glob "$g") (Concept "A") (Concept "B") (Glob "$h"))
(List (Glob "$i") (Concept "A") (Concept "B") (Glob "$j") (Glob "$k"))
(define ztm (List (Concept "A") (Concept "B")))
*unspecified*