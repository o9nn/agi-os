(use-modules (opencog) (opencog exec) (opencog sensory))
(define txt-stream
(cog-execute! (FileReadNode "file:///tmp/demo.txt")))
txt-stream
txt-stream
txt-stream
txt-stream
txt-stream
(cog-set-value! (Concept "foo") (Predicate "some place")
(cog-execute! (FileReadNode "file:///tmp/demo.txt")))
(cog-execute!
(SetValue (Concept "foo") (Predicate "some place")
(FileRead "file:///tmp/demo.txt")))
(define txt-stream-gen
(ValueOf (Concept "foo") (Predicate "some place")))
(cog-execute! txt-stream-gen)
(cog-execute! txt-stream-gen)
(cog-execute! txt-stream-gen)
(cog-execute! txt-stream-gen)
(cog-execute! txt-stream-gen)
(use-modules (opencog nlp) (opencog nlp lg-parse))
(cog-execute!
(SetValue (Concept "foo") (Predicate "some place")
(FileRead "file:///tmp/demo.txt")))
(cog-execute! (LgParseBonds txt-stream-gen (Number 1) (LgDict "any")))
(cog-execute! (LgParseBonds txt-stream-gen (Number 1) (LgDict "any")))
(cog-execute! (LgParseBonds txt-stream-gen (Number 1) (LgDict "any")))