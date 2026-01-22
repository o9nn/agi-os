(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog exec))
(define b1
(Section
(Concept "burr-1")
(ConnectorSeq
(Connector (Concept "E") (ConnectorDir "*")))))
(define b2
(Section
(Concept "burr-2")
(ConnectorSeq
(Connector (Concept "E") (ConnectorDir "*"))
(Connector (Concept "E") (ConnectorDir "*")))))
(define b3
(Section
(Concept "burr-3")
(ConnectorSeq
(Connector (Concept "E") (ConnectorDir "*"))
(Connector (Concept "E") (ConnectorDir "*"))
(Connector (Concept "E") (ConnectorDir "*")))))
(define b4
(Section
(Concept "burr-4")
(ConnectorSeq
(Connector (Concept "E") (ConnectorDir "*"))
(Connector (Concept "E") (ConnectorDir "*"))
(Connector (Concept "E") (ConnectorDir "*"))
(Connector (Concept "E") (ConnectorDir "*")))))
(define b5
(Section
(Concept "burr-5")
(ConnectorSeq
(Connector (Concept "E") (ConnectorDir "*"))
(Connector (Concept "E") (ConnectorDir "*"))
(Connector (Concept "E") (ConnectorDir "*"))
(Connector (Concept "E") (ConnectorDir "*"))
(Connector (Concept "E") (ConnectorDir "*")))))
(define b6
(Section
(Concept "burr-6")
(ConnectorSeq
(Connector (Concept "E") (ConnectorDir "*"))
(Connector (Concept "E") (ConnectorDir "*"))
(Connector (Concept "E") (ConnectorDir "*"))
(Connector (Concept "E") (ConnectorDir "*"))
(Connector (Concept "E") (ConnectorDir "*"))
(Connector (Concept "E") (ConnectorDir "*")))))
(define weights (PredicateNode "weights"))
(cog-set-value! b1 weights (FloatValue 1))
(cog-set-value! b2 weights (FloatValue (/ 1.0 2)))
(cog-set-value! b3 weights (FloatValue (/ 1.0 3)))
(cog-set-value! b4 weights (FloatValue (/ 1.0 4)))
(cog-set-value! b5 weights (FloatValue (/ 1.0 5)))
(cog-set-value! b6 weights (FloatValue (/ 1.0 6)))
(define lexis (Concept "six burrs"))
(Member b1 lexis)
(Member b2 lexis)
(Member b3 lexis)
(Member b4 lexis)
(Member b5 lexis)
(Member b6 lexis)
(define polarity-set (Concept "any to any"))
(Member (Set (ConnectorDir "*") (ConnectorDir "*")) polarity-set)
*unspecified*