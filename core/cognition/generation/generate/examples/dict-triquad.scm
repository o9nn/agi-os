(use-modules (srfi srfi-1))
(use-modules (opencog))
(define dict-triquad (Concept "dict-triquad"))
(for-each
(lambda (sect) (Member sect dict-triquad))
(list
(Section
(Concept "LEFT-WALL")
(ConnectorSeq
(Connector (Concept "Xp") (ConnectorDir "+"))
(Connector (Concept "WV") (ConnectorDir "+"))
(Connector (Concept "W") (ConnectorDir "+"))))
(Section
(Concept "Mary")
(ConnectorSeq
(Connector (Concept "W") (ConnectorDir "-"))
(Connector (Concept "S") (ConnectorDir "+"))))
(Section
(Concept "John")
(ConnectorSeq
(Connector (Concept "Ce") (ConnectorDir "-"))
(Connector (Concept "S") (ConnectorDir "+"))))
(Section
(Concept "think")
(ConnectorSeq
(Connector (Concept "I") (ConnectorDir "-"))
(Connector (Concept "WV") (ConnectorDir "-"))
(Connector (Concept "CV") (ConnectorDir "+"))
(Connector (Concept "Ce") (ConnectorDir "+"))))
(Section
(Concept "fall")
(ConnectorSeq
(Connector (Concept "I") (ConnectorDir "-"))
(Connector (Concept "CV") (ConnectorDir "-"))
(Connector (Concept "Xc") (ConnectorDir "+"))))
(Section
(Concept "might")
(ConnectorSeq
(Connector (Concept "S") (ConnectorDir "-"))
(Connector (Concept "I") (ConnectorDir "+"))))
(Section
(Concept "could")
(ConnectorSeq
(Connector (Concept "S") (ConnectorDir "-"))
(Connector (Concept "I") (ConnectorDir "+"))))
(Section
(Concept ".")
(ConnectorSeq
(Connector (Concept "Xp") (ConnectorDir "-"))
(Connector (Concept "Xc") (ConnectorDir "-"))))
))