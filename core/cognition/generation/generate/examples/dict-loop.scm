(use-modules (srfi srfi-1))
(use-modules (opencog))
(define dict-loop (Concept "dict-loop"))
(for-each
(lambda (sect) (Member sect dict-loop))
(list
(Section
(Concept "LEFT-WALL")
(ConnectorSeq
(Connector (Concept "WV") (ConnectorDir "+"))
(Connector (Concept "W") (ConnectorDir "+"))))
(Section
(Concept "John")
(ConnectorSeq
(Connector (Concept "W") (ConnectorDir "-"))
(Connector (Concept "S") (ConnectorDir "+"))))
(Section
(Concept "Mary")
(ConnectorSeq
(Connector (Concept "W") (ConnectorDir "-"))
(Connector (Concept "S") (ConnectorDir "+"))))
(Section
(Concept "saw")
(ConnectorSeq
(Connector (Concept "S") (ConnectorDir "-"))
(Connector (Concept "WV") (ConnectorDir "-"))
(Connector (Concept "O") (ConnectorDir "+"))))
(Section
(Concept "a")
(ConnectorSeq
(Connector (Concept "D") (ConnectorDir "+"))))
(Section
(Concept "cat")
(ConnectorSeq
(Connector (Concept "D") (ConnectorDir "-"))
(Connector (Concept "O") (ConnectorDir "-"))))
(Section
(Concept "dog")
(ConnectorSeq
(Connector (Concept "D") (ConnectorDir "-"))
(Connector (Concept "O") (ConnectorDir "-"))))
))