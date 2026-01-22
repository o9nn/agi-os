(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog exec))
(define left-wall (Concept "LEFT-WALL"))
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
(Concept "John")
(ConnectorSeq
(Connector (Concept "Ce") (ConnectorDir "-"))
(Connector (Concept "S") (ConnectorDir "+"))))
(Section
(Concept "Mary")
(ConnectorSeq
(Connector (Concept "Ce") (ConnectorDir "-"))
(Connector (Concept "S") (ConnectorDir "+"))))
(Section
(Concept "thinks")
(ConnectorSeq
(Connector (Concept "S") (ConnectorDir "-"))
(Connector (Concept "WV") (ConnectorDir "-"))
(Connector (Concept "CV") (ConnectorDir "+"))
(Connector (Concept "Ce") (ConnectorDir "+"))))
(Section
(Concept "saw")
(ConnectorSeq
(Connector (Concept "S") (ConnectorDir "-"))
(Connector (Concept "CV") (ConnectorDir "-"))
(Connector (Concept "O") (ConnectorDir "+"))))
(Section
(Concept "a")
(ConnectorSeq
(Connector (Concept "D") (ConnectorDir "+"))))
(Section
(Concept "bird")
(ConnectorSeq
(Connector (Concept "D") (ConnectorDir "-"))
(Connector (Concept "O") (ConnectorDir "-"))))