(use-modules (srfi srfi-1))
(use-modules (opencog))
(define dict-mixed (Concept "dict-mixed"))
(for-each
	(lambda (sect) (Member sect dict-mixed))
	(list
		(Section
			(Concept "LEFT-WALL")
			(ConnectorSeq
				(Connector (Concept "Xp") (ConnectorDir "+"))
				(Connector (Concept "WV") (ConnectorDir "+"))
				(Connector (Concept "W") (ConnectorDir "+"))))
		(Section
			(Concept "the")
			(ConnectorSeq
				(Connector (Concept "D") (ConnectorDir "+"))))
		(Section
			(Concept "a")
			(ConnectorSeq
				(Connector (Concept "Ds") (ConnectorDir "+"))))
		(Section
			(Concept "dog")
			(ConnectorSeq
				(Connector (Concept "W") (ConnectorDir "-"))
				(Connector (Concept "D") (ConnectorDir "-"))
				(Connector (Concept "MXs") (ConnectorDir "+"))
				(Connector (Concept "S") (ConnectorDir "+"))))
		(Section
			(Concept "cat")
			(ConnectorSeq
				(Connector (Concept "W") (ConnectorDir "-"))
				(Connector (Concept "D") (ConnectorDir "-"))
				(Connector (Concept "MXs") (ConnectorDir "+"))
				(Connector (Concept "S") (ConnectorDir "+"))))
		(Section
			(Concept ",")
			(ConnectorSeq
				(Connector (Concept "Xd") (ConnectorDir "+"))))
		(Section
			(Concept ",")
			(ConnectorSeq
				(Connector (Concept "Xc") (ConnectorDir "-"))))
		(Section
			(Concept ".")
			(ConnectorSeq
				(Connector (Concept "Xp") (ConnectorDir "-"))
				(Connector (Concept "Xf") (ConnectorDir "-"))))
		(Section
			(Concept "black")
			(ConnectorSeq
				(Connector (Concept "A") (ConnectorDir "+"))))
		(Section
			(Concept "white")
			(ConnectorSeq
				(Connector (Concept "A") (ConnectorDir "+"))))
		(Section
			(Concept "lab")
			(ConnectorSeq
				(Connector (Concept "A") (ConnectorDir "-"))
				(Connector (Concept "Ds") (ConnectorDir "-"))
				(Connector (Concept "Xd") (ConnectorDir "-"))
				(Connector (Concept "MXs") (ConnectorDir "-"))
				(Connector (Concept "Xc") (ConnectorDir "+"))))
		(Section
			(Concept "barked")
			(ConnectorSeq
				(Connector (Concept "WV") (ConnectorDir "-"))
				(Connector (Concept "S") (ConnectorDir "-"))
				(Connector (Concept "Xf") (ConnectorDir "+"))))
		(Section
			(Concept "purred")
			(ConnectorSeq
				(Connector (Concept "WV") (ConnectorDir "-"))
				(Connector (Concept "S") (ConnectorDir "-"))
				(Connector (Concept "Xf") (ConnectorDir "+"))))
))