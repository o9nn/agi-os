(use-modules (srfi srfi-1))
(use-modules (opencog persist))
(define-public (get-germ-sections GERM)
"
  get-germ-sections GERM - return all sections that the germ is
  at the center of.
  Assumes that the sections for the germ are already in the atomspace.
  Use `fetch-germ-sections` to load them from storage.
"
	(cog-incoming-by-type GERM 'Section)
)
(define-public (fetch-germ-sections GERM)
"
  fetch-germ-sections GERM - return all sections that the germ is
  at the center of.
  Fetches the sections from storage
  yet.  Use `get-germ-sections` if fetching is not needed.
"
	(fetch-incoming-by-type GERM 'Section)
	(cog-incoming-by-type GERM 'Section)
)
(define-public (get-germ-connector-seqs GERM)
"
  get-germ-connector-seqs GERM - return all connector sequences
  that appear in sections on the GERM. There is one connector sequence
  per section.
  Assumes that the sections for the germ are already in the atomspace.
  These can be loaded by saying (fetch-germ-sections GERM)
"
	(map
		(lambda (SEC) (cog-outgoing-atom SEC 1))
		(get-germ-sections GERM))
)
(define-public (get-germ-connectors GERM)
"
  get-germ-connectors GERM - return all connectors that appear in
  the connector sequences of (all) sections on the GERM.
  Assumes that the sections for the germ are already in the atomspace.
  These can be loaded by saying (fetch-incoming-by-type GERM 'Section)
"
	(delete-dup-atoms
		(concatenate!
			(map cog-outgoing-set
				(get-germ-connector-seqs GERM))))
)
(define-public (get-germ-endpoints GERM)
"
  get-germ-endpoints GERM - return all vertexes that appear as
  endpoints in the connector sets on the GERM.
  Assumes that the sections for the germ are already in the atomspace.
  These can be loaded by saying (fetch-incoming-by-type GERM 'Section)
"
	(delete-dup-atoms
		(map
			(lambda (CNCTR) (cog-outgoing-atom CNCTR 0))
			(get-germ-connectors GERM)))
)
(define-public (get-conseq-sections CONSEQ)
"
  get-conseq-sections CONSEQ - return all sections that have this
  connector sequence in them.
  Assumes that all sections are already in the atomspace
  `fetch-conseq-sections` instead.
"
	(cog-incoming-by-type CONSEQ 'Section)
)
(define-public (fetch-conseq-sections CONSEQ)
"
  fetch-conseq-sections CONSEQ - return all sections that have this
  connector sequence in them.
  Fetches sections from storage (does not assume they have been loaded
  yet). Use 'get-conseq-sections` if fetching is not needed.
"
	(fetch-incoming-by-type CONSEQ 'Section)
	(get-conseq-sections CONSEQ)
)
(define-public (get-connector-sections CNCTR)
"
  get-connector-sections CONNECTOR - return all sections that have
  this connector appearing in their connector sequence.
  Assumes that all connector sequences and sections are already in
  the atomspace
"
	(delete-dup-atoms
		(concatenate!
			(map get-conseq-sections
				(cog-incoming-by-type CNCTR 'ConnectorSeq))))
)
(define-public (fetch-connector-sections CNCTR)
"
  fetch-connector-sections CONNECTOR - return all sections that have
  this connector appearing in their connector sequence.
  Fetches sections and connector sequences from storage (does not
  assume they have been loaded yet). Use 'get-connector-sections`
  if fetching is not needed.
"
	(fetch-incoming-by-type CNCTR 'ConnectorSeq)
	(delete-dup-atoms
		(concatenate!
			(map fetch-conseq-sections
				(cog-incoming-by-type CNCTR 'ConnectorSeq))))
)
(define-public (get-endpoint-sections END)
"
  get-endpoint-sections ENDPOINT - return all sections that have this
  endpoint appearing in a connector in their connector sequences.
  Assumes that all connector sequences and sections are already in
  the atomspace
"
	(delete-dup-atoms
		(concatenate!
			(map get-connector-sections
				(cog-incoming-by-type END 'Connector))))
)
(define-public (fetch-endpoint-sections END)
"
  fetch-endpoint-sections ENDPOINT - return all sections that have this
  endpoint appearing in a connector in a connector sequence.
  Fetches connectors, connector sequences and sections from storage
  (does not assume they have been loaded yet). Use
  'get-connector-sections` if fetching is not needed.
"
	(fetch-incoming-by-type END 'Connector)
	(delete-dup-atoms
		(concatenate!
			(map fetch-connector-sections
				(cog-incoming-by-type END 'Connector))))
)
(define-public (get-conseq-germs CONSEQ)
"
  get-conseq-germs CONSEQ - return all germs that have this connector
  sequence in their section. There is one connector sequence per section.
  Assumes that all sections are already in the atomspace
  `fetch-conseq-germs` instead.
"
	(map (lambda (SEC) (cog-outgoing-atom SEC 0))
		(get-conseq-sections CONSEQ))
)
(define-public (fetch-conseq-germs CONSEQ)
"
  fetch-conseq-germs CONSEQ - return all germs that have this connector
  sequence in their section. There is one connector sequence per section.
  Fetches sections from storage (does not assume they have been loaded
  yet). Use 'get-conseq-germs` if fetching is not needed.
"
	(fetch-incoming-by-type CONSEQ 'Section)
	(get-conseq-germs CONSEQ)
)
(define-public (get-connector-germs CNCTR)
"
  get-connector-germs CONNECTOR - return all germs that have this
  connector appearing in their section.
  Assumes that all connector sequences and sections are already in
  the atomspace
"
	(delete-dup-atoms
		(concatenate!
			(map get-conseq-germs
				(cog-incoming-by-type CNCTR 'ConnectorSeq))))
)
(define-public (fetch-connector-germs CNCTR)
"
  fetch-connector-germs CONNECTOR - return all germs that have this
  connector appearing in their section.
  Fetches sections and connector sequences from storage (does not
  assume they have been loaded yet). Use 'get-connector-germs`
  if fetching is not needed.
"
	(fetch-incoming-by-type CNCTR 'ConnectorSeq)
	(delete-dup-atoms
		(concatenate!
			(map fetch-conseq-germs
				(cog-incoming-by-type CNCTR 'ConnectorSeq))))
)
(define-public (get-endpoint-germs END)
"
  get-endpoint-germs ENDPOINT - return all germs that have this
  endpoint appearing in a connector in their section.
  Assumes that all connector sequences and sections are already in
  the atomspace
"
	(delete-dup-atoms
		(concatenate!
			(map get-connector-germs
				(cog-incoming-by-type END 'Connector))))
)
(define-public (fetch-endpoint-germs END)
"
  fetch-endpoint-germs ENDPOINT - return all germs that have this
  endpoint appearing in a connector in their section.
  Fetches connectors, connector sequences and sections from storage
  (does not assume they have been loaded yet). Use 'get-endpoint-germs`
  if fetching is not needed.
"
	(fetch-incoming-by-type END 'Connector)
	(delete-dup-atoms
		(concatenate!
			(map fetch-connector-germs
				(cog-incoming-by-type END 'Connector))))
)