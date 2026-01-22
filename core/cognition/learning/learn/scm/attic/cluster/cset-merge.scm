(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog sheaf) (opencog persist))
(define (matching-sequences CON-A CON-B)
"
  matching-sequences CON-A CON-B -- return matching ConnectorSeqs's
  Find all ConnectorSeq that contain CON-A, and check to see if an
  equivalent ConnectorSeq exists, containing CON-B. If so, create
  a scheme pair containing both. Return a list of these matching pairs.
"
	(filter-map
		(lambda (ASEQ)
			(define bseq
				(cog-link 'ConnectorSeq
					(map
						(lambda (CON) (if (equal? CON-A CON) CON-B CON))
						(cog-outgoing-set ASEQ))))
			(if (nil? bseq) #f (cons ASEQ bseq)))
		(cog-incoming-by-type CON-A 'ConnectorSeq)))
(define (matching-sections CON-A CON-B)
"
  matching-sections CON-A CON-B -- return matching Sections
  Find all Sections that contain CON-A, and check to see if an
  equivalent Section exists, containing CON-B. If so, create
  a scheme pair containing both. Return a list of these matching pairs.
"
	(concatenate! (map
		(lambda (PR)
			(define sequ-a (car PR))
			(define sequ-b (cdr PR))
			(filter-map
				(lambda (A-SEC)
					(define b-sec (cog-link 'Section (gar A-SEC) sequ-b))
					(if (nil? b-sec) #f (cons A-SEC b-sec)))
				(cog-incoming-by-type sequ-a 'Section)))
		(matching-sequences CON-A CON-B)))
)
(define (fetch-class-words CLS-LST)
	(delete-dup-atoms
		(concatenate!
			(map
				(lambda (CLS)
					(fetch-incoming-by-type CLS 'MemberLink)
					(map
						(lambda (MEMB) (cog-outgoing-atom MEMB 0))
						(cog-incoming-by-type CLS 'MemberLink)))
				CLS-LST)))
)