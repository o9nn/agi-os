(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog uuid))
(define (graph-to-nodes GRAPH)
	(define pt-list
		(map (lambda (sect) (gar sect))
			(cog-outgoing-set GRAPH)))
	(fold
		(lambda (point str)
			(string-concatenate (list
				"\tnode [\n"
				"\t\tid "
				(format #f "~D" (cog-assign-uuid point))
				"\n"
				"\t\tlabel \""
				(cog-name point)
				"\"\n"
				"\t]\n"
				str
			)))
		""
		(delete-dup-atoms pt-list)
	)
)
(define (graph-to-edges GRAPH)
	(define dupe-sets '())
	(define is-duplicate? (make-atom-set))
	(define (filter-dupe elist result duper-list)
		(define duper?
			(if (null? duper-list) (make-atom-set) (car duper-list)))
		(define duper-rest
			(if (null? duper-list) '() (cdr duper-list)))
		(if (null? duper-list)
			(set! dupe-sets (append dupe-sets (list duper?))))
		(if (null? elist)
			result
			(filter-dupe
				(keep-duplicate-atoms elist)
				(append
					(filter
						(lambda (edge) (not (duper? edge)))
						elist)
					result)
				duper-rest)))
	(define edge-list
		(append-map
			(lambda (sect)
				(filter-dupe
					(cog-outgoing-set (gdr sect))
					'()
					dupe-sets))
			(cog-outgoing-set GRAPH)))
	(fold
		(lambda (edge str)
			(string-concatenate (list
				"\tedge [\n"
				"\t\tsource "
				(format #f "~D" (cog-assign-uuid (gadr edge)))
				"\n\t\ttarget "
				(format #f "~D" (cog-assign-uuid (gddr edge)))
				"\n\t\tlabel \""
				(cog-name (gar edge))
				"\"\n\t]\n"
				str
			)))
		""
		edge-list
	)
)
(define-public (export-to-gml GRAPH-SET)
"
   export-to-gml GRAPH-SET
   Export the GRAPH-SET - a SetLink wrapping one or more graphs,
	to a UTF-8 encoded text string in GML - Graph Modeling Language
   format.
"
	(define graph-id 0)
	(fold
		(lambda (grph str)
			(set! graph-id (+ 1 graph-id))
			(string-concatenate (list
				"graph [\n"
				"\tcomment \"Created by opencog generate\"\n"
				"\tdirected 1\n"
				(format #f "\tlabel \"placeholder ~D\"\n" graph-id)
				(format #f "\tid ~D\n" graph-id)
				(graph-to-nodes grph)
				(graph-to-edges grph)
				"]\n"
				str)))
		""
		(cog-outgoing-set GRAPH-SET))
)