(use-modules (opencog))
(use-modules (srfi srfi-1))
(define-public (graph-add-linear GRAPH NUMA-LIST)
"
  Linear, Sequential Graph (LSG) parser.
  Given an existing GRAPH, create a connected graph by attaching any
  unconnected Atoms in the NUMA-LIST with new edges. Attachments are
  made in sequential order, thus preserving the order of the NUMA-LIST
  and preserving the planarity (if any) of the original graph.
  The GRAPH should be an existing (possibly empty) list of 'wedges'
  connecting Atom pairs. Each 'wedge' is a weighted pair of numbered
  atoms, having the scheme form of `((NL . AL) (NR . AR) . W)` where
  AL and AR are the left and right Atoms of the edge
  ordinal numbers (integers), such that NL is less than NR, and W is
  a floating-point weight. The dot represents a scheme pair, built
  with `cons`.
  The NUMA-LIST should be a scheme-list of ordinally-numbered atoms.
  This should be a list of scheme pairs `(Num . Atom)` where `Num` is
  is an ordinal number, and `Atom` is some Atom.
  This returns a new graph, in the form of a wedge-list. The added
  edges will have a weight of minus-infinity.
  If the graph has a bridge over a sequence of unconnected nodes,
  then a loop will be created, as those unconnected nodes will be
  attached to both the left and to the right.
  If the graph has multiple components (islands), but no disconnected
  nodes, then the returned result will remain disconnected. Use
  `graph-add-bridges` to connect together islands. But if there is
  a disconnected node between two islands, then that node will be
  attached to both islands, thus connecting them.  Islands remain
  unconnected only if nothing is between them.
"
	(define (make-wedge VA VB) (cons (cons VA VB) -inf.0))
	(define (*join-em-up result to-at prev verli grali disli)
		(cond
			((or (null? disli) (null? verli)) result)
			((null? grali)
				(*join-em-up
					(if (null? prev) result
						(cons (make-wedge prev (car verli)) result))
					'() (car verli) (cdr verli) grali (cdr disli)))
			(else
				(let* ((vxit (car verli))
						(grit (car grali))
						(dsit (car disli))
						(bigg (if (null? to-at) result
								(cons (make-wedge to-at vxit) result)))
					)
					(cond
						((equal? vxit grit)
							(*join-em-up bigg '() vxit (cdr verli) (cdr grali) disli))
						((equal? vxit dsit)
							(*join-em-up
								(if (null? prev) bigg
									(cons (make-wedge prev vxit) bigg))
								vxit '() (cdr verli) grali (cdr disli)))
						(else (throw 'invalid-vertex 'graph-add-linear
							(format #f "Unexpected vertex ~A" vxit))))
	))))
	(define graver (sort-numalist (numas-in-wedge-list GRAPH)))
	(define discon (sort-numalist (lset-difference equal? NUMA-LIST graver)))
	(define alldem (sort-numalist NUMA-LIST))
	(*join-em-up GRAPH '() '() alldem graver discon)
)
(define-public (graph-add-bridges GRAPH)
"
  Sequential Island Bridger (SIB) parser.
  Given an existing GRAPH which may contain disconnected components
  or 'islands', this will add edges that connect neighboring islands.
  The GRAPH should be an existing, non-empty list of 'wedges'.
  This returns a new list of wedges, such that the resulting graph
  is simply connected.
  XXX FIXME WARNING DANGER: As written, this runs in exponential time
  as the size of the graph (the wedges), and thus can explode in
  runtime, going from a fraction of a second for one graph, and many
  minutes (or hours) for a graph that is 20% bigger!  This makes this
  function unusable. You've been warned!
"
	(define (make-wedge VA VB) (cons (cons VA VB) -inf.0))
	(define sorted-numas (sort-numalist (numas-in-wedge-list GRAPH)))
	(define right-end
		(if (null? sorted-numas) (cons -inf.0 #f)
			(right-most-numa (car sorted-numas) GRAPH)))
	(define right-idx (numa-get-index right-end))
	(define remainder (drop-while
		(lambda (numa) (<= (numa-get-index numa) right-idx))
		sorted-numas))
	(if (null? remainder) GRAPH
		(let ((gap (left-most-numa (car remainder) GRAPH)))
			(graph-add-bridges (cons (make-wedge right-end gap) GRAPH))))
)