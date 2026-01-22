(use-modules (srfi srfi-1))
(define-public (make-sections WEDGE-LIST)
"
  make-sections - create sections of a graph.
  Given a graph, expressed as a set of edges, return a list of sections
  for the vertexes in the graph. By definition, there is only one
  section per vertex.
  The WEDGE-LIST is assumed to be a list of weighted edges. The weights
  are ignored.  The vertexes in the graph are assumed to just be the
  set of vertexes that appear at the ends of each edge
  extracted automatically, below.
  The returned sections are a list of SectionLinks, one for each vertex.
  The SectionLink will list (in order) a list of ConnectorLink's, with
  each connector implicitly specifying an edge, by specifying the atom
  at the far end of the edge.  The connectors are labeled with direction
  marks, '+' and '-', indicating whether the far end is to the right or
  the left of the given vertex.
  So, for example, given the MST parse
     (mst-parse-text 'The game is played on a level playing field')
  the word 'playing' might get this connector set:
    (Section
       (WordNode \"playing\")
       (ConnectorSeq
          (Connector
             (WordNode \"level\")
             (ConnectorDir \"-\"))
          (Connector
             (WordNode \"field\")
             (ConnectorDir \"+\"))))
  As the local section of a single graph, it captures the local
  structure that there was a link level<-->playing and a link
  playing<-->field. The ConnectorDir indicates whether the link went
  to the left or to the right.  This allow the ConnectorSeq to be
  independent of Section itself
  mentions the (WordNode \"playing\").  This allows different
  ConnectorSeq's to be explicitly compared.
"
	(define (mk-pseudo NUMA WEDLI)
		(define left-nus (sort-numalist (left-linked-numas NUMA WEDLI)))
		(define right-nus (sort-numalist (right-linked-numas NUMA WEDLI)))
		(define left-cnc
			(map (lambda (sw)
					(Connector
						(numa-get-atom sw)
						(ConnectorDir "-")))
			left-nus))
		(define right-cnc
			(map (lambda (sw)
					(Connector
						(numa-get-atom sw)
						(ConnectorDir "+")))
			right-nus))
		(Section
			(numa-get-atom NUMA)
			(ConnectorSeq (append left-cnc right-cnc)))
	)
	(map
		(lambda (seq) (mk-pseudo seq WEDGE-LIST))
		(numas-in-wedge-list WEDGE-LIST))
)