(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog matrix))
(define-public (make-any-link-api)
"
make-any-link-api -- Word-pair access methods.
This implements a word-pair object, where the two words are connected
with a BondNode \"ANY\", in an EdgeLink.
That is, a word pair is represented as:
EdgeLink
BondNode \"ANY\"
ListLink
WordNode \"word\"
WordNode \"bird\"
The above provides a location for storing various counts, frequencies,
entropies, etc pertaining to this particular pair.
The 'get-pair method returns the above EdgeLink, if it exists.
The 'make-pair method will create it, if it does not exist.
Left-side counts, frequencies, etc. such as N(*,y) P(*,y) or
log_2 P(*,y) will be placed on the left-marginal, which is returned
by the 'left-wildcard method:
EdgeLink
BondNode \"ANY\"
ListLink
AnyNode \"left-word\"
WordNode \"bird\"
The corresponding N(x,*) P(x,*) etc are hung on the right-marginal,
returned by the 'right-wildcard method:
EdgeLink
BondNode \"ANY\"
ListLink
WordNode \"word\"
AnyNode \"right-word\"
Finally, the 'left-type and 'right-type methods return the type
of the the two sides of the pair.
"
(make-edge-pair-api
'EdgeLink
(BondNode "ANY")
'WordNode
'WordNode
(AnyNode "left-word")
(AnyNode "right-word")
"ANY"
"Link Grammar ANY link Word Pairs")
)