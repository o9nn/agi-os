(use-modules (opencog) (opencog nlp) (opencog exec))
(use-modules (opencog nlp fuzzy))
(use-modules (opencog nlp relex2logic))
(use-modules (opencog nlp relex2logic))
(load-r2l-rulebase)
(define (get-interp-node sent-node)
"
  Given a sentence, get the likliest interpretation node for it.
  At this time, it simply returns the very first interpretation.
  Yes, this is a quick hack, needs fixing. XXX FIXME.
"
	(define parse (car (cog-chase-link 'ParseLink 'ParseNode sent-node)))
	(car (cog-chase-link 'InterpretationLink 'InterpretationNode parse)))
(define (get-interp-of-r2l r2l-set-list)
"
  Given a ListLink of r2l-sets, pick out the InterpetationNode from
  each, and return those (as a list).
XXX this may be junk/obsolete, the format of r2l-sets seems to have
changed recently.  I'm confused. Current structure seems to be this:
(ReferenceLink (InterpretationNode \"sentence@f2b..\") (SetLink ...))
but this is not what the code below looks for...
"
	(define (find-interp r2l-set)
		(define (find-inh inh-link)
			(if (eq? (cog-type inh-link) 'InheritanceLink)
				(eq? 'InterpretationNode
					(cog-type (car (cog-outgoing-set inh-link))))
				#f
			)
		)
		(car (cog-outgoing-set
			(find find-inh (cog-outgoing-set r2l-set))))
	)
	(map find-interp (cog-outgoing-set r2l-set-list))
)
(define known-directives
	(list
		(get-interp-node (car (nlp-parse "look left")))
		(get-interp-node (car (nlp-parse "look right")))
		(get-interp-node (car (nlp-parse "look up")))
		(get-interp-node (car (nlp-parse "look down")))
	))
(define (imperative-process-v2 imp)
"
  Process imperative IMP, which should be a SentenceNode.
"
	(define r2l-set (get-r2l-set-of-sent imp))
	(define fzset (cog-fuzzy-match r2l-set))
	(define interp (car (get-interp-of-r2l fzset)))
	(define known (find (lambda (inp) (eq? interp inp)) known-directives))
	(if (eq? #f known)
		(display "I don't know how to do that.\n")
	)
	known
)