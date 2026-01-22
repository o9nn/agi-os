(use-modules (srfi srfi-1))
(use-modules (opencog nlp sureal))
(use-modules (opencog nlp relex2logic))
(define current-sentence (AnchorNode "*-eva-current-sent-*"))
(define current-reply (AnchorNode "*-reply-*"))
(StateLink current-reply (Set))
(define where-look-rule
	(BindLink
		(VariableList
			(var-decl "$sent" "SentenceNode")
			(var-decl "$parse" "ParseNode")
			(var-decl "$interp" "InterpretationNode")
			(var-decl "$verb-inst" "WordInstanceNode")
			(var-decl "$qvar-inst" "WordInstanceNode")
			(var-decl "$subj-inst" "WordInstanceNode")
		)
		(AndLink
			(StateLink current-sentence (Variable "$sent"))
			(parse-of-sent   "$parse" "$sent")
			(interp-of-parse "$interp" "$parse")
			(word-in-parse   "$verb-inst" "$parse")
			(word-in-parse   "$qvar-inst" "$parse")
			(LemmaLink (VariableNode "$qvar-inst") (WordNode "where"))
			(LemmaLink (VariableNode "$verb-inst") (WordNode "look"))
			(word-pos "$verb-inst" "verb")
			(dependency "_subj" "$verb-inst" "$subj-inst")
			(LemmaLink (VariableNode "$subj-inst") (WordNode "you"))
		)
		(ListLink
			current-reply
			(SetLink
				(Evaluation (Predicate "looking") (ListLink (Concept "I")))
(VariableNode "$verb-inst")
			)
		)
	)
)
(define prt-sent
	(BindLink
		(VariableList
			(var-decl "$sent" "SentenceNode")
			(var-decl "$parse" "ParseNode")
			(var-decl "$word-inst" "WordInstanceNode")
			(var-decl "$word" "WordNode")
		)
		(AndLink
			(StateLink current-sentence (Variable "$sent"))
			(parse-of-sent   "$parse" "$sent")
			(word-in-parse   "$word-inst" "$parse")
			(LemmaLink (Variable "$word-inst") (Variable "$word"))
		)
		(ListLink
			(Variable "$word")
		)
	)
)
(define (prt-curr-sent) (cog-execute! prt-sent))
(define face-expression-state (AnchorNode "Facial Expression State"))
(define what-doing-rule
	(BindLink
		(VariableList
			(var-decl "$sent" "SentenceNode")
			(var-decl "$parse" "ParseNode")
			(var-decl "$verb-inst" "WordInstanceNode")
			(var-decl "$qvar-inst" "WordInstanceNode")
			(var-decl "$subj-inst" "WordInstanceNode")
			(var-decl "$expression" "ConceptNode")
		)
		(AndLink
			(StateLink current-sentence (Variable "$sent"))
			(parse-of-sent   "$parse" "$sent")
			(word-in-parse   "$verb-inst" "$parse")
			(word-in-parse   "$qvar-inst" "$parse")
			(LemmaLink (VariableNode "$qvar-inst") (WordNode "what"))
			(LemmaLink (VariableNode "$verb-inst") (WordNode "do"))
			(word-pos "$verb-inst" "verb")
			(dependency "_subj" "$verb-inst" "$subj-inst")
			(LemmaLink (VariableNode "$subj-inst") (WordNode "you"))
			(State face-expression-state (Variable "$expression"))
		)
		(ListLink
			current-reply
			(SetLink
				(Evaluation (Predicate "doing") (ListLink (Concept "I")))
(VariableNode "$expression")
			)
		)
	)
)
(define (cog-grounded? EXPR)
"
  cog-grounded? EXPR
  Return #f if EXPR contains a VariableNode, else return #t.
"
	(if (cog-node? EXPR)
		(not (eq? 'VariableNode (cog-type EXPR)))
		(not (find (lambda (x) (not (cog-grounded? x))) (cog-outgoing-set EXPR)))
	)
)
(define (get-grounded-replies)
	(filter cog-grounded? (cog-incoming-set current-reply))
)
(define (self-wh-query QUERY)
"
  Process a query about self.  Return an answer, or else nil, if
  no answer is known.  QUERY should be a SentenceNode.
"
	(define (verbalize-reply rep-lnk)
		(if (eq? 0 (cog-arity rep-lnk))
			'()
			(let ((r2l-set (cog-outgoing-atom rep-lnk 1)))
(format #t "The reply is:\n~a\n" r2l-set)
				(if (eq? 0 cog-arity r2l-set)
					'()
					(let
						((string-seq (sureal r2l-set)))
						(display string-seq) (newline)
						string-seq
					)))))
	(StateLink current-sentence QUERY)
	(cog-execute! what-doing-rule)
(format #t  "Replies to questions:\n~a\n" (get-grounded-replies))
	(let ((reply-words (filter verbalize-reply (get-grounded-replies))))
(format #t "Reply words are: ~a\n" reply-words)
		(if (null? reply-words)
			(set! reply-words
				(list "Sorry I didn't understand the question.\n"))
		)
		(map cog-extract-recursive! (get-grounded-replies))
		reply-words
	)
)