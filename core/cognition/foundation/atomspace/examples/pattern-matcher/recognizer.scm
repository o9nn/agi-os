(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog exec))
(BindLink
	(ListLink
		(Concept "I") (Glob "$star") (Concept "you"))
	(ListLink
		(Concept "I") (Glob "$star") (Concept "you") (Concept "too")))
(BindLink
	(ListLink
		(Concept "I")
		(Concept "love")
		(Glob "$star"))
	(ListLink
		(Concept "I")
		(Concept "like")
		(Glob "$star")
		(Concept "a")
		(Concept "lot!")))
(define sent
	(ListLink (Concept "I") (Concept "love") (Concept "you")))
(cog-execute! (DualLink sent))
(SetLink
	(ListLink
		(Concept "I")
		(Glob "$star")
		(Concept "you")
	)
	(ListLink
		(Concept "I")
		(Concept "love")
		(Glob "$star")
	)
)
(define adv-sent
	(ListLink
		(Concept "I")
		(Concept "really")
		(Concept "truly")
		(Concept "love")
		(Concept "you")))
(cog-execute! (DualLink adv-sent))
(define (get-consequents ANTECEDENT)
"
  get-consequents ANTECEDENT -- given the ANTECEDENT, return a set of
  all of the consequents of a rule.
  Example usage:
     (get-consequents (List (Concept \"I\") (Glob \"$star\") (Concept \"you\")))
"
	(cog-execute!
		(GetLink
			(TypedVariable (Variable "$consequent") (Type "ListLink"))
			(Quote (BindLink ANTECEDENT (Unquote (Variable "$consequent"))))
		)
	)
)
(define (get-rules-for-ante ANTECEDENT)
"
  get-rules-for-ante ANTECEDENT -- given the ANTECEDENT, return a set
  of all rules that can be applied to it.
  Example usage:
     (get-rules-for-ante (List (Concept \"I\") (Glob \"$star\") (Concept \"you\")))
"
	(cog-execute!
		(Put
			(TypedVariable (Variable "$list") (Type "ListLink"))
			(Quote (Bind ANTECEDENT (Unquote (Variable "$list"))))
			(Get
				(Variable "$consequent")
				(Quote (BindLink ANTECEDENT (Unquote (Variable "$consequent"))))
			)
		)
	)
)
(define (get-untyped-rules DATA)
"
  get-untyped-rules DATA -- given the graph DATA, return a set of all
  rules that can be applied to it.
  Example usage:
     (get-untyped-rules (List (Concept \"I\") (Concept \"love\") (Concept \"you\")))
"
	(cog-execute!
		(Put
			(TypedVariable (Variable "$ante") (Type "ListLink"))
			(Put
				(TypedVariable (Variable "$list") (Type "ListLink"))
				(Quote (BindLink
					(Unquote (Variable "$ante"))
					(Unquote (Variable "$list"))
				))
				(GetLink
					(Variable "$consequent")
					(Quote (BindLink
						(Unquote (Variable "$ante"))
						(Unquote (Variable "$consequent"))))
				))
			(Dual DATA)
		))
)
(define (unwrap-rules RULES)
   (fold
		(lambda (s li) (cons (car (cog-outgoing-set s)) li))
		'()
		(cog-outgoing-set RULES)))
(define ruleset
	(unwrap-rules
		(get-untyped-rules (List (Concept "I") (Concept "love") (Concept "you")))))
(map cog-execute! ruleset)
(define a-love-b
	(BindLink
		(VariableList
			(TypedVariable (Glob "$A") (Type "ConceptNode"))
			(TypedVariable (Glob "$B") (Type "ConceptNode")))
		(ListLink
			(Glob "$A")
			(Concept "loves")
			(Glob "$B"))
		(ListLink
			(Concept "I'm")
			(Concept "sure")
			(Concept "that")
			(Glob "$A")
			(Concept "loves")
			(Glob "$B"))))
(define mary-n-joe
	(List (Concept "Mary") (Concept "loves") (Concept "Joe")))
(cog-execute! (Dual mary-n-joe))
(define (pattern-getter ANTECEDENT)
	(GetLink
		(VariableList
			(TypedVariable (Variable "$vardecl")
				(TypeChoice
					(Type "VariableNode")
					(Type "TypedVariableLink")
					(Type "VariableList")))
			(TypedVariable (Variable "$consequent") (Type "ListLink")))
		(Quote (BindLink
				(Unquote (Variable "$vardecl"))
				ANTECEDENT
				(Unquote (Variable "$consequent"))))
	)
)
(define (get-conseq-typed ANTECEDENT)
"
  get-conseq-typed ANTECEDENT -- given the ANTECEDENT, return a set of
  all of the consequents of a rule.
  Example usage:
     (get-conseq-typed (List (Glob \"$A\") (Concept \"loves\") (Glob \"$B\")))
"
	(cog-execute! (pattern-getter ANTECEDENT))
)
(define (rule-getter ANTECEDENT)
	(Put
		(VariableList
			(Variable "$decls")
			(Variable "$sequent"))
		(Quote (Bind
				(Unquote (Variable "$decls"))
				ANTECEDENT
				(Unquote (Variable "$sequent"))))
		(pattern-getter ANTECEDENT)
	)
)
(define (get-typed-rules-for-ante ANTECEDENT)
"
  get-typed-rules-for-ante ANTECEDENT -- given the ANTECEDENT, return a set
  of all rules (with type declarations in them) that can be applied to it.
  Example usage:
     (get-typed-rules-for-ante (List (Glob \"$A\") (Concept \"loves\") (Glob \"$B\")))
"
	(cog-execute! (rule-getter ANTECEDENT))
)
(define (rule-recognizer DATA)
	(Put
		(TypedVariable (Variable "$ante") (Type "ListLink"))
		(rule-getter (Unquote (Variable "$ante")))
		(Dual DATA)
	)
)
(define (get-typed-rules DATA)
"
  get-typed-rules DATA -- given the graph DATA, return a set of all
  rules that (that have type declarations) that can be applied to it.
  Example usage:
     (get-typed-rules (List (Concept \"I\") (Concept \"love\") (Concept \"you\")))
"
	(cog-execute! (rule-recognizer DATA))
)
(define ruleset
	(unwrap-rules
		(get-typed-rules (List (Concept "Mary") (Concept "loves") (Concept "Joe")))))
(map cog-execute! ruleset)
(SetLink
   (ListLink
      (ConceptNode "I'm")
      (ConceptNode "sure")
      (ConceptNode "that")
      (ConceptNode "Mary")
      (ConceptNode "loves")
      (ConceptNode "Joe")))
(define (aiml-reply SENT)
	(define (split-sentence SENT)
		(ListLink
			(map (lambda (word) (Concept word)) (string-split SENT #\ ))))
	(define (get-aiml-response SENT)
		(define ruleset
			(unwrap-rules (get-typed-rules (split-sentence SENT))))
		(map cog-execute! ruleset)
	)
	(define (atoms-to-strings NODELIST)
		(fold-right
			(lambda (s li) (cons (string-append (cog-name s) " ") li))
			'()
			(cog-outgoing-set NODELIST)))
	(define (make-sent NODELIST)
		(string-concatenate (atoms-to-strings NODELIST)))
	(define (make-reply SENT)
		(map make-sent
			(cog-outgoing-set (car (get-aiml-response SENT)))))
	(define reply "")
	(begin
		(cog-push-atomspace)
		(set! reply (make-reply SENT))
		(cog-pop-atomspace)
		reply)
)
*unspecified*