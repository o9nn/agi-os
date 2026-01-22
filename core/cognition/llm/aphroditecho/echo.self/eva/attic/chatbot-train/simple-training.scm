(use-modules (opencog) (opencog query) (opencog exec) (opencog eva-model)
    (opencog eva-behavior))
(load "../aiml2oc/aiml2oc_guile/code/OpenCogAimlReply1.scm")
(load-from-path "opencog/eva-behavior/cfg-eva.scm")
(load "behavior-defs.scm")
(load "behavior-rules.scm")
(define training-rule
    (BindLink
        (ListLink
            (ConceptNode "WHEN")
            (ConceptNode "I")
            (ConceptNode "SAY")
            (GlobNode "$stimulus")
            (ConceptNode "THEN")
            (GlobNode "$response"))
        (Evaluation
            (GroundedPredicateNode "scm:create-behavior-rule")
            (ListLink
                (List (GlobNode "$stimulus"))
                (List (GlobNode "$response"))))))
#!
(define training-rule
    (BindLink
        (ListLink
            (ConceptNode "WHEN")
            (ConceptNode "I")
            (ConceptNode "SAY")
            (GlobNode "$stimulus")
            (ConceptNode "THEN")
            (ConceptNode "YOU")
            (GlobNode "$response"))
        (Evaluation
            (GroundedPredicateNode "scm:create-behavior-rule")
            (ListLink
                (List (GlobNode "$stimulus"))
                (List (GlobNode "$response"))))))
!#
(define (execute-behavior-with-stimulus input-str)
    (define rule)
    (define bind-results)
    (define eval-results)
    (define as-orig)
    (define temp-rule)
    (define consequent)
    (define result)
	(define listified-string (mapConceptualizeString (clean-text input-str)))
    (set! rule (get-tree-with-antecedent listified-string))
    (if rule
        (begin
            (display "rule: ")(display rule)(newline)
            (set! as-orig (cog-set-atomspace! (cog-new-atomspace)))
            (set! temp-rule
                (cog-new-link 'BindLink (cog-outgoing-set rule)))
            (cog-new-link 'ListLink (cog-outgoing-set listified-string))
            (set! bind-results (cog-execute! temp-rule))
            (cog-set-atomspace! as-orig)
            (set! consequent (gar bind-results))
            (if (cog-node? consequent)
                (set! consequent (cog-node
                    (cog-type consequent) (cog-name consequent))))
            (set! result (cog-evaluate! consequent))
        )
        (set! result #f)
    )
    (cog-extract! listified-string)
	result
)
(define (get-tree-with-antecedent listified-string)
	(cog-push-atomspace)
    (let* ((query-pattern (PatternLink
                               (BindLink
                                  listified-string
                                  (VariableNode "$impl"))))
           (results (cog-recognize query-pattern))
          )
        (cog-pop-atomspace)
        (if (> (length (cog-outgoing-set results)) 0)
	        (gar results)
	        #f
	    )
    )
)
(define (clean-text input-str)
    (string-trim-both (cleanText (string-upcase input-str))))
(define (say input-str)
    (execute-behavior-with-stimulus input-str))
(define (string-to-atomese input)
    (define atomese-string)
    (define stim-and-response)
    (define cleaned-text
        (string-trim-both (cleanText (string-upcase input))))
    (set! atomese-string (genQueryPattern cleaned-text))
    (display "atomese-string: ")(atomese-string)(newline)
)
(define (delistify ll)
	(define concept-words (cog-outgoing-set ll))
	(define first (cog-name (list-ref concept-words 0)))
	(set! concept-words (list-tail concept-words 1))
	(string-downcase
		(fold (lambda (new prev) (string-append prev " " (cog-name new)))
			first
			concept-words)))
(define (create-behavior-rule stimulus response)
    (define new-rule)
    (define atomese-string)
    (display "\n(create-behavior-rule) \n    stimulus: ")(display stimulus)
        (display "    response: ")(display response)(newline)
	(set! new-rule
    	(BindLink
    		stimulus
			(DefinedPredicateNode  (delistify response))
		)
	)
	(display new-rule)
    (stv 1 1)
)
(define (incoming atom) (cog-incoming-set atom))
(define (prt) (cog-prt-atomspace))
(define (root atom) (cog-get-root atom))
(define w (Concept "WHEN"))
(define (training-rule? atom)
    (cog-satisfy
        (SatisfactionLink
            (DefineLink
                (DefinedType "training-rule")
                atom))))
(define (training-rule2 atom)
    (cog-get
        (GetLink
            (DefineLink
                (DefinedType "training-rule")
                atom))))