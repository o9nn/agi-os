(use-modules (opencog nlp))
(define-public (grounded-talk USER QUERY)
"
  grounded-talk USER QUERY -- accept USER's text and perform action,
  or maybe generate a reply (replies are currently broken).
  This is a truncated chatbot interface, for use with the robot.
  It accepts an utterance (in the form of a text string) and, if it is
  understood, then the robot performs an action.
  The USER is the user-name  The QUERY is the string holding what the
  user said.
"
    (define sent-node (car (nlp-parse QUERY)))
    (display "Hello ")
    (display USER)
    (display ", you said: \"")
    (display QUERY)
    (display "\"")
    (newline)
    (let* ((gutr (sentence-get-utterance-type sent-node))
           (utr (if (equal? '() gutr) '() (car gutr)))
        )
    (cond
        ((equal? utr (DefinedLinguisticConceptNode "TruthQuerySpeechAct"))
            (display "You asked a Truth Query\n")
            (display "I can't process truth query for now\n")
        )
        ((equal? utr (DefinedLinguisticConceptNode "InterrogativeSpeechAct"))
            (display "You made an Interrogative SpeechAct\n")
            (self-wh-query sent-node)
        )
        ((equal? utr (DefinedLinguisticConceptNode "DeclarativeSpeechAct"))
            (display "You made a Declarative SpeechAct\n")
        )
        ((equal? utr (DefinedLinguisticConceptNode "ImperativeSpeechAct"))
            (display "You made a Imperative SpeechAct\n")
				(imperative-process sent-node)
        )
        (else
            (display "Sorry, I can't identify the speech act type\n")
        )
    )))