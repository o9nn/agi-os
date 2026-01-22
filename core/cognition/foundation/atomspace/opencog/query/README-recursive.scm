(DefineLink
	(DefinedPredicateNode "factorial")
	(LambdaLink
		(VariableNode "$n")
		(SequentialAndLink
			(GreaterThanLink (VariableNode "$n") (NumberNode 1))
			(TimesLink
				(VariableNode "$n")
				(PutLink
					(DefinedPredicateNode "factorial")
					(PlusLink (VariableNode "$n") (NumberNode -1)))))))
(DefineLink
	(DefinedSchemaNode "NP detector")
	(LambdaLink
		(VariableNode "$word")
		(ChoiceLink
			(EvaluationLink
				(PredicateNode "Noun")
				(ListLink
					(VariableNode "$word")))
			(EvaluationLink
				(PredicateNode "NP")
				(ListLink
					(VariableNode "$word")
					(DefinedSchemaNode "NP detector"))))))
(EvaluationLink
	(PredicateNode "NP")
	(ListLink
		(WordNode "a")
		(EvaluationLink
			(PredicateNode "NP")
			(ListLink
				(WordNode "noun")
				(EvaluationLink
					(PredicateNode "Noun")
					(ListLink
						(WordNode "phrase")))))))
(DefineLink
	(DefinedSchemaNode "flat NP detector")
	(LambdaLink
		(VariableNode "$word")
		(VariableNode "$np")
		(ChoiceLink
			(EvaluationLink
				(PredicateNode "Noun")
				(ListLink
					(VariableNode "$word")))
			(AndLink
				(EvaluationLink
					(PredicateNode "WordPair")
					(ListLink
						(VariableNode "$word")
						(VariableNode "$np")))
				(PutLink
					(DefinedSchemaNode "flat NP detector")
					(ListLink
						(VariableNode "$word")
						(VariableNode "$np"))) ))))
(EvaluationLink
	(PredicateNode "Determiner")
	(ListLink (WordNode "a")))
(EvaluationLink
	(PredicateNode "Adjective")
	(ListLink (WordNode "short")))
(EvaluationLink
	(PredicateNode "Noun")
	(ListLink (WordNode "phrase")))
(EvaluationLink
	(PredicateNode "WordPair")
	(ListLink (WordNode "short") (WordNode "phrase")))
(EvaluationLink
	(PredicateNode "WordPair")
	(ListLink (WordNode "a") (WordNode "short")))