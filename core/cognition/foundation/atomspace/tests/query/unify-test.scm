(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))
(Inheritance (Concept "A") (Concept "B"))
(opencog-test-runner)
(test-begin "UnifyUTest::test_unify_basic_4")
(define tub4
	(cog-execute!
		(Get
			(VariableList (Variable "$X") (Variable "$Y"))
			(Identical
				(Inheritance (Variable "$X") (Concept "B"))
				(Inheritance (Concept "A") (Variable "$Y"))))))
(format #t "Got ~A\n" tub4)
(test-assert "UnifyUTest::test_unify_basic_4"
	(equal? tub4
		(Set (List (Concept "A") (Concept "B")))))
(test-end "UnifyUTest::test_unify_basic_4")
(test-begin "UnifyUTest::test_unify_basic_5")
(define tub5
	(cog-execute!
		(Get
			(Variable "$X")
			(Identical
				(Inheritance (Variable "$X") (Variable "$Y"))
				(Inheritance (Concept "A") (Variable "$Y"))))))
(format #t "Got ~A\n" tub5)
(test-assert "UnifyUTest::test_unify_basic_5"
	(equal? tub5
		(Set (Concept "A"))))
(test-end "UnifyUTest::test_unify_basic_5")
(test-begin "UnifyUTest::test_unify_basic_8")
(define tub8
	(cog-execute!
		(Get
			(VariableList (Variable "$X") (Variable "$Y"))
			(Identical
				(Inheritance (Variable "$X") (Variable "$Y"))
				(Inheritance (Concept "A") (Variable "$Z"))))))
(format #t "Got ~A\n" tub8)
(test-assert "UnifyUTest::test_unify_basic_8"
	(equal? tub8
		(Set (List (Concept "A") (Variable "$Z")))))
(test-end "UnifyUTest::test_unify_basic_8")
(test-begin "UnifyUTest::test_unify_undeclared_var_2_alt")
(define tuuv2
	(cog-execute!
		(Bind
			(Variable "$X")
			(Identical
				(Variable "$X")
				(Variable "$Z"))
			(List (Quote (Variable "$X")) (Variable "$X")))))
(format #t "Got ~A\n" tuuv2)
(test-assert "UnifyUTest::test_unify_undeclared_var_2_alt"
	(equal? tuuv2
		(Set (List (Variable "$X") (Variable "$Z")))))
(test-end "UnifyUTest::test_unify_undeclared_var_2_alt")
(test-begin "UnifyUTest::test_unify_unordered_2")
(define tun2
	(cog-execute!
		(Get
			(Variable "$Y")
			(Identical
				(And (Concept "A") (Concept "B"))
				(And (Concept "A") (Variable "$Y"))))))
(format #t "Got ~A\n" tun2)
(test-assert "UnifyUTest::test_unify_unordered_2"
	(equal? tun2
		(Set (Concept "B"))))
(test-end "UnifyUTest::test_unify_unordered_2")
(test-begin "UnifyUTest::test_unify_unordered_5")
(define tun5
	(cog-execute!
		(Get
			(VariableList (Variable "$X") (Variable "$Y"))
			(Identical
				(And (Concept "A") (Concept "A") (Concept "B") (Concept "B"))
				(And (Concept "A") (Concept "B") (Variable "$X") (Variable "$Y"))))))
(define tune5
	(Set
		(List (Concept "A") (Concept "B"))
		(List (Concept "B") (Concept "A"))))
(format #t "Got ~A\n" tun5)
(format #t "Expected ~A\n" tune5)
(test-end "UnifyUTest::test_unify_unordered_5")
(opencog-test-end)