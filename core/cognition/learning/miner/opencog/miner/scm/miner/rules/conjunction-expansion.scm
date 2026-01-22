(load "miner-rule-utils.scm")
(define (gen-conjunction-expansion-rule nary mv enforce-specialization)
(define f-vardecl (Variable "$f-vardecl"))
(define g-vardecl (Variable "$g-vardecl"))
(define db (Variable "$db"))
(define ms (Variable "$ms"))
(define g-body (Variable "$g-body"))
(define ConceptT (Type "ConceptNode"))
(define NumberT (Type "NumberNode"))
(define AndT (Type "AndLink"))
(define f-vardecl-decl f-vardecl)
(define g-vardecl-decl g-vardecl)
(define g-body-decl g-body)
(define db-decl (TypedVariable db ConceptT))
(define ms-decl (TypedVariable ms NumberT))
(define g (Quote (Lambda (Unquote g-vardecl) (Unquote g-body))))
(define minsup-g (minsup-eval g db ms))
(define formula-name (string-append "scm-eager: conjunction-expansion-"
(if enforce-specialization
"specialization-" "")
"mv-" (number->string mv)
"-formula"))
(define formula (GroundedSchema formula-name))
(if (<= nary 1)
(let* (
(f-body (Variable "$f-body"))
(f-body-decl f-body)
(f (Quote (Lambda (Unquote f-vardecl) (Unquote f-body))))
(minsup-f (minsup-eval f db ms)))
(Bind
(VariableSet
f-vardecl-decl
g-vardecl-decl
f-body-decl
g-body-decl
db-decl
ms-decl)
(And
(Present
minsup-f
minsup-g)
(absolutely-true-eval minsup-f)
(absolutely-true-eval minsup-g)
(not-equal-top f)
(not-equal-top g)
(unary-conjunction-eval g-body)
(if (= nary 1)
(unary-conjunction-eval f-body)
'()))
(ExecutionOutput
formula
(List
(minsup-eval (top) db ms)
(Set minsup-f
minsup-g)))))
(let* (
(f-conjuncts (gen-variables "$f-conjunct" nary))
(f-conjuncts-decls f-conjuncts)
(f (Quote (Lambda (Unquote f-vardecl)
(Present (map Unquote f-conjuncts)))))
(minsup-f (minsup-eval f db ms)))
(Bind
(VariableSet
f-vardecl-decl
g-vardecl-decl
f-conjuncts-decls
g-body-decl
db-decl
ms-decl)
(And
(Present
minsup-f
minsup-g)
(absolutely-true-eval minsup-f)
(absolutely-true-eval minsup-g)
(not-equal-top g)
(unary-conjunction-eval g-body))
(ExecutionOutput
formula
(List
(minsup-eval (top) db ms)
(Set minsup-f
minsup-g)))))))
(define (gen-conjunction-expansion-formula mv enforce-specialization)
(lambda (conclusion . premises)
(if (= (length premises) 1)
(let* ((minsup-fg (car premises))
(minsup-f (cog-outgoing-atom minsup-fg 0))
(minsup-g (cog-outgoing-atom minsup-fg 1))
(f (get-pattern minsup-f))
(g (get-pattern minsup-g))
(db (get-db minsup-f))
(ms (get-ms minsup-f))
(mv-nn (Number mv))
(es enforce-specialization)
(fgs (if (unary-conjunction? (get-body g))
(cog-expand-conjunction f g db ms mv-nn es)
(cog-expand-conjunction g f db ms mv-nn es)))
(mk-minsup (lambda (fg) (minsup-eval-true fg db ms)))
(minsup-fgs (map mk-minsup (cog-outgoing-set fgs))))
(Set minsup-fgs)))))
(define (gen-formula a) (gen-conjunction-expansion-formula a #f))
(define conjunction-expansion-mv-1-formula (gen-formula 1))
(define conjunction-expansion-mv-2-formula (gen-formula 2))
(define conjunction-expansion-mv-3-formula (gen-formula 3))
(define conjunction-expansion-mv-4-formula (gen-formula 4))
(define conjunction-expansion-mv-5-formula (gen-formula 5))
(define conjunction-expansion-mv-6-formula (gen-formula 6))
(define conjunction-expansion-mv-7-formula (gen-formula 7))
(define conjunction-expansion-mv-8-formula (gen-formula 8))
(define conjunction-expansion-mv-9-formula (gen-formula 9))
(define (gen-spec-formula a) (gen-conjunction-expansion-formula a #t))
(define conjunction-expansion-specialization-mv-1-formula (gen-spec-formula 1))
(define conjunction-expansion-specialization-mv-2-formula (gen-spec-formula 2))
(define conjunction-expansion-specialization-mv-3-formula (gen-spec-formula 3))
(define conjunction-expansion-specialization-mv-4-formula (gen-spec-formula 4))
(define conjunction-expansion-specialization-mv-5-formula (gen-spec-formula 5))
(define conjunction-expansion-specialization-mv-6-formula (gen-spec-formula 6))
(define conjunction-expansion-specialization-mv-7-formula (gen-spec-formula 7))
(define conjunction-expansion-specialization-mv-8-formula (gen-spec-formula 8))
(define conjunction-expansion-specialization-mv-9-formula (gen-spec-formula 9))