(define (gen-junc-lambda-distribution-fact junc N)
(let* (
(TyVs (VariableNode "$TyVs"))
(var-var (TypedVariableLink
TyVs
(TypeChoice
(TypeNode "TypedVariableLink")
(TypeNode "VariableNode")
(TypeNode "VariableList"))))
(gen-conjuctee-name (lambda (i)
(string-append "$Body-"
(number->string i))))
(conjuctee-names (map gen-conjuctee-name (iota N)))
(var-conjuctees (map VariableNode conjuctee-names))
(var-decl-list (cons var-var var-conjuctees))
(variables (VariableList var-decl-list))
(gen-lambda (lambda (v) (LambdaLink TyVs v)))
(lambdas (map gen-lambda var-conjuctees))
(left-term (LambdaLink TyVs (apply junc var-conjuctees)))
(right-term (apply junc lambdas)))
(EquivalenceLink (stv 1 1)
variables
left-term
right-term)))
(define and-lambda-distribution-1-fact
(gen-junc-lambda-distribution-fact AndLink 1))
(define and-lambda-distribution-2-fact
(gen-junc-lambda-distribution-fact AndLink 2))
(define and-lambda-distribution-3-fact
(gen-junc-lambda-distribution-fact AndLink 3))
(define or-lambda-distribution-1-fact
(gen-junc-lambda-distribution-fact OrLink 1))
(define or-lambda-distribution-2-fact
(gen-junc-lambda-distribution-fact OrLink 2))
(define or-lambda-distribution-3-fact
(gen-junc-lambda-distribution-fact OrLink 3))