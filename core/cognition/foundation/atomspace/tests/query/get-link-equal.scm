(use-modules (opencog) (opencog exec))
(define top
  (Lambda
    (Variable "$X")
    (Variable "$X"))
)
(define (top? x)
  (Equal
    x
    top)
)
(define gl
  (GetLink
    (VariableList
      (VariableNode "$vardecl")
      (VariableNode "$body"))
    (AndLink
      (QuoteLink
        (LambdaLink
          (UnquoteLink
            (VariableNode "$vardecl"))
          (UnquoteLink
            (VariableNode "$body"))))
      (top?
        (QuoteLink
          (LambdaLink
            (UnquoteLink
              (VariableNode "$vardecl"))
            (UnquoteLink
              (VariableNode "$body")))))))
)
(define expect
  (SetLink
    (ListLink
      (VariableNode "$X")
      (VariableNode "$X")))
)