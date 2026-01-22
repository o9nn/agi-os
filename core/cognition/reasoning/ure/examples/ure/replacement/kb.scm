(define (believe A B)
  (Evaluation
    (Predicate "believe")
    (List A B)))
(define max-believes-universe-is-math
  (believe
    (Concept "Max Tegmark")
    (Similarity
      (Concept "Universe")
      (Concept "Mathematics"))))
(define nil-believes-max-believes-universe-is-math
  (believe
    (Concept "Nil")
    max-believes-universe-is-math))
(define (synonymous A B)
  (Evaluation
    (Predicate "synonymous")
    (List A B)))
(synonymous (Concept "Mathematics") (Concept "Consciousness"))