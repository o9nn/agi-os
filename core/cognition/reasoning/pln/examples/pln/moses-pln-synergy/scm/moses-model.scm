(define moses-model
   (ImplicationLink (stv 0.875 0.0099)
      (OrLink
         (PredicateNode "take-treatment-1")
         (PredicateNode "eat-lots-fruits-vegetables")
      )
      (PredicateNode "recovery-speed-of-injury-alpha")
   )
)