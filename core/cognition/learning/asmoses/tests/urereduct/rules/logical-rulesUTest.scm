(define testcase1
    (AndLink
       (False)
       (False)
       (True)))
(define expected1
 (SetLink
   (ReducedTo
     (AndLink
     (False)
     (False)
     (True))
     (False)
     (False))))
(define testcase2
  (OrLink
       (False)
       (False)
       (True)))
(define expected2
 (SetLink
   (ReducedTo
     (OrLink
     (False)
     (False)
     (True))
     (True))))
(define expected3
   (SetLink
    (ReducedTo
     (AndLink
     (False)
     (True)
     (False))
     (False))))
(define expected4
(SetLink
   (ReducedTo
     (OrLink
     (True)
     (False)
     (False))
     (True)
     (False))))
(define expected5
   (SetLink
     (ReducedTo
     (OrLink
     (False)
     (False)
     (True))
     (True)
     (False)
     (False))))
(define testcase8
(NotLink
        (NotLink
          (False))))
(define expected8
  (SetLink
    (ReducedTo
      (NotLink
       (NotLink
         (False)))
       (False))))