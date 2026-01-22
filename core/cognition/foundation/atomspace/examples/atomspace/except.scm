(use-modules (opencog) (opencog exec))
(cog-execute!
   (ExecutionOutput
      (GroundedSchema "py:b0rk3n_junk")
      (List
         (Concept "1")
         (Concept "2"))))
(catch
   #t
   (lambda ()
      (cog-execute!
         (ExecutionOutput
            (GroundedSchema "py:b0rk3n_junk")
            (List
               (Concept "1")
               (Concept "2")))))
   (lambda (key . args)
      (display "Ohhh noooo Mr. Bill!!! ") (display key)
      (newline)
      (display "Sluggo says to ... ") (display args)
      (newline) (newline)
   ))
(cog-execute!
   (ExecutionOutput
      (GroundedSchema "scm:(((((uber-badf")
      (List
         (Concept "1")
         (Concept "2"))))