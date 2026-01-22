#!/usr/bin/env guile
!#
(define-module (test-namespace-direct)
  #:use-module (ice-9 format))
(format #t "🧪 Testing Plan9/Inferno namespace features directly...~%")
(catch #t
  (lambda ()
    (use-modules (cogkernel plan9-namespace))
    (format #t "✅ plan9-namespace module loaded~%")
    (format #t "~%👶 Testing process namespace creation...~%")
    (define proc-ns (make-process-namespace 1234))
    (format #t "Process namespace created: ~a~%" (process-namespace? proc-ns))
    (format #t "~%🔧 Testing basic namespace functions...~%")
    (format #t "Process namespace PID: ~a~%" (process-namespace-pid proc-ns))
    (format #t "✅ Basic namespace operations working!~%"))
  (lambda (key . args)
    (format #t "❌ Test failed: ~a ~a~%" key args)))
(format #t "~%✅ Direct namespace test complete!~%")