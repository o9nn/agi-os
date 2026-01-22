#!/usr/bin/env guile
!#
(use-modules (srfi srfi-1)
(ice-9 format))
(define *a000081-cache* (make-vector 1000 #f))
(vector-set! *a000081-cache* 0 0)
(vector-set! *a000081-cache* 1 1)
(define (compute-divisors n)
"Return list of all positive divisors of n"
(if (<= n 1)
(list 1)
(let loop ((d 1) (divisors '()))
(cond
((> d n) (reverse divisors))
((= (modulo n d) 0) (loop (+ d 1) (cons d divisors)))
(else (loop (+ d 1) divisors))))))
(define (sum-of-divisors k a-fn)
"Compute ∑_{d|k} d·a_d"
(let ((divisors (compute-divisors k)))
(apply + (map (lambda (d) (* d (a-fn d))) divisors))))
(define (a000081-recursive n)
"Compute A000081(n) using the recursive formula"
(cond
((< n 0) 0)
((= n 0) 0)
((= n 1) 1)
((vector-ref *a000081-cache* n)
(vector-ref *a000081-cache* n))
(else
(let* ((result
(let loop ((k 1) (sum 0))
(if (> k (- n 1))
(/ sum (- n 1))
(let* ((divisor-sum (sum-of-divisors k a000081-recursive))
(a-term (a000081-recursive (- n k)))
(term (* divisor-sum a-term)))
(loop (+ k 1) (+ sum term)))))))
(vector-set! *a000081-cache* n (inexact->exact (round result)))
(vector-ref *a000081-cache* n)))))
(define (a000081-nth n)
"Get the nth term of the A000081 sequence"
(a000081-recursive n))
(define (a000081-sequence n)
"Generate the first n terms of A000081 sequence"
(map a000081-nth (iota n)))
(define *alpha* 2.9557652857)
(define *C* 0.4399237)
(define (asymptotic-approximation n)
"Asymptotic approximation: a_n ~ C·α^n·n^{-3/2}"
(if (<= n 0)
0
(* *C* (expt *alpha* n) (expt n -1.5))))
(define (display-sequence n)
"Display the first n terms of A000081"
(format #t "A000081 sequence (first ~a terms):~%" n)
(let ((seq (a000081-sequence n)))
(format #t "~{~a~^, ~}~%" seq)
(format #t "~%")))
(define (generating-function-coeffs x terms)
"Compute coefficients for the generating function A(x) = ∑ a_n x^n"
(let ((seq (a000081-sequence terms)))
(apply + (map (lambda (n an) (* an (expt x n)))
(iota terms) seq))))
(define (main)
"Main demonstration of A000081 computations"
(format #t "=== Ghost in the Guile Shell ===~%")
(format #t "A000081: Number of unlabeled rooted trees~%~%")
(display-sequence 15)
(format #t "Specific computations:~%")
(do ((i 1 (+ i 1)))
((> i 10))
(format #t "a(~a) = ~a~%" i (a000081-nth i)))
(format #t "~%")
(format #t "Asymptotic approximation comparison:~%")
(format #t "n\tExact\tAsymptotic\tRatio~%")
(do ((i 5 (+ i 1)))
((> i 12))
(let ((exact (a000081-nth i))
(approx (asymptotic-approximation i)))
(format #t "~a\t~a\t~,3f\t\t~,4f~%"
i exact approx (/ approx exact))))
(format #t "~%")
(format #t "Generating function A(0.1) ≈ ~,6f~%"
(generating-function-coeffs 0.1 20))
(format #t "Generating function A(0.2) ≈ ~,6f~%"
(generating-function-coeffs 0.2 20)))
(when (and (defined? 'command-line) (not (null? (command-line))))
(main))