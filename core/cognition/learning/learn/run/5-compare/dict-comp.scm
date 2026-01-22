#! /usr/bin/env -S guile
!#
(use-modules (srfi srfi-1))
(use-modules (ice-9 rdelim))
(use-modules (opencog) (opencog nlp) (opencog learn))
(if (not (equal? 4 (length (program-arguments))))
	(begin
		(format #t
			"Usage: ~A <gold-dict> <test-dict> <sentence-file>\n"
			(first (program-arguments)))
		(exit #f)))
(define gold-dict (second (program-arguments)))
(define test-dict (third (program-arguments)))
(define sent-file (fourth (program-arguments)))
(if (not (equal? (stat:type (stat gold-dict)) 'directory))
	(begin
		(format #t "Cannot find reference dictionary ~A\n" gold-dict)
		(exit #f)))
(if (not (equal? (stat:type (stat test-dict)) 'directory))
	(begin
		(format #t "Cannot find test dictionary ~A\n" test-dict)
		(exit #f)))
(if (not (access? sent-file R_OK))
	(begin
		(format #t "Cannot find sentence file ~A\n" sent-file)
		(exit #f)))
(format #t "Comparing \"~A\" to \"~A\" with sentences from \"~A\"\n"
	gold-dict test-dict sent-file)
(define compare
	(make-lg-comparator (LgDictNode gold-dict) (LgDictNode test-dict) '()
		#:INCLUDE-MISSING #f))
(define (process-file PORT)
	(define line (read-line PORT))
	(if (not (eof-object? line))
		(begin
			(if (and
				(< 0 (string-length line))
				(not (equal? #\# (string-ref line 0)))
				(not (equal? #\! (string-ref line 0)))
				(not (equal? #\* (string-ref line 0)))
				(not (equal? #\% (string-ref line 0))))
				(compare line))
			(process-file PORT))
		(compare #f)))
(process-file (open sent-file O_RDONLY))
(format #t "Finiished comparing \"~A\" to \"~A\" with sentences from \"~A\"\n"
	gold-dict test-dict sent-file)