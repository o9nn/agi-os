(use-modules (srfi srfi-1))
(use-modules (ice-9 rdelim))
(use-modules (opencog) (opencog nlp) (opencog nlp learn))
(if (not (equal? 3 (length (program-arguments))))
	(begin
		(format #t
			"Usage: guile -s dict-comp.scm <dict-name> <sentence-file-name>\n")
		(exit #f)))
(define test-dict (second (program-arguments)))
(define sent-file (third (program-arguments)))
(if (not (equal? (stat:type (stat test-dict)) 'directory))
	(begin
		(format #t "Cannot find dictionary ~A\n" test-dict)
		(exit #f)))
(if (not (access? sent-file R_OK))
	(begin
		(format #t "Cannot find sentence file ~A\n" sent-file)
		(exit #f)))
(format #t "Verifying dicationary \"~A\" with sentences from \"~A\"\n"
	test-dict sent-file)
(define compare
	(make-lg-comparator (LgDictNode "en") (LgDictNode test-dict)
		#:INCLUDE-MISSING #t))
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
(format #t "Finished verifying dictionary \"~A\" with sentences from \"~A\"\n"
	test-dict sent-file)