(use-modules (srfi srfi-1))
(define (fetch-all-gram-classes)
	(load-atoms-of-type 'WordClassNode)
	(for-each
		(lambda (CLS) (fetch-incoming-by-type CLS 'MemberLink))
		(cog-get-atoms 'WordClassNode)))
(cog-count-atoms 'WordClassNode)
(cog-get-atoms 'WordClassNode)
(define (num-classified-words)
	(define (nmemb CLS) (length (cog-incoming-by-type CLS 'MemberLink)))
	(fold (lambda (CLS cnt) (+ cnt (nmemb CLS))) 0
		(cog-get-atoms 'WordClassNode)))
(define (prt-class-summary)
	(define (nmemb CLS) (length (cog-incoming-by-type CLS 'MemberLink)))
	(define all-classes (cog-get-atoms 'WordClassNode))
	(define by-size
		(sort! all-classes
			(lambda (CLS-A CLS-B) (> (nmemb CLS-A) (nmemb CLS-B)))))
	(define multi-cnt 0)
	(define single-cnt 0)
	(define hi-cnt 10000000000)
	(format #t "There are ~A words placed into ~A classes\n"
		(num-classified-words) (cog-count-atoms 'WordClassNode))
	(for-each
		(lambda (CLS)
			(if (< 1 (nmemb CLS))
				(set! multi-cnt (+ multi-cnt 1))
				(set! single-cnt (+ single-cnt 1))))
		by-size)
	(format #t "There are ~A classes with multiple members, and ~A singltons\n"
		multi-cnt single-cnt)
	(for-each
		(lambda (CLS)
			(define n (nmemb CLS))
			(if (< 1 n)
				(if (< n hi-cnt)
					(begin
						(set! hi-cnt n)
						(format #t "\nClasses with ~A members: <~A>"
							n (cog-name CLS)))
					(format #t " <~A>" (cog-name CLS)))))
		by-size)
	(newline)
)
(define (prt-members-of-class CLS)
	(define membs (cog-incoming-by-type CLS 'MemberLink))
	(define words (map gar membs))
	(define words-by-freq
		(sort! words
			(lambda (WRD-A WRD-B) (> (get-count WRD-A) (get-count WRD-B)))))
	(format #t "Class <~A> has ~A members:\n   "
		(cog-name CLS) (length words-by-freq))
	(for-each
		(lambda (wrd)
			(format #t "~A " (cog-name wrd)))
		words-by-freq)
	(newline))
(define (prt-all-classes)
	(define (nmemb CLS) (length (cog-incoming-by-type CLS 'MemberLink)))
	(define all-classes (cog-get-atoms 'WordClassNode))
	(define by-size
		(sort! all-classes
			(lambda (CLS-A CLS-B) (> (nmemb CLS-A) (nmemb CLS-B)))))
	(format #t "There are ~A words placed into ~A classes\n"
		(num-classified-words) (cog-count-atoms 'WordClassNode))
	(for-each prt-members-of-class by-size))
(define (prt-class-mebership WRD)
	(define membs (cog-incoming-by-type WRD 'MemberLink))
	(define classes (map gdr membs))
	(format #t "Word '~A' belongs to ~A classes:\n   "
		(cog-name WRD) (length classes))
	(for-each
		(lambda (cls) (format #t "<~A> " (cog-name cls)))
		classes)
	(newline))
(define (prt-multi-members)
	(define nwrds 0)
	(define ducls '())
	(define (summer WRD)
		(define membs (cog-incoming-by-type WRD 'MemberLink))
		(if (< 1 (length membs))
			(begin
				(prt-class-mebership WRD)
				(set! nwrds (+ nwrds 1))
				(set! ducls (append ducls membs))))
	)
	(for-each summer (cog-get-atoms 'WordNode))
	(format #t "total words=~A total classes=~A unique classes=~A\n"
		nwrds (length ducls)
		(length (remove-duplicate-atoms (map gdr ducls))))
)
(define (prt-distribution FUNC)
	(define all-classes (cog-get-atoms 'WordClassNode))
	(define by-size
		(sort! all-classes
			(lambda (CLS-A CLS-B) (> (FUNC CLS-A) (FUNC CLS-B)))))
	(define cnt 1)
	(for-each
		(lambda (CLS)
			(format #t "~A	~A\n" cnt (FUNC CLS))
			(set! cnt (+ cnt 1)))
		by-size))
(define (prt-word-distribution)
	(define (nmemb CLS) (length (cog-incoming-by-type CLS 'MemberLink)))
	(prt-distribution nmemb))
(define (get-count atom)
	(if (cog-atom? atom) (cog-count atom) 0))
(define (prt-disjunct-support-distribution)
	(define (nmemb CLS) (length (cog-incoming-by-type CLS 'Section)))
	(prt-distribution nmemb))
(define (prt-disjunct-count-distribution)
	(define (nmemb CLS)
		(fold (lambda (SECT cnt) (+ cnt (get-count SECT))) 0
 			(cog-incoming-by-type CLS 'Section)))
	(prt-distribution nmemb))
(define (prt-disjunct-length-distribution)
	(define (nmemb CLS) (sqrt
		(fold (lambda (SECT cnt) (+ cnt
			(* (get-count SECT) (get-count SECT)))) 0
 			(cog-incoming-by-type CLS 'Section))))
	(prt-distribution nmemb))
(define (prt-dj-size-distribution)
	(define (num-sections SIZ)
		(fold (lambda (CLS SUM)
				(+ SUM (length (get-sections-by-size CLS SIZ))))
			0
			(cog-get-atoms 'WordClassNode)))
	(define (prt-dist SIZ)
		(format #t "~A	~A\n" SIZ (num-sections SIZ)))
	(format #t "disjunct-size vs num-disjuncts\n")
	(list-tabulate 15 prt-dist)
)
(define (prt-dj-weighted-size-distribution)
	(define (sum-section-weights SEC-LST)
		(fold (lambda (SEC SUM) (+ SUM (get-count SEC))) 0 SEC-LST))
	(define (weighted-num-sections SIZ)
		(fold (lambda (CLS SUM)
				(+ SUM (sum-section-weights (get-sections-by-size CLS SIZ))))
			0
			(cog-get-atoms 'WordClassNode)))
	(define (prt-dist SIZ)
		(format #t "~A	~A\n" SIZ (weighted-num-sections SIZ)))
	(format #t "disjunct-size vs weighted-num-disjuncts\n")
	(list-tabulate 15 prt-dist)
)