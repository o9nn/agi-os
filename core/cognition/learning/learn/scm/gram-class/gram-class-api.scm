(use-modules (srfi srfi-1))
(use-modules (opencog))
(use-modules (opencog persist))
(use-modules (opencog matrix))
(define-public (add-gram-class-api LLOBJ)
"
  add-gram-class-api LLOBJ -- Enable (WordClass, disjunct) pairs.
  This will take LLOBJ and extend it's native `left-type with the
  `WordClass` type, so that the left type can be either.  It also
  provides methods for managing the `MemberLink`s that indicate
  membership of the 'left-type in the `WordClass`.
  The membership of a word to a WordClass is denoted as
      (MemberLink (WordNode \"foo\") (WordClassNode \"bar\"))
  Keep in mind that a word might belong to more than one WordClass.
  Contributions to the class are stored as counts on the MemberLink.
  See the `pseudo-csets.scm` file for a general overview.
  Provided methods:
    'left-type -- returns (TypeChoice (LLOBJ 'left-type) (Type 'WordClassNode))
    'store-aux -- Store the MemberLinks above.
    'fetch-pairs -- Fetch both WordClassNodes and MemberLinks.
    'cluster-type -- returns (Type 'WordClassNode)
    'get-clusters -- return all left-basis elements that are of
                     cluster type.
    'make-cluster WA WB -- Creates a WordClassNode
"
	(define (get-left-type)
		(TypeChoice (LLOBJ 'left-type) (Type 'WordClassNode)))
	(define any-left (AnyNode "gram-class"))
	(define (get-left-wildcard DJ) (ListLink any-left DJ))
	(define any-right (LLOBJ 'right-element (LLOBJ 'wild-wild)))
	(define (get-wild-wild) (ListLink any-left any-right))
	(define (fetch-disjuncts)
		(LLOBJ 'fetch-pairs)
		(define start-time (current-time))
		(load-atoms-of-type 'WordClassNode)
		(for-each
			(lambda (wcl)
				(fetch-incoming-by-type wcl 'MemberLink))
			(cog-get-atoms 'WordClassNode))
		(fetch-incoming-set any-left)
		(format #t "Elapsed time to load grammatical classes: ~A secs\n"
			(- (current-time) start-time)))
	(define (store-aux)
		(for-each
			(lambda (memb-list)
				(for-each
					(lambda (memb)
						(if (eq? 'WordNode (cog-type (gar memb)))
							(store-atom memb)))
					memb-list))
			(map (lambda (wrdcls) (cog-incoming-by-type wrdcls 'MemberLink))
				(cog-get-atoms 'WordClassNode))))
	(define (get-cluster-type) (Type 'WordClassNode))
	(define (get-clusters)
		(define stars (add-pair-stars LLOBJ))
		(filter (lambda (W) (equal? 'WordClassNode (cog-type W)))
			(stars 'left-basis)))
	(define (make-cluster A-ATOM B-ATOM)
		(define is-a-class (eq? 'WordClassNode (cog-type A-ATOM)))
		(define is-b-class (eq? 'WordClassNode (cog-type B-ATOM)))
		(cond
			(is-a-class A-ATOM)
			(is-b-class B-ATOM)
			(else (let
					((cluname (string-join
						(list (cog-name A-ATOM) (cog-name B-ATOM)))))
				(every
					(lambda (N)
						(if (nil? (cog-node 'WordClassNode cluname)) #f
							(begin
								(set! cluname (string-append cluname " (dup)"))
								#t)))
					(iota 10000))
				(WordClassNode cluname)))))
	(define (describe)
		(display (procedure-property add-gram-class-api 'documentation)))
	(define (provides meth)
		(case meth
			((left-type)        get-left-type)
			((store-aux)        store-aux)
			(else #f)
	))
	(lambda (message . args)
		(apply (case message
			((name)           (lambda () "WordClass-Disjunct Pairs"))
			((id)             (lambda () "gram-class"))
			((left-type)      get-left-type)
			((left-wildcard)  get-left-wildcard)
			((wild-wild)      get-wild-wild)
			((fetch-pairs)    fetch-disjuncts)
			((store-aux)      store-aux)
			((cluster-type)   get-cluster-type)
			((get-clusters)   get-clusters)
			((make-cluster)   make-cluster)
			((provides)       provides)
			((filters?)       (lambda () #f))
			((describe)       describe)
			((help)           describe)
			((obj)            (lambda () "add-gram-class-api"))
			((base)           (lambda () LLOBJ))
			(else             (lambda ( . rest ) (apply LLOBJ (cons message args))))
		) args))
)