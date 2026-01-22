(setlocale LC_CTYPE "")
(setlocale LC_NUMERIC "C")
(define-module (opencog))
(if (resolve-module (list 'opencog 'as-config) #:ensure #f)
	(use-modules (opencog as-config))
	(load-from-path "opencog/as-config.scm"))
(load-extension (string-append opencog-ext-path-smob "libsmob") "opencog_guile_init")
(export
cog-add-atomspace
cog-arity
cog-atom
cog-atom?
cog-atom-less?
cog-atomspace
cog-atomspace?
cog-atomspace-clear
cog-atomspace-cow!
cog-atomspace-cow?
cog-atomspace-env
cog-atomspace-readonly?
cog-atomspace-ro!
cog-atomspace-rw!
cog-atomspace-uuid
cog-confidence
cog-count
cog-count-atoms
cog-equal?
cog-extract!
cog-extract-recursive!
cog-get-subtypes
cog-get-types
cog-handle
cog-inc-count!
cog-incoming-by-type
cog-incoming-set
cog-incoming-size
cog-incoming-size-by-type
cog-inc-value!
cog-keys
cog-keys->alist
cog-link
cog-link?
cog-map-type
cog-mean
cog-name
cog-new-ast
cog-new-atom
cog-new-atomspace
cog-new-link
cog-new-node
cog-new-value
cog-node
cog-node?
cog-number
cog-outgoing-atom
cog-outgoing-by-type
cog-outgoing-set
cog-set-atomspace!
cog-set-server-mode!
cog-set-tv!
cog-set-value!
cog-set-value-ref!
cog-set-values!
cog-subtype?
cog-tv
cog-tv-confidence
cog-tv-count
cog-tv-mean
cog-type
cog-type->int
cog-update-value!
cog-value
cog-value?
cog-value->list
cog-value-ref
cog-value-type
)
(define (cpp-exception-printer port key args default-printer)
	(format port "Atomspace C++ exception:\n~A\n" args))
(set-exception-printer! 'C++-EXCEPTION cpp-exception-printer)
(define-public cog-initial-as (cog-atomspace))
(define-public my-as (cog-atomspace))
(if (nil? cog-initial-as)
	(begin
		(set! cog-initial-as (cog-new-atomspace))
		(cog-set-atomspace! cog-initial-as)))
(define-public (cog-as ATOM) "See cog-atomspace" (cog-atomspace ATOM))
(define-public (cog-extract ATOM) "See cog-extract!" (cog-extract! ATOM))
(define-public (cog-extract-recursive ATOM)
	"See cog-extract-recursive!" (cog-extract-recursive! ATOM))
(define-public (cog-remove ATOM) "See cog-extract!" (cog-extract! ATOM))
(define-public (alist . x) (list 'alist x))
(include-from-path "opencog/base/core_types.scm")
(define-public (TypeSet . x)
	(apply cog-new-link (cons TypeIntersectionLinkType x)))
(define-public (TypeSetLink . x)
	(apply cog-new-link (cons TypeIntersectionLinkType x)))
(include-from-path "opencog/base/core-docs.scm")
(include-from-path "opencog/base/atom-docs.scm")
(include-from-path "opencog/base/utilities.scm")
(include-from-path "opencog/base/atom-cache.scm")
(include-from-path "opencog/base/apply.scm")
(include-from-path "opencog/base/tv.scm")
(include-from-path "opencog/base/types.scm")
(include-from-path "opencog/base/file-utils.scm")
(include-from-path "opencog/base/debug-trace.scm")
(define-public (cog-undefined-handle) "obsolete function" '())