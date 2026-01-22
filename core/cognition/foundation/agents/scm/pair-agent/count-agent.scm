(use-modules (opencog) (opencog exec) (opencog persist))
(use-modules (opencog nlp) (opencog nlp lg-parse))
(use-modules (opencog learn))
(use-modules (opencog sensory))
(use-modules (srfi srfi-1))
(define (setup-parser STORAGE)
"
Create parser attached to storage.
"
(make-random-pair-parser
(ValueOf (Anchor "parse pipe") (Predicate "text src"))
STORAGE)
)
(define (obs-file FILE-NAME PARSER)
(define sensor (Sensory (string-append "file://" FILE-NAME)))
(cog-execute!
(SetValue
(Anchor "parse pipe") (Predicate "text src")
(Open (Type 'TextFileStream) sensor)))
(define (looper) (cog-execute! PARSER) (looper))
(catch #t looper
(lambda (key . args) (format #t "The end ~A\n" key)))
(cog-extract-recursive! sensor)
)
#|
(use-modules (opencog persist-rocks))
(define storage-node (MonoStorageNode "monospace:///tmp/foo.rdb"))
(cog-open storage-node)
(define parser (setup-parser storage-node))
(obs-file "/tmp/demo.txt" parser)
(cog-report-counts)
(cog-get-atoms 'Word)
(cog-execute! (ValueOf (Word "is") (Predicate "*-TruthValueKey-*")))
(cog-get-atoms 'Edge)
(cog-execute! (ValueOf
(car (cog-get-atoms 'Edge))
(Predicate "*-TruthValueKey-*")))
(extract-type 'WordNode)
(cog-close storage-node)
(use-modules (opencog persist-rocks))
(define storage-node (MonoStorageNode "monospace:///tmp/foo.rdb"))
(cog-open storage-node)
(load-atomspace)
(cog-report-counts)
|#