(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs))
(catch #t
(lambda () (use-modules (dbi dbi)))
(lambda (key . args)
(format #t "Error: guile-dbi interfaces missing:\n   ~A: ~A: ~A \n"
key (car args) (cadr args)) #f))
(use-modules (opencog))
(use-modules (opencog matrix))
(use-modules (opencog sheaf))
(use-modules (dbi dbi))
(define (number->tag num)
(define (number->letters num)
(define letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(unfold-right negative?
(lambda (i) (string-ref letters (remainder i 26)))
(lambda (i) (- (quotient i 26) 1))
num))
(list->string (cons #\T (number->letters num)))
)
(define get-cnr-name
(let* ((cnt 0)
(cache (make-afunc-cache
(lambda (WORD-PAIR)
(set! cnt (+ cnt 1))
(number->tag cnt)))))
(lambda (left-word right-word)
(cache (ListLink left-word right-word)))
)
)
(define (cset-to-lg-dj GERM CSET)
"
cset-to-lg-dj GERM CSET
Return a link-grammar compatible disjunct string for CSET,
in such a way that it can connect to GERM. Here, GERM should
be a WordNode or a WordClassNode, and CSET should be a
ConnectorSeq.
"
(define (cword-to-lg-con WORD DIR)
(string-append
(if (equal? DIR "-")
(get-cnr-name WORD GERM)
(get-cnr-name GERM WORD)
)
DIR
)
)
(define (cword-list-to-lg-con-list WRDLI DIR)
(if (eq? 1 (length WRDLI))
(cword-to-lg-con (car WRDLI) DIR)
(string-append "("
(fold
(lambda (WRD STR)
(string-append STR " or " (cword-to-lg-con WRD DIR)))
(cword-to-lg-con (car WRDLI) DIR)
(cdr WRDLI))
")"))
)
(define (connector-to-lg-cnr WRD-OR-CLA DIR)
(define wctype (cog-type WRD-OR-CLA))
(if (eq? 'WordClassNode wctype)
(cword-to-lg-con WRD-OR-CLA DIR)
(let* ((memb-list (cog-incoming-by-type WRD-OR-CLA 'MemberLink))
(cls-list (map gdr memb-list)))
(when (eq? 0 (length cls-list))
(format #t "Error: Word ~A not in any class\n" WRD-OR-CLA)
(throw 'bad-membership 'cset-to-lg-dj "Word not in class"))
(cword-list-to-lg-con-list cls-list DIR)))
)
(define (dj-append CONNECTOR dj)
(define word (gar CONNECTOR))
(define dir (cog-name (gdr CONNECTOR)))
(define cnr (connector-to-lg-cnr word dir))
(if (equal? dir "-")
(string-append cnr " & " dj)
(string-append dj " & " cnr)))
(fold
(lambda (CNR dj)
(if dj
(dj-append CNR dj)
(connector-to-lg-cnr (gar CNR) (cog-name (gdr CNR)))))
#f
(cog-outgoing-set CSET))
)
(define (make-db-adder DB-NAME LOCALE COST-FN)
(if (file-exists? DB-NAME)
(throw 'fail-create 'make-db-adder
(format #f
"Error: file '~A' exists; will not over-write.\n\tMaybe you should move it out of the way?" DB-NAME)))
(define db-obj (dbi-open "sqlite3" DB-NAME))
(define wrd-id 0)
(define nprt 0)
(define is-open #t)
(define start (current-time))
(define secs (current-time))
(define word-cache (make-atom-set))
(define warn-cnt 0)
(define (escquote STR BEG)
(define pos (string-index STR (lambda (C) (equal? C #\')) BEG))
(if pos
(escquote
(string-replace STR "''" pos pos 1 2)
(+ pos 2))
STR))
(define (add-one-word WORD-STR CLASS-STR)
(define word-str (escquote WORD-STR 0))
(define class-str (escquote CLASS-STR 0))
(if (string=? word-str "###LEFT-WALL###")
(set! word-str "LEFT-WALL"))
(define query-str
(format #f
"INSERT INTO Morphemes VALUES ('~A', '~A~C~D', '~A');"
word-str word-str #\etx wrd-id class-str))
(dbi-query db-obj query-str)
(when (not (equal? 0 (car (dbi-get_status db-obj))))
(format #t "sqlite3 failure on query=~A\n" query-str)
(throw 'fail-insert 'make-db-adder
(cdr (dbi-get_status db-obj))))
)
(define (mk-cls-str STR)
(format #f "{{~A}}" (escquote STR 0)))
(define (add-word-class CLASS-NODE)
(define cls-type (cog-type CLASS-NODE))
(set! wrd-id (+ wrd-id 1))
(cond
((eq? cls-type 'WordNode)
(let ((word-str (cog-name CLASS-NODE)))
(add-one-word word-str (mk-cls-str word-str))))
((eq? cls-type 'WordClassNode)
(let ((cls-str (mk-cls-str (cog-name CLASS-NODE))))
(for-each
(lambda (memb)
(add-one-word (cog-name (gar memb)) cls-str))
(cog-incoming-by-type CLASS-NODE 'MemberLink))))
(else
(throw 'fail-insert 'make-db-adder
"Must be either a WordNode or a WordClassNode")))
)
(define (add-germ-cset-pair GERM CSET COST)
(define germ-str (cog-name GERM))
(define dj-str (cset-to-lg-dj GERM CSET))
(set! nprt (+ nprt 1))
(if (equal? 0 (remainder nprt 5000))
(begin
(dbi-query db-obj "END TRANSACTION;")
(dbi-query db-obj "BEGIN TRANSACTION;")
))
(if (equal? 0 (remainder nprt 25000))
(begin
(format #t "~D done in ~D secs; inserting into <~A>: ~A;\n"
nprt (- (current-time) secs) germ-str dj-str)
(set! secs (current-time))
))
(if (not (word-cache GERM))
(add-word-class GERM))
(dbi-query db-obj (format #f
"INSERT INTO Disjuncts VALUES ('~A', '~A', ~F);"
(mk-cls-str germ-str) dj-str COST))
(let ((err-code (car (dbi-get_status db-obj)))
(err-msg (cdr (dbi-get_status db-obj))))
(if (not (equal? 0 err-code))
(if (string-prefix? "UNIQUE" err-msg)
(if (< warn-cnt 10)
(begin
(set! warn-cnt (+ 1 warn-cnt))
(format #t "Warning: ~A: Did you forget to classify the connectors?\n"
err-msg)))
(throw 'fail-insert 'make-db-adder err-msg))))
)
(define (add-section SECTION)
(if (eq? 'Section (cog-type SECTION))
(let ((germ (gar SECTION))
(cset (gdr SECTION))
(cost (COST-FN SECTION)))
(if (< cost 1.0e3)
(add-germ-cset-pair germ cset cost)))))
(define (shutdown)
(when is-open
(set! is-open #f)
(dbi-query db-obj "END TRANSACTION;")
(dbi-close db-obj)
(format #t "Finished inserting ~D records in ~D secs (~6F/sec)\n"
nprt (- (current-time) start)
(/ nprt (- (current-time) start)))))
(define (raii-add-section SECTION)
(with-throw-handler #t
(lambda () (add-section SECTION))
(lambda (key . args) (shutdown))))
(define (add-unknown-word-handler CLASS)
(set! wrd-id (+ wrd-id 1))
(dbi-query db-obj (format #f
"INSERT INTO Morphemes VALUES ('<UNKNOWN-WORD>', '<UNKNOWN-WORD.~D>', '~A');"
wrd-id (mk-cls-str (cog-name CLASS))))
(if (not (equal? 0 (car (dbi-get_status db-obj))))
(throw 'fail-insert 'make-db-adder
(cdr (dbi-get_status db-obj))))
)
(dbi-query db-obj (string-append
"CREATE TABLE Morphemes ( "
"morpheme TEXT NOT NULL, "
"subscript TEXT UNIQUE NOT NULL, "
"classname TEXT NOT NULL);" ))
(if (not (equal? 0 (car (dbi-get_status db-obj))))
(throw 'fail-create 'make-db-adder
(cdr (dbi-get_status db-obj))))
(dbi-query db-obj
"CREATE INDEX morph_idx ON Morphemes(morpheme);")
(dbi-query db-obj (string-append
"CREATE TABLE Disjuncts ("
"classname TEXT NOT NULL, "
"disjunct TEXT NOT NULL, "
"cost REAL, "
"UNIQUE(classname,disjunct) );"))
(dbi-query db-obj
"CREATE INDEX class_idx ON Disjuncts(classname);")
(dbi-query db-obj (string-append
"INSERT INTO Morphemes VALUES ("
"'<dictionary-version-number>', "
"'<dictionary-version-number>', "
"'<dictionary-version-number>');"))
(dbi-query db-obj (string-append
"INSERT INTO Disjuncts VALUES ("
"'<dictionary-version-number>', 'V5v9v0+', 0.0);"))
(dbi-query db-obj (string-append
"INSERT INTO Morphemes VALUES ("
"'<dictionary-locale>', "
"'<dictionary-locale>', "
"'<dictionary-locale>');"))
(dbi-query db-obj (string-append
"INSERT INTO Disjuncts VALUES ("
"'<dictionary-locale>', '"
(string-map (lambda (c) (if (equal? c #\_) #\4 c)) LOCALE)
"+', 0.0);"))
(dbi-query db-obj (string-append
"INSERT INTO Morphemes VALUES ("
"'<UNKNOWN-WORD>', "
"'<UNKNOWN-WORD>', "
"'<UNKNOWN-WORD>');"))
(dbi-query db-obj (string-append
"INSERT INTO Disjuncts VALUES ("
"'<UNKNOWN-WORD>', 'XXXBOGUS+', 0.0);"))
(dbi-query db-obj "PRAGMA synchronous = OFF;")
(dbi-query db-obj "PRAGMA journal_mode = MEMORY;")
(dbi-query db-obj "BEGIN TRANSACTION;")
(lambda (message . args)
(case message
((add-section)     (apply raii-add-section args))
((add-unknown)     (apply add-unknown-word-handler args))
((shutdown)        (shutdown))
)
)
)
(define*-public (export-csets CSETS DB-NAME LOCALE #:key
(INCLUDE-UNKNOWN #f))
"
export-csets CSETS DB-NAME LOCALE
Write connector sets to a Link Grammar-compatible sqlite3 file.
CSETS is a matrix containing the connector sets to be written.
DB-NAME is the database name to write to.
LOCALE is the locale to use
Optional keyword: #:INCLUDE-UNKNOWN If set to #t, then each word class
will also be exported as an UNKNOWN-WORD, allowing the LG parser to use
this word class when encountering a word that it does not know (i.e.
is not a part of the vocabulary.)
Note that link-grammar expects the database file to be called
\"dict.db\", always!
Example usage:
(define pca (make-pseudo-cset-api))
(define gca (make-gram-class-api pca))
(export-csets gca \"dict.db\" \"EN_us\")
In this example, it is assumed that a clustering step has been
performed, to group words into word-classes. The `gca` object is the
usual API to wordclass-disjunct pairs.
Example usage:
(define pca (make-pseudo-cset-api))
(define fca (add-subtotal-filter pca 50 50 10 #f))
(export-csets fca \"dict.db\" \"EN_us\" #:INCLUDE-UNKNOWN #t)
In this example, it is assumed that NO clustering has been done. Here,
`pca` is the usual API to word-disjunct pairs.  The subtotal filter
only admits those sections with a large-enough count. Caution: this
can result in HUGE dictionaries!
"
(define psa (add-pair-stars CSETS))
(define mi-source (add-pair-freq-api psa #:nothrow #t))
(define looper (add-loop-api psa))
(define (cost-fn SECTION)
(- (mi-source 'pair-fmi SECTION)))
(define multi-member-classes
(filter
(lambda (CLS)
(< 1 (length (cog-incoming-by-type CLS 'MemberLink))))
(psa 'left-basis)))
(define dbase (make-db-adder DB-NAME LOCALE cost-fn))
(define (sectioner SECTION) (dbase 'add-section SECTION))
(define cnt 0)
(define (cntr x) (set! cnt (+ cnt 1)))
(looper 'for-each-pair cntr)
(format #t "Will store ~D sections\n" cnt)
(looper 'for-each-pair sectioner)
(if INCLUDE-UNKNOWN
(begin
(format #t "Will store ~D unknown word classes\n"
(length multi-member-classes))
(for-each
(lambda (cls) (dbase 'add-unknown cls))
multi-member-classes))
(format #t "Skipping adding unknown-word classes\n"))
(dbase 'shutdown)
)