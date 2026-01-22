(use-modules (guix packages)
             ((guix licenses) #:prefix license:)
             (guix git-download)
             (guix gexp)
             (guix build-system cmake)
             (guix build-system gnu)
             (guix packages)
             (guix utils)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages gettext)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages libffi)
             (gnu packages machine-learning)
             (gnu packages pkg-config)
             (gnu packages swig)
             (gnu packages texinfo)
             (ice-9 match))
(define %source-dir (dirname (current-filename)))
(define-public llama-cpp-cpu-only
  (let ((base llama-cpp))
    (package/inherit base
      (arguments
        (substitute-keyword-arguments (package-arguments base)
          ((#:configure-flags configure-flags)
           #~(cons*  "-DCMAKE_POSITION_INDEPENDENT_CODE=TRUE" #$configure-flags))))
      (native-inputs (list pkg-config)))))
(package
  (name "guile_llama_cpp")
  (version "0.2")
  (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
  (build-system gnu-build-system)
  (arguments `())
  (native-inputs
    (list autoconf
      automake
      libtool
      pkg-config
      swig-next))
  (inputs (list
  		  guile-3.0
  		  guile-lib
  		  llama-cpp-cpu-only))
  (synopsis "")
  (description "")
  (home-page "")
  (license license:lgpl3+))