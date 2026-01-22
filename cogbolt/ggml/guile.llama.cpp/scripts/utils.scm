(use-modules (ice-9 iconv))
(define* (llama-token-to-piece-return-string ctx token #:optional (special #t))
  (let* ((check #f)
         (tbv (make-bytevector 8 0))
         (n-tokens (llama-token-to-piece (llama-get-model ctx) token
					 tbv special))
         (results #f))
    (if (< n-tokens 0)
        (begin
          (set! n-tokens (- n-tokens))
          (set! results (make-bytevector  n-tokens))
          (set! check (llama-token-to-piece (llama-get-model ctx) token
					    results special)))
        (begin
          (set! results (make-bytevector n-tokens))
          (bytevector-copy! tbv 0 results 0 n-tokens)))
    (utf8->string results)))
(define (llama-batch-clear batch)
  (llama-batch-n-tokens-set batch 0))