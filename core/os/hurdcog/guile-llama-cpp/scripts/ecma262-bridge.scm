#! /bin/sh
# -*- mode: scheme
install_dir=$(dirname $0)/..
local_lib_dir=${install_dir}/share/guile/site/3.0/scripts  #follow make install directory hierarchy of autotools
export GUILE_LOAD_PATH=${install_dir}:${local_lib_dir}:${GUILE_LOAD_PATH}
export LD_LIBRARY_PATH=${install_dir}/lib:${LD_LIBRARY_PATH}
exec guile -e main -s "$0" "$@"
!#
(use-modules (rnrs)
             (system foreign)
             (system vm trace)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 match)
             (json))
(include-from-path "init_common.scm")
(use-modules (guile-llama-cpp))
(include-from-path "utils.scm")
(define (execute-javascript code)
  "Execute JavaScript code using Node.js and return the result"
  (let* ((js-wrapper (string-append
                     "try { "
                     "const result = (" code "); "
                     "console.log(JSON.stringify({ success: true, result: result })); "
                     "} catch (error) { "
                     "console.log(JSON.stringify({ success: false, error: error.message })); "
                     "}"))
         (pipe (open-input-output-pipe "node -e \"" js-wrapper "\""))
         (result-str (read-line pipe)))
    (close-pipe pipe)
    (if (eof-object? result-str)
        #f
        (json-string->scm result-str))))
(define (create-llm-js-context model-path)
  "Create a JavaScript context with LLM functionality"
  (let* ((llm-api-js (string-append
                     "const llmAPI = {
                       modelPath: '" model-path "',
                       prompt: function(text) {
                         // This will be handled by the Scheme bridge
                         return { type: 'llm-prompt', text: text }
                       },
                       tokenize: function(text) {
                         return { type: 'llm-tokenize', text: text }
                       },
                       generate: function(prompt, options = {}) {
                         return { type: 'llm-generate', prompt: prompt, options: options }
                       }
                     }
                     // ECMA-262 features for LLM interaction
                     const ecma262LLM = {
                       async: {
                         prompt: async function(text) {
                           return new Promise((resolve) => {
                             resolve(llmAPI.prompt(text))
                           })
                         }
                       },
                       // Array methods for token processing
                       processTokens: function(tokens) {
                         return tokens.map(token => ({ 
                           id: token, 
                           processed: true 
                         }))
                       },
                       // String methods for prompt processing
                       processPrompt: function(prompt) {
                         return prompt
                           .trim()
                           .split('\\n')
                           .filter(line => line.length > 0)
                           .join(' ')
                       },
                       // Object destructuring and modern syntax
                       createConfig: function({ temperature = 0.7, maxTokens = 100 } = {}) {
                         return { temperature, maxTokens }
                       }
                     }
                     // Export API
                     globalThis.llmAPI = llmAPI
                     globalThis.ecma262LLM = ecma262LLM
    llm-api-js))
(define (execute-llm-javascript code model-path)
  "Execute JavaScript code with LLM context"
  (let* ((context-js (create-llm-js-context model-path))
         (full-js (string-append context-js "\n" code))
         (result (execute-javascript full-js)))
    (if result
        (let* ((success (assoc-ref result "success"))
               (js-result (if success
                             (assoc-ref result "result")
                             (assoc-ref result "error"))))
          (if success
              (handle-llm-js-result js-result model-path)
              (error "JavaScript execution failed:" js-result)))
        (error "Failed to execute JavaScript"))))
(define (handle-llm-js-result js-result model-path)
  "Handle JavaScript result and execute LLM operations if needed"
  (cond
   ((and (hash-table? js-result) 
         (equal? (hash-ref js-result "type") "llm-prompt"))
    (let ((prompt-text (hash-ref js-result "text")))
      (llm-prompt prompt-text model-path)))
   ((and (hash-table? js-result)
         (equal? (hash-ref js-result "type") "llm-generate"))
    (let ((prompt-text (hash-ref js-result "prompt"))
          (options (hash-ref js-result "options")))
      (llm-generate prompt-text model-path options)))
   (else
    js-result)))
(define (llm-prompt prompt-text model-path)
  "Execute LLM prompt using existing simple prompt functionality"
  (prompt prompt-text model-path))
(define (llm-generate prompt-text model-path options)
  "Execute LLM generation with options"
  (prompt prompt-text model-path))
(define (ecma262-llm-demo model-path)
  "Demonstrate ECMA-262 features with LLM integration"
  (format #t "=== ECMA-262 + LLM Integration Demo ===~%")
  (format #t "~%Example 1: Basic JavaScript LLM call~%")
  (let ((js-code "llmAPI.prompt('What is artificial intelligence?')"))
    (format #t "JavaScript: ~a~%" js-code)
    (let ((result (execute-llm-javascript js-code model-path)))
      (format #t "Result: ~a~%" result)))
  (format #t "~%Example 2: ECMA-262 async features~%")
  (let ((js-code "ecma262LLM.async.prompt('Explain machine learning in one sentence')"))
    (format #t "JavaScript: ~a~%" js-code)
    (let ((result (execute-llm-javascript js-code model-path)))
      (format #t "Result: ~a~%" result)))
  (format #t "~%Example 3: Modern JavaScript syntax~%")
  (let ((js-code "
    const config = ecma262LLM.createConfig({ temperature: 0.8, maxTokens: 50 })
    const prompt = ecma262LLM.processPrompt(`
      What is the future of AI?
      Please be concise.
    `)
    llmAPI.generate(prompt, config)"))
    (format #t "JavaScript: ~a~%" js-code)
    (let ((result (execute-llm-javascript js-code model-path)))
      (format #t "Result: ~a~%" result)))
  (format #t "~%Example 4: Token processing with arrays~%")
  (let ((js-code "
    const tokens = [1, 2, 3, 4, 5]
    const processed = ecma262LLM.processTokens(tokens)
    processed.map(token => `Token ${token.id}: ${token.processed ? 'OK' : 'FAIL'}`).join(', ')"))
    (format #t "JavaScript: ~a~%" js-code)
    (let ((result (execute-llm-javascript js-code model-path)))
      (format #t "Result: ~a~%" result))))
(define (main args)
  (cond
   ((< (length args) 2)
    (format #t "Usage: ~a [demo|exec] <model-path> [javascript-code]~%" (car args))
    (format #t "  demo: Run ECMA-262 integration demonstration~%")
    (format #t "  exec: Execute JavaScript code with LLM integration~%")
    (exit 1))
   ((and (>= (length args) 3)
         (string=? (cadr args) "demo"))
    (let ((model-path (caddr args)))
      (ecma262-llm-demo model-path)))
   ((and (>= (length args) 4)
         (string=? (cadr args) "exec"))
    (let ((model-path (caddr args))
          (js-code (cadddr args)))
      (format #t "Executing JavaScript with LLM integration:~%")
      (format #t "JavaScript: ~a~%" js-code)
      (let ((result (execute-llm-javascript js-code model-path)))
        (format #t "Result: ~a~%" result))))
   (else
    (format #t "Invalid command. Use 'demo' or 'exec'.~%")
    (exit 1)))
  (exit 0))