(use-modules (guile-llama-cpp ecma262)
(srfi srfi-64)
(ice-9 format))
(test-begin "ecma262-integration")
(test-assert "ecma262-features-available"
(ecma262-features-available?))
(test-case "basic-javascript-eval"
(let-values (((success result) (javascript-eval "2 + 2")))
(test-assert "javascript-eval-success" success)
(test-equal "javascript-eval-result" 4 result)))
(test-case "javascript-string-processing"
(let-values (((success result)
(javascript-eval "'hello world'.toUpperCase()")))
(test-assert "string-processing-success" success)
(test-equal "string-processing-result" "HELLO WORLD" result)))
(test-case "javascript-array-processing"
(let-values (((success result)
(javascript-eval "[1,2,3].map(x => x * 2)")))
(test-assert "array-processing-success" success)
(test-equal "array-processing-result" '(2 4 6) result)))
(test-assert "ecma262-context-creation"
(string? (create-ecma262-context)))
(test-case "modern-javascript-syntax"
(let-values (((success result)
(javascript-eval "
const obj = { a: 1, b: 2 }
const { a, b } = obj
const sum = (x, y) => x + y
sum(a, b)
")))
(test-assert "modern-syntax-success" success)
(test-equal "modern-syntax-result" 3 result)))
(test-case "template-literals"
(let-values (((success result)
(javascript-eval "
const name = 'ECMA-262'
const version = 'ES2023'
`${name} ${version}`
")))
(test-assert "template-literals-success" success)
(test-equal "template-literals-result" "ECMA-262 ES2023" result)))
(test-case "promise-syntax"
(let-values (((success result)
(javascript-eval "
const p = new Promise(resolve => resolve('success'))
p.constructor.name
")))
(test-assert "promise-syntax-success" success)
(test-equal "promise-syntax-result" "Promise" result)))
(test-case "ecma262-llm-context"
(let-values (((success result)
(javascript-eval (string-append
(create-ecma262-context)
"\nECMA.llm.prompt('test')"))))
(test-assert "ecma262-llm-context-success" success)
(test-assert "ecma262-llm-context-structure"
(and (hash-table? result)
(equal? (hash-ref result "type") "llm-call")
(equal? (hash-ref result "operation") "prompt")))))
(test-end "ecma262-integration")
(format #t "~%=== ECMA-262 Integration Test Results ===~%")
(format #t "Node.js available: ~a~%" (ecma262-features-available?))
(format #t "ECMA-262 context size: ~a characters~%"
(string-length (create-ecma262-context)))
(format #t "Integration tests completed.~%")