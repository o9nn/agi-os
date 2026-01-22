(def measure-size (fn [filepath]
(length (slurp filepath))))
(def verify-functionality (fn [filepath]
(and
(= 0 (shell "node" "-c" filepath))
(= 0 (shell "npm" "test")))))
(def syntax-compaction (fn [code]
(-> code
(replace #"\s+" "")
(replace #"function\s+(\w+)" "f="))))
(def statement-reduction (fn [code]
(replace code #"function\s*\(([^)]*)\)\s*{" "$1=>{")))
(def structural-optimization (fn [code] code))
(def semantic-equivalence (fn [code] code))
(def optimize-iteration (fn [code filepath transforms]
(let [original-size (count code)
transformed (reduce #(%2 %1) code transforms)
new-size (count transformed)]
(spit filepath transformed)
(if (and (verify-functionality filepath)
(< new-size original-size))
{:status :accept :code transformed :size new-size}
{:status :reject :code code :size original-size}))))
(def minimize-code (fn [filepath]
(let [code (slurp filepath)
transforms [syntax-compaction
statement-reduction
structural-optimization
semantic-equivalence]]
(println "Initial size:" (count code) "bytes")
(loop [c code
v 0]
(let [result (optimize-iteration c filepath transforms)]
(if (= (:status result) :accept)
(do
(println "v" v ":" (:size result) "bytes")
(recur (:code result) (inc v)))
(println "Converged at" (count c) "bytes")))))))
(def principles
[:functionality-is-sacred
:measure-everything
:verify-continuously
:version-iteratively
:embrace-reversibility
:converge-systematically])
(def decision-rule (fn [func-preserved size-reduced]
(cond
(and func-preserved size-reduced) :accept
(and func-preserved (not size-reduced)) :neutral
:else :reject)))
