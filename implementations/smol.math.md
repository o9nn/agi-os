# Smol Agent Protocol: Mathematical Formulation

## 1. Problem Statement

The Smol Agent Protocol addresses the following constrained optimization problem:

### Optimization Problem

```
minimize    f(x)
subject to  g(x) = 0
            x ∈ X
```

where:
- `x ∈ X` represents a valid code program in the search space X
- `f: X → ℝ⁺` is the objective function: `f(x) = size(x)` measured in bytes
- `g: X → {0,1}` is the constraint function: `g(x) = functionality(x₀) ⊕ functionality(x)`
- `x₀` is the initial code (baseline)
- `⊕` denotes XOR (constraint satisfied when functionalities match)

## 2. Formal Definitions

### 2.1 Code Space

```
X = {x | x is syntactically valid code in language L}
```

### 2.2 Size Function

```
f: X → ℕ
f(x) = |x|  (number of bytes in representation of x)
```

### 2.3 Functionality Equivalence

```
functionality: X → F
F is the space of observable behaviors

g(x) = 0 ⟺ functionality(x) = functionality(x₀)
g(x) = 1 ⟺ functionality(x) ≠ functionality(x₀)
```

### 2.4 Transformation Space

```
T = {T₁, T₂, ..., Tₖ}  where Tᵢ: X → X

Common transformations:
- T₁: Syntax compaction (whitespace removal)
- T₂: Statement reduction (functional equivalence)
- T₃: Structural optimization (CSE, constant folding)
- T₄: Semantic equivalence (shorter idioms)
```

## 3. Algorithm

### 3.1 Iterative Optimization

```
Algorithm: Smol-Optimize(x₀, T, max_iter)
Input: x₀ ∈ X (initial code)
       T = {T₁, ..., Tₖ} (transformation set)
       max_iter ∈ ℕ (maximum iterations)
Output: x* ∈ X (optimized code)

1:  i ← 0
2:  xᵢ ← x₀
3:  size₀ ← f(x₀)
4:  while i < max_iter do
5:      xₜₑₘₚ ← Apply-Transformations(xᵢ, T)
6:      sizeₜₑₘₚ ← f(xₜₑₘₚ)
7:      tests ← Verify(xₜₑₘₚ)
8:      
9:      if tests = PASS ∧ sizeₜₑₘₚ < f(xᵢ) then
10:         xᵢ₊₁ ← xₜₑₘₚ        # ACCEPT
11:         i ← i + 1
12:         Report(i, f(xᵢ))
13:     else if tests = PASS ∧ sizeₜₑₘₚ = f(xᵢ) then
14:         # NEUTRAL - try alternative transformation
15:         break
16:     else
17:         # REJECT - converged
18:         break
19:     end if
20: end while
21: return xᵢ
```

### 3.2 Transformation Application

```
Apply-Transformations(x, T):
    x' ← x
    for each Tⱼ ∈ T do
        x' ← Tⱼ(x')
    end for
    return x'

Composition notation:
Apply-Transformations(x, {T₁, T₂, ..., Tₖ}) = Tₖ ∘ Tₖ₋₁ ∘ ... ∘ T₂ ∘ T₁(x)
```

### 3.3 Verification Function

```
Verify(x): X → {PASS, FAIL}

Verify(x) = PASS ⟺ ∀ test ∈ TestSuite: 
                      output(test, x) = output(test, x₀)
```

## 4. Decision Rule

### 4.1 Three-Way Classification

```
Decision(xᵢ, xₜₑₘₚ):
    
    verified ← Verify(xₜₑₘₚ) = PASS
    improved ← f(xₜₑₘₚ) < f(xᵢ)
    
    return {
        ACCEPT   if verified ∧ improved
        NEUTRAL  if verified ∧ ¬improved
        REJECT   otherwise
    }
```

### 4.2 Acceptance Predicate

```
Accept(xᵢ, xₜₑₘₚ) ⟺ [g(xₜₑₘₚ) = 0] ∧ [f(xₜₑₘₚ) < f(xᵢ)]
```

## 5. Convergence Properties

### 5.1 Monotonic Decrease

```
Theorem (Size Monotonicity):
If xᵢ₊₁ = Accept(xᵢ, Tⱼ(xᵢ)) for some Tⱼ ∈ T,
then f(xᵢ₊₁) < f(xᵢ)

Corollary:
The sequence {f(x₀), f(x₁), f(x₂), ...} is strictly decreasing.
```

### 5.2 Termination

```
Theorem (Guaranteed Termination):
Since f: X → ℕ is bounded below (f(x) ≥ 1 for non-empty code),
and the sequence {f(xᵢ)} is strictly decreasing when not converged,
the algorithm must terminate in finite time.

Proof:
- Initial size: f(x₀) = n₀ ∈ ℕ
- Lower bound: f(x) ≥ 1
- Strict decrease: f(xᵢ₊₁) < f(xᵢ) for each accepted transformation
- Maximum iterations: n₀ - 1
∴ Algorithm terminates in at most n₀ - 1 iterations. □
```

### 5.3 Optimality

```
Definition (Local Optimum):
x* is a local optimum if:
    ∀ T ∈ T: [f(T(x*)) ≥ f(x*)] ∨ [g(T(x*)) ≠ 0]

The algorithm converges to a local optimum with respect to T.
```

### 5.4 Convergence Criterion

```
Converged(xₙ, T) ⟺ ∀ T ∈ T: ¬Accept(xₙ, T(xₙ))

Equivalently:
Converged(xₙ, T) ⟺ ∀ T ∈ T: 
    [f(T(xₙ)) ≥ f(xₙ)] ∨ [g(T(xₙ)) ≠ 0]
```

## 6. Performance Metrics

### 6.1 Size Reduction

```
Reduction(x₀, x*) = f(x₀) - f(x*)

Reduction%(x₀, x*) = 100 × (f(x₀) - f(x*)) / f(x₀)
```

### 6.2 Compression Ratio

```
CompressionRatio(x₀, x*) = f(x*) / f(x₀)

Optimal: CompressionRatio → 0 (smaller is better)
```

### 6.3 Efficiency

```
Efficiency = Reduction(x₀, x*) / Iterations

Measures average byte reduction per optimization step
```

## 7. Constraints and Invariants

### 7.1 Hard Constraints

```
C₁: ∀ i: g(xᵢ) = 0                    (Functionality preservation)
C₂: ∀ i: xᵢ ∈ X                       (Syntactic validity)
C₃: ∀ i < n: f(xᵢ₊₁) ≤ f(xᵢ)         (Non-increasing size)
```

### 7.2 Invariants

```
I₁: functionality(xᵢ) = functionality(x₀)  ∀ i
I₂: TestSuite(xᵢ) = PASS                    ∀ i
I₃: f(xₙ) ≤ f(x₀)                           (Final size ≤ initial size)
```

## 8. Complexity Analysis

### 8.1 Time Complexity

```
Per iteration:
- Apply transformations: O(|T| × |x|)
- Verify functionality: O(TestCost)
- Total per iteration: O(|T| × |x| + TestCost)

Overall:
- Worst case iterations: O(|x₀|)
- Total: O(|x₀| × (|T| × |x₀| + TestCost))
- Simplified: O(|x₀|² × |T| + |x₀| × TestCost)
```

### 8.2 Space Complexity

```
O(|x₀|)  for storing code versions
```

## 9. Properties

### 9.1 Correctness

```
Theorem (Correctness):
If the algorithm terminates with output x*, then:
1. functionality(x*) = functionality(x₀)  (Functionality preserved)
2. f(x*) ≤ f(x₀)                         (Size non-increasing)
3. x* is a local optimum w.r.t. T         (Convergence)

Proof: By construction and invariants I₁, I₂, I₃. □
```

### 9.2 Safety

```
Theorem (Safety):
At every iteration i, if xᵢ₊₁ is accepted:
    Verify(xᵢ₊₁) = PASS
∴ No incorrect code is ever accepted.

Proof: By decision rule line 9 in algorithm. □
```

## 10. Extensions

### 10.1 Multi-objective Optimization

```
minimize    (f₁(x), f₂(x), ..., fₘ(x))
subject to  g(x) = 0

where:
- f₁(x) = size(x)
- f₂(x) = readability(x)
- f₃(x) = complexity(x)
```

### 10.2 Stochastic Optimization

```
Incorporate randomization in transformation selection:
    Tᵢ ~ Categorical(T, p)
    
where p is a probability distribution over T.
```

### 10.3 Parallel Optimization

```
Apply transformations in parallel:
    candidates = {T₁(x), T₂(x), ..., Tₖ(x)}
    x' = argmin{f(c) : c ∈ candidates ∧ g(c) = 0}
```

---

## References

- Constraint Optimization Theory
- Code Golf Techniques
- Program Transformation Systems
- Software Testing and Verification
