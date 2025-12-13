# Deep Tree Echo × OpenCog AtomSpace Integration

## Overview

This directory contains the integration of the **Deep Tree Echo State Reservoir Computer** with **OpenCog's AtomSpace** cognitive architecture.

## Files

### Documentation

- **`../../docs/OPENCOG_ATOMSPACE_INTEGRATION.md`** - Complete integration architecture
  - AtomSpace representation design
  - Mapping to URE, PLN, ECAN, MOSES, DeSTIN
  - Performance considerations
  - Future extensions

### Code

- **`atomspace_integration.scm`** - Complete Atomese implementation
  - Rooted trees as hypergraph atoms
  - B-series genomes with coefficients
  - P-system membranes
  - Echo state reservoirs
  - J-surface manifold
  - Integration with OpenCog subsystems

## Prerequisites

### OpenCog Installation

Install OpenCog (tested with OpenCog 5.x):

```bash
# Ubuntu/Debian
sudo apt-get install opencog

# Or build from source
git clone https://github.com/opencog/atomspace.git
cd atomspace
mkdir build && cd build
cmake ..
make -j4
sudo make install
```

### Guile Scheme

OpenCog uses Guile Scheme:

```bash
sudo apt-get install guile-3.0
```

## Running the Example

### Basic Execution

```bash
cd examples/DeepTreeEcho
guile -l atomspace_integration.scm
```

### Interactive Mode

```bash
guile
```

Then in the Guile REPL:

```scheme
(load "atomspace_integration.scm")

;; Inspect atoms
(cog-prt-atomspace)

;; Query rooted trees
(cog-get-atoms 'RootedTreeNode)

;; Get genome coefficients
(cog-incoming-by-type genome-gen0 'CoefficientLink)

;; Spread attention
(spread-attention dte-reservoir)

;; Evolve genome
(define child (crossover-genomes genome-gen0 genome-gen0))
(mutate-genome child 0.1)
```

## Architecture Mapping

### Deep Tree Echo → AtomSpace

| Component | AtomSpace Representation |
|-----------|-------------------------|
| Rooted Trees | `RootedTreeNode` atoms with `TreeStructureLink` |
| B-Series Genome | `BSeriesGenomeNode` with `CoefficientLink` to trees |
| P-System Membranes | `MembraneNode` with `MembraneParentLink` hierarchy |
| Echo State Reservoir | `ReservoirNode` with `NeuronNode` and `WeightLink` |
| J-Surface | `JSurfaceNode` with `ManifoldPointLink` and `GradientLink` |
| Complete Reservoir | `DeepTreeEchoNode` with `ReservoirComponentLink` |

### OpenCog Subsystems

| Subsystem | Application |
|-----------|-------------|
| **URE** | B-series order condition verification |
| **PLN** | Uncertain fitness reasoning |
| **ECAN** | Attention-based resource allocation |
| **MOSES** | Genome evolution and optimization |
| **DeSTIN** | Hierarchical temporal learning |
| **Pattern Miner** | Tree motif discovery |

## Example Queries

### Find All Trees of Order 4

```scheme
(cog-get-atoms 'RootedTreeNode)
```

### Get Genome Coefficients

```scheme
(define coeffs (cog-incoming-by-type genome-gen0 'CoefficientLink))
(for-each
  (lambda (link)
    (display (cog-name (gadr link)))  ; Tree name
    (display " -> ")
    (display (cog-name (gddr link)))  ; Coefficient
    (newline))
  coeffs)
```

### Find High-Fitness Reservoirs

```scheme
(cog-execute!
  (BindLink
    (TypedVariableLink
      (VariableNode "$res")
      (TypeNode 'DeepTreeEchoNode))
    (EvaluationLink
      (PredicateNode "high-fitness")
      (VariableNode "$res"))
    (VariableNode "$res")))
```

### Spread Attention

```scheme
(spread-attention dte-reservoir)
(display "Genome attention: ")
(display (cog-av-sti genome-gen0))
```

### Evolve Population

```scheme
;; Create population
(define population
  (map (lambda (i)
         (create-bseries-genome 
           (string-append "genome_" (number->string i))
           all-trees
           0
           (random:uniform)))
       (iota 10)))

;; Evaluate fitness
(for-each
  (lambda (genome)
    (display (cog-name genome))
    (display ": ")
    (display (evaluate-fitness genome))
    (newline))
  population)

;; Crossover
(define child (crossover-genomes (car population) (cadr population)))

;; Mutate
(mutate-genome child 0.1)
```

## URE Inference Example

### Define Rule Base

```scheme
(use-modules (opencog ure))

;; Create rule base for B-series
(define bseries-rbs (ConceptNode "bseries-rule-base"))

;; Add order condition rule
(ure-add-rule bseries-rbs order-condition-rule)

;; Run forward chaining
(cog-fc bseries-rbs genome-gen0)
```

## PLN Reasoning Example

### Fitness Prediction

```scheme
(use-modules (opencog pln))

;; Define implication
(ImplicationLink (stv 0.9 0.8)
  (EvaluationLink
    (PredicateNode "high-fitness")
    (VariableNode "$reservoir"))
  (EvaluationLink
    (PredicateNode "good-performance")
    (VariableNode "$reservoir")))

;; Evidence
(EvaluationLink (stv 0.75 0.9)
  (PredicateNode "high-fitness")
  dte-reservoir)

;; PLN deduction
(pln-bc
  (EvaluationLink
    (PredicateNode "good-performance")
    dte-reservoir))
```

## ECAN Attention Dynamics

### Attention Spreading

```scheme
;; High-fitness reservoir spreads attention
(spread-attention dte-reservoir)

;; Check component attention
(display "Component attention values:\n")
(display "  Genome: ")
(display (cog-av-sti genome-gen0))
(newline)
(display "  Membrane: ")
(display (cog-av-sti membrane-1))
(newline)
(display "  J-Surface: ")
(display (cog-av-sti jsurface-gen0))
(newline)
```

### Attention Decay

```scheme
;; Decay attention on all atoms
(define (decay-attention-all rate)
  (for-each
    (lambda (atom)
      (cog-set-av! atom
        (av (* (cog-av-sti atom) (- 1.0 rate))
            (cog-av-lti atom)
            (cog-av-vlti atom))))
    (cog-get-all-atoms)))

(decay-attention-all 0.1)  ; 10% decay
```

## MOSES Evolution

### Evolve Population

```scheme
;; Initialize population
(define population
  (map (lambda (i)
         (create-bseries-genome 
           (string-append "genome_gen0_" (number->string i))
           all-trees 0 (random:uniform)))
       (iota 20)))

;; Evolution loop
(define (evolve-generation pop)
  ;; Evaluate fitness
  (let ((fitnesses (map evaluate-fitness pop)))
    
    ;; Selection (tournament)
    (let ((parents
            (map (lambda (i)
                   (let ((a (list-ref pop (random 20)))
                         (b (list-ref pop (random 20))))
                     (if (> (evaluate-fitness a) (evaluate-fitness b))
                       a b)))
                 (iota 20))))
      
      ;; Crossover and mutation
      (map (lambda (i)
             (let ((child (crossover-genomes
                            (list-ref parents i)
                            (list-ref parents (modulo (+ i 1) 20)))))
               (mutate-genome child 0.1)
               child))
           (iota 20)))))

;; Run evolution
(define gen1 (evolve-generation population))
(define gen2 (evolve-generation gen1))
(define gen3 (evolve-generation gen2))
```

## DeSTIN Hierarchical Learning

### Belief Propagation

```scheme
;; Bottom-up pass
(define (bottom-up-pass layer input)
  ;; Compute beliefs from input
  (let ((beliefs (compute-beliefs input)))
    (EvaluationLink
      (PredicateNode "layer-beliefs")
      (ListLink layer beliefs))))

;; Top-down pass
(define (top-down-pass layer prior)
  ;; Modulate beliefs with prior
  (let ((modulated (modulate-beliefs layer prior)))
    modulated))

;; Full hierarchy pass
(define (destin-pass input)
  ;; Bottom-up
  (let ((l3-beliefs (bottom-up-pass destin-layer-3 input)))
    (let ((l2-beliefs (bottom-up-pass destin-layer-2 l3-beliefs)))
      (let ((l1-beliefs (bottom-up-pass destin-layer-1 l2-beliefs)))
        
        ;; Top-down
        (let ((l2-modulated (top-down-pass destin-layer-2 l1-beliefs)))
          (let ((l3-modulated (top-down-pass destin-layer-3 l2-modulated)))
            l3-modulated))))))
```

## Pattern Mining

### Find Common Tree Patterns

```scheme
;; Find trees appearing in high-fitness genomes
(define high-fitness-trees
  (cog-execute! find-high-fitness-trees))

;; Count occurrences
(define (count-tree-occurrences tree)
  (length
    (filter
      (lambda (genome)
        (not (null?
          (cog-get-link 'CoefficientLink genome tree))))
      population)))

;; Find most common trees
(define tree-counts
  (map (lambda (tree)
         (cons tree (count-tree-occurrences tree)))
       all-trees))

(define sorted-trees
  (sort tree-counts
        (lambda (a b) (> (cdr a) (cdr b)))))
```

## Performance Tips

### Atom Count Management

```scheme
;; Check atom count
(display "Total atoms: ")
(display (cog-count-atoms))
(newline)

;; Clear low-attention atoms
(define (clear-low-attention threshold)
  (for-each
    (lambda (atom)
      (if (< (cog-av-sti atom) threshold)
        (cog-delete atom)))
    (cog-get-all-atoms)))
```

### Efficient Queries

```scheme
;; Use typed variables for faster matching
(BindLink
  (TypedVariableLink
    (VariableNode "$tree")
    (TypeNode 'RootedTreeNode))  ; Type constraint
  (InheritanceLink
    (VariableNode "$tree")
    (TreeOrderNode "order_4"))
  (VariableNode "$tree"))
```

## Visualization

### Export to GraphViz

```scheme
;; Export AtomSpace to DOT format
(define (export-to-dot filename)
  (with-output-to-file filename
    (lambda ()
      (display "digraph DeepTreeEcho {\n")
      
      ;; Nodes
      (for-each
        (lambda (atom)
          (display "  \"")
          (display (cog-name atom))
          (display "\" [label=\"")
          (display (cog-type atom))
          (display "\\n")
          (display (cog-name atom))
          (display "\"];\n"))
        (cog-get-all-atoms))
      
      ;; Edges
      (for-each
        (lambda (link)
          (for-each
            (lambda (target)
              (display "  \"")
              (display (cog-name link))
              (display "\" -> \"")
              (display (cog-name target))
              (display "\";\n"))
            (cog-outgoing-set link)))
        (cog-get-atoms 'Link))
      
      (display "}\n"))))

(export-to-dot "deeptreeecho.dot")
```

Then visualize:

```bash
dot -Tpng deeptreeecho.dot -o deeptreeecho.png
```

## Troubleshooting

### Issue: Atoms not found

```scheme
;; Check if atom exists
(cog-node 'RootedTreeNode "tree_1_1")

;; List all atom types
(cog-get-types)
```

### Issue: Link creation fails

```scheme
;; Ensure atoms exist before linking
(define tree (cog-node 'RootedTreeNode "tree_1_1"))
(if (null? tree)
  (set! tree (RootedTreeNode "tree_1_1")))
```

### Issue: Memory usage high

```scheme
;; Clear unused atoms
(cog-atomspace-clear)

;; Or use attention-based forgetting
(clear-low-attention 10)
```

## Further Reading

- [OpenCog Wiki](https://wiki.opencog.org/)
- [AtomSpace Documentation](https://github.com/opencog/atomspace)
- [URE Tutorial](https://wiki.opencog.org/w/URE)
- [PLN Tutorial](https://wiki.opencog.org/w/PLN)
- [ECAN Documentation](https://wiki.opencog.org/w/ECAN)

## Citation

If you use this integration in your research:

```bibtex
@software{deeptreeecho_atomspace2024,
  title = {Deep Tree Echo × OpenCog AtomSpace Integration},
  author = {CogPy Team},
  year = {2024},
  url = {https://github.com/cogpy/cogpilot.jl}
}
```

---

🌳 **"The tree remembers in the hypergraph, and the echoes propagate through attention."**
