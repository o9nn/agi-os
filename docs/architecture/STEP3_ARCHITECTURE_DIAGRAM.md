# Step 3: Hierarchical Planning - Architecture Diagram

## System Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         AGI-OS Cognitive Architecture                        │
└─────────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────┐
│ Layer 4: Goal-Directed Behavior (STEP 3)                                    │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌──────────────────────────────────────────────────────────────────────┐  │
│  │ Hierarchical Planning System                                         │  │
│  │                                                                      │  │
│  │  ┌─────────────────┐  ┌──────────────────┐  ┌──────────────────┐  │  │
│  │  │ Goal            │  │ Planning         │  │ Execution        │  │  │
│  │  │ Representation  │→ │ Algorithms       │→ │ Monitoring       │  │  │
│  │  │                 │  │                  │  │                  │  │  │
│  │  │ • Strategic     │  │ • HTN Planning   │  │ • Progress       │  │  │
│  │  │ • Tactical      │  │ • Constraints    │  │   Tracking       │  │  │
│  │  │ • Operational   │  │ • Optimization   │  │ • Deviation      │  │  │
│  │  │                 │  │ • Robustness     │  │   Detection      │  │  │
│  │  └─────────────────┘  └──────────────────┘  └──────────────────┘  │  │
│  │         ↑                      ↑                      ↓             │  │
│  │         │                      │                      │             │  │
│  │         └──────────────────────┴──────────────────────┘             │  │
│  │                   Plan Adaptation Loop                              │  │
│  │                                                                      │  │
│  └──────────────────────────────────────────────────────────────────────┘  │
│         ↑                                                  ↓                 │
│         │                                                  │                 │
└─────────┼──────────────────────────────────────────────────┼─────────────────┘
          │                                                  │
          │ Goals                                            │ Actions
          │                                                  │
┌─────────┴──────────────────────────────────────────────────┴─────────────────┐
│ Layer 3: Autognostic Reasoning (STEP 2)                                      │
├──────────────────────────────────────────────────────────────────────────────┤
│                                                                               │
│  ┌──────────────────────────────────────────────────────────────────────┐   │
│  │ LLM-Based Diagnostic Analysis                                        │   │
│  │                                                                      │   │
│  │  ┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐  │   │
│  │  │ Diagnostic       │  │ LLM Service      │  │ Suggestion       │  │   │
│  │  │ Serialization    │→ │ (Mistral 7B)     │→ │ Engine           │  │   │
│  │  │                  │  │                  │  │                  │  │   │
│  │  │ • Atom-to-Text   │  │ • Inference      │  │ • Parsing        │  │   │
│  │  │ • Context        │  │ • Validation     │  │ • Ranking        │  │   │
│  │  │   Injection      │  │ • Explanation    │  │ • Filtering      │  │   │
│  │  └──────────────────┘  └──────────────────┘  └──────────────────┘  │   │
│  │         ↑                      ↑                      ↓              │   │
│  │         │                      │                      │              │   │
│  │         └──────────────────────┴──────────────────────┘              │   │
│  │              Feedback Loop: Outcome → Heuristics                     │   │
│  │                                                                      │   │
│  └──────────────────────────────────────────────────────────────────────┘   │
│         ↑                                                  ↓                  │
│         │                                                  │                  │
└─────────┼──────────────────────────────────────────────────┼──────────────────┘
          │                                                  │
          │ Diagnostics                                      │ Suggestions
          │                                                  │
┌─────────┴──────────────────────────────────────────────────┴──────────────────┐
│ Layer 2: Self-Awareness (STEP 1)                                             │
├───────────────────────────────────────────────────────────────────────────────┤
│                                                                               │
│  ┌──────────────────────────────────────────────────────────────────────┐   │
│  │ Diagnostic Atom Framework                                            │   │
│  │                                                                      │   │
│  │  ┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐  │   │
│  │  │ ECAN/PLN         │  │ Diagnostic       │  │ Persistence      │  │   │
│  │  │ Integration      │→ │ Atom Generation  │→ │ & Retrieval      │  │   │
│  │  │                  │  │                  │  │                  │  │   │
│  │  │ • Error          │  │ • Real-time      │  │ • AtomSpace      │  │   │
│  │  │ • Prediction     │  │   generation     │  │   Storage        │  │   │
│  │  │   Mismatch       │  │ • Categorization │  │ • Indexing       │  │   │
│  │  │ • Stall          │  │ • Relationships  │  │ • Queries        │  │   │
│  │  └──────────────────┘  └──────────────────┘  └──────────────────┘  │   │
│  │                                                                      │   │
│  └──────────────────────────────────────────────────────────────────────┘   │
│         ↑                                                  ↓                  │
│         │                                                  │                  │
└─────────┼──────────────────────────────────────────────────┼──────────────────┘
          │                                                  │
          │ System State                                     │ Diagnostics
          │                                                  │
┌─────────┴──────────────────────────────────────────────────┴──────────────────┐
│ Layer 1: Infrastructure (Foundation)                                         │
├───────────────────────────────────────────────────────────────────────────────┤
│                                                                               │
│  ┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐           │
│  │ AtomSpace        │  │ CogServer        │  │ Build System     │           │
│  │                  │  │                  │  │                  │           │
│  │ • Hypergraph DB  │  │ • Networking     │  │ • CMake          │           │
│  │ • Indexing       │  │ • Caching        │  │ • Debian Pkg     │           │
│  │ • Queries        │  │ • Distribution   │  │ • CI/CD           │           │
│  └──────────────────┘  └──────────────────┘  └──────────────────┘           │
│                                                                               │
└───────────────────────────────────────────────────────────────────────────────┘
```

---

## Step 3 Internal Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                    Step 3: Hierarchical Planning System                      │
└─────────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────┐
│ INPUT: Diagnostic Atoms from Step 1 & LLM Suggestions from Step 2           │
└─────────────────────────────────────────────────────────────────────────────┘
                                    ↓
┌─────────────────────────────────────────────────────────────────────────────┐
│ Module 1: Goal Representation                                               │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  Input: Diagnostic atoms, LLM suggestions                                   │
│                                                                              │
│  ┌────────────────────────────────────────────────────────────────────┐    │
│  │ Goal Generation                                                    │    │
│  │                                                                    │    │
│  │  Diagnostic Atom → Goal Converter                                 │    │
│  │  ├─ Error Diagnostic → ErrorRecoveryGoal                          │    │
│  │  ├─ Prediction Mismatch → OptimizationGoal                        │    │
│  │  └─ Stall Category → PerformanceGoal                              │    │
│  │                                                                    │    │
│  │  LLM Suggestion → Goal Converter                                  │    │
│  │  ├─ Repair Suggestion → OperationalGoal                           │    │
│  │  └─ Repair Explanation → Goal Description                         │    │
│  └────────────────────────────────────────────────────────────────────┘    │
│                                    ↓                                        │
│  ┌────────────────────────────────────────────────────────────────────┐    │
│  │ Goal Decomposition                                                 │    │
│  │                                                                    │    │
│  │  Strategic Goal Decomposition                                     │    │
│  │  ├─ Strategic Goal → Multiple Tactical Goals                      │    │
│  │  └─ Constraint Propagation                                        │    │
│  │                                                                    │    │
│  │  Tactical Goal Decomposition                                      │    │
│  │  ├─ Tactical Goal → Multiple Operational Goals                    │    │
│  │  └─ Dependency Analysis                                           │    │
│  │                                                                    │    │
│  │  Completeness Verification                                        │    │
│  │  ├─ All subgoals cover parent goal                                │    │
│  │  └─ No missing objectives                                         │    │
│  └────────────────────────────────────────────────────────────────────┘    │
│                                    ↓                                        │
│  ┌────────────────────────────────────────────────────────────────────┐    │
│  │ Goal Relationship Management                                       │    │
│  │                                                                    │    │
│  │  Relationship Types:                                              │    │
│  │  ├─ Prerequisite (A must complete before B)                       │    │
│  │  ├─ Conflict (A and B cannot execute simultaneously)              │    │
│  │  ├─ Enablement (A enables B)                                      │    │
│  │  ├─ Resource Sharing (A and B compete for resources)              │    │
│  │  └─ Temporal (sequential, parallel, conditional)                  │    │
│  │                                                                    │    │
│  │  Conflict Detection & Resolution                                  │    │
│  │  ├─ Identify conflicting goals                                    │    │
│  │  ├─ Resolve through prioritization                                │    │
│  │  └─ Suggest alternative goals                                     │    │
│  └────────────────────────────────────────────────────────────────────┘    │
│                                    ↓                                        │
│  Output: Goal Hierarchy with relationships and constraints                  │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
                                    ↓
┌─────────────────────────────────────────────────────────────────────────────┐
│ Module 2: Planning Algorithms                                               │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  Input: Goal Hierarchy with constraints                                     │
│                                                                              │
│  ┌────────────────────────────────────────────────────────────────────┐    │
│  │ Hierarchical Task Network (HTN) Planning                           │    │
│  │                                                                    │    │
│  │  Task Network Construction                                        │    │
│  │  ├─ Convert goals to tasks                                        │    │
│  │  ├─ Define task decomposition methods                             │    │
│  │  └─ Build task network                                            │    │
│  │                                                                    │    │
│  │  Task Ordering                                                    │    │
│  │  ├─ Respect prerequisite constraints                              │    │
│  │  ├─ Support parallel execution                                    │    │
│  │  └─ Handle conditional execution                                  │    │
│  │                                                                    │    │
│  │  Primitive Task Identification                                    │    │
│  │  ├─ Identify executable tasks                                     │    │
│  │  └─ Map to actions                                                │    │
│  └────────────────────────────────────────────────────────────────────┘    │
│                                    ↓                                        │
│  ┌────────────────────────────────────────────────────────────────────┐    │
│  │ Constraint-Based Planning                                          │    │
│  │                                                                    │    │
│  │  Constraint Types:                                                │    │
│  │  ├─ Resource Constraints (memory, CPU, time)                      │    │
│  │  ├─ Temporal Constraints (deadlines, precedence)                  │    │
│  │  └─ Logical Constraints (mutual exclusion, dependencies)          │    │
│  │                                                                    │    │
│  │  Constraint Satisfaction                                          │    │
│  │  ├─ Constraint propagation                                        │    │
│  │  ├─ Consistency checking                                          │    │
│  │  └─ Conflict resolution                                           │    │
│  │                                                                    │    │
│  │  Constraint Relaxation                                            │    │
│  │  ├─ For infeasible problems                                       │    │
│  │  ├─ Identify relaxable constraints                                │    │
│  │  └─ Generate alternative plans                                    │    │
│  └────────────────────────────────────────────────────────────────────┘    │
│                                    ↓                                        │
│  ┌────────────────────────────────────────────────────────────────────┐    │
│  │ Plan Quality Optimization                                          │    │
│  │                                                                    │    │
│  │  Cost Modeling                                                    │    │
│  │  ├─ Execution time cost                                           │    │
│  │  ├─ Resource consumption cost                                     │    │
│  │  └─ Reliability cost                                              │    │
│  │                                                                    │    │
│  │  Single-Objective Optimization                                    │    │
│  │  ├─ Minimize execution time                                       │    │
│  │  ├─ Minimize resource consumption                                 │    │
│  │  └─ Maximize reliability                                          │    │
│  │                                                                    │    │
│  │  Multi-Objective Optimization                                     │    │
│  │  ├─ Pareto frontier generation                                    │    │
│  │  ├─ Trade-off analysis                                            │    │
│  │  └─ User preference incorporation                                 │    │
│  └────────────────────────────────────────────────────────────────────┘    │
│                                    ↓                                        │
│  ┌────────────────────────────────────────────────────────────────────┐    │
│  │ Plan Robustness                                                    │    │
│  │                                                                    │    │
│  │  Failure Scenario Analysis                                        │    │
│  │  ├─ Identify potential failures                                   │    │
│  │  ├─ Estimate failure probabilities                                │    │
│  │  └─ Assess impact on goals                                        │    │
│  │                                                                    │    │
│  │  Contingency Plan Generation                                      │    │
│  │  ├─ Generate recovery strategies                                  │    │
│  │  ├─ Create alternative plans                                      │    │
│  │  └─ Implement fallback mechanisms                                 │    │
│  │                                                                    │    │
│  │  Uncertainty Handling                                             │    │
│  │  ├─ Probabilistic planning                                        │    │
│  │  ├─ Uncertainty propagation                                       │    │
│  │  └─ Risk assessment                                               │    │
│  └────────────────────────────────────────────────────────────────────┘    │
│                                    ↓                                        │
│  Output: Optimized, robust plan with contingencies                          │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
                                    ↓
┌─────────────────────────────────────────────────────────────────────────────┐
│ Module 3: Integration & Validation                                          │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  Input: Plan from planning algorithms                                       │
│                                                                              │
│  ┌────────────────────────────────────────────────────────────────────┐    │
│  │ Plan Validation via LLM (Step 2)                                   │    │
│  │                                                                    │    │
│  │  ├─ Validate plan feasibility                                     │    │
│  │  ├─ Verify resource estimates                                     │    │
│  │  ├─ Check for logical errors                                      │    │
│  │  └─ Assess expected effectiveness                                 │    │
│  └────────────────────────────────────────────────────────────────────┘    │
│                                    ↓                                        │
│  ┌────────────────────────────────────────────────────────────────────┐    │
│  │ Plan-to-Action Conversion                                          │    │
│  │                                                                    │    │
│  │  ├─ Convert plan tasks to executable actions                      │    │
│  │  ├─ Create action atoms                                           │    │
│  │  ├─ Set action parameters                                         │    │
│  │  └─ Define action dependencies                                    │    │
│  └────────────────────────────────────────────────────────────────────┘    │
│                                    ↓                                        │
│  Output: Validated, executable plan                                         │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
                                    ↓
┌─────────────────────────────────────────────────────────────────────────────┐
│ Module 4: Execution Monitoring & Adaptation                                 │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  Input: Executed plan actions and outcomes                                  │
│                                                                              │
│  ┌────────────────────────────────────────────────────────────────────┐    │
│  │ Execution Monitoring                                               │    │
│  │                                                                    │    │
│  │  Real-Time Tracking                                               │    │
│  │  ├─ Track execution of each action                                │    │
│  │  ├─ Monitor resource consumption                                  │    │
│  │  ├─ Detect action failures                                        │    │
│  │  └─ Measure progress toward goals                                 │    │
│  │                                                                    │    │
│  │  Metrics Collection                                               │    │
│  │  ├─ Execution time vs estimate                                    │    │
│  │  ├─ Resource usage vs budget                                      │    │
│  │  ├─ Success/failure indicators                                    │    │
│  │  └─ Goal achievement status                                       │    │
│  └────────────────────────────────────────────────────────────────────┘    │
│                                    ↓                                        │
│  ┌────────────────────────────────────────────────────────────────────┐    │
│  │ Deviation Detection                                                │    │
│  │                                                                    │    │
│  │  Deviation Analysis                                               │    │
│  │  ├─ Compare actual vs expected outcomes                           │    │
│  │  ├─ Classify deviation types                                      │    │
│  │  ├─ Assess impact on goal achievement                             │    │
│  │  └─ Determine if adaptation needed                                │    │
│  │                                                                    │    │
│  │  Deviation Handling                                               │    │
│  │  ├─ Minor deviations: Continue with monitoring                    │    │
│  │  ├─ Moderate deviations: Adjust plan                              │    │
│  │  └─ Major deviations: Activate contingency                        │    │
│  └────────────────────────────────────────────────────────────────────┘    │
│                                    ↓                                        │
│  ┌────────────────────────────────────────────────────────────────────┐    │
│  │ Plan Adaptation                                                    │    │
│  │                                                                    │    │
│  │  Adaptation Strategies                                            │    │
│  │  ├─ Reorder remaining actions                                     │    │
│  │  ├─ Reallocate resources                                          │    │
│  │  ├─ Activate contingency plans                                    │    │
│  │  └─ Generate new plans if necessary                               │    │
│  │                                                                    │    │
│  │  Adaptation Validation                                            │    │
│  │  ├─ Verify adapted plan feasibility                               │    │
│  │  ├─ Check resource availability                                   │    │
│  │  └─ Validate goal achievement potential                           │    │
│  └────────────────────────────────────────────────────────────────────┘    │
│                                    ↓                                        │
│  ┌────────────────────────────────────────────────────────────────────┐    │
│  │ Continuous Improvement                                             │    │
│  │                                                                    │    │
│  │  Learning from Outcomes                                           │    │
│  │  ├─ Capture execution results                                     │    │
│  │  ├─ Analyze planning effectiveness                                │    │
│  │  ├─ Identify improvement opportunities                            │    │
│  │  └─ Update planning heuristics                                    │    │
│  │                                                                    │    │
│  │  Knowledge Base Updates                                           │    │
│  │  ├─ Refine resource estimates                                     │    │
│  │  ├─ Update success probabilities                                  │    │
│  │  ├─ Improve goal representations                                  │    │
│  │  └─ Enhance planning heuristics                                   │    │
│  └────────────────────────────────────────────────────────────────────┘    │
│                                    ↓                                        │
│  Output: Improved planning knowledge for future plans                       │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
                                    ↓
┌─────────────────────────────────────────────────────────────────────────────┐
│ OUTPUT: Executed Actions & Feedback Loop                                    │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Data Flow Diagram

```
Step 1: Diagnostics          Step 2: LLM Suggestions       Step 3: Planning
─────────────────────────────────────────────────────────────────────────────

Diagnostic Atoms
    │
    ├─ Error Diagnostic
    ├─ Prediction Mismatch
    └─ Stall Category
         │
         ↓
    ┌─────────────────┐
    │ Serialization   │
    │ (Step 2)        │
    └────────┬────────┘
             │
             ↓
    ┌─────────────────┐
    │ LLM Service     │
    │ (Mistral 7B)    │
    └────────┬────────┘
             │
             ↓
    ┌─────────────────┐
    │ Repair          │
    │ Suggestions     │
    └────────┬────────┘
             │
             ├─ Suggestion Text
             ├─ Confidence Score
             ├─ Resource Estimate
             └─ Expected Impact
                  │
                  ↓
             ┌──────────────────────┐
             │ Goal Generation      │
             │ (Step 3)             │
             └────────┬─────────────┘
                      │
                      ↓
             ┌──────────────────────┐
             │ Goal Decomposition   │
             │ (Step 3)             │
             └────────┬─────────────┘
                      │
                      ↓
             ┌──────────────────────┐
             │ Planning Algorithms  │
             │ (Step 3)             │
             │ • HTN Planning       │
             │ • Constraints        │
             │ • Optimization       │
             └────────┬─────────────┘
                      │
                      ↓
             ┌──────────────────────┐
             │ Plan Validation      │
             │ (via Step 2 LLM)     │
             └────────┬─────────────┘
                      │
                      ↓
             ┌──────────────────────┐
             │ Executable Plan      │
             │ (Step 3)             │
             └────────┬─────────────┘
                      │
                      ↓
             ┌──────────────────────┐
             │ Action Execution     │
             │ (System)             │
             └────────┬─────────────┘
                      │
                      ↓
             ┌──────────────────────┐
             │ Execution Monitoring │
             │ (Step 3)             │
             └────────┬─────────────┘
                      │
                      ├─ Success → Feedback to Step 2
                      ├─ Deviation → Plan Adaptation
                      └─ Failure → Contingency Activation
```

---

## Component Interaction Matrix

| Component | Interacts With | Data Exchanged | Frequency |
|-----------|---|---|---|
| Goal Generation | Step 1 Diagnostics | Diagnostic atoms | Real-time |
| Goal Generation | Step 2 LLM | Repair suggestions | Real-time |
| Goal Decomposition | Goal Representation | Goal hierarchies | Real-time |
| HTN Planning | Goal Decomposition | Decomposed goals | Real-time |
| Constraint Planning | HTN Planning | Task networks | Real-time |
| Optimization | Constraint Planning | Constrained plans | Real-time |
| Robustness | Optimization | Optimized plans | Real-time |
| Plan Validation | Step 2 LLM | Plans for validation | Real-time |
| Execution Monitoring | Plan Execution | Action status | Real-time |
| Deviation Detection | Execution Monitoring | Execution metrics | Real-time |
| Plan Adaptation | Deviation Detection | Deviation info | On-demand |
| Continuous Improvement | Execution Results | Outcome data | Batch |
| Heuristic Updates | Continuous Improvement | Learned heuristics | Batch |

---

## Conclusion

The architecture diagram shows Step 3 as a sophisticated hierarchical planning system that:

1. **Receives input** from diagnostic atoms (Step 1) and LLM suggestions (Step 2)
2. **Generates goals** from diagnostics and suggestions
3. **Decomposes goals** into hierarchical structures
4. **Plans execution** using HTN, constraints, and optimization
5. **Validates plans** using LLM service
6. **Monitors execution** and adapts plans dynamically
7. **Learns continuously** from outcomes

The tight integration with Steps 1 and 2 creates a unified cognitive architecture where diagnosis informs planning, planning guides execution, and outcomes improve future planning.
