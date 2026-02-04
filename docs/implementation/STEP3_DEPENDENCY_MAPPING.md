# Step 3: Hierarchical Planning - Detailed Dependency Mapping

## Executive Summary

Step 3 depends on successful completion of Steps 1 and 2 in critical ways. This document maps each dependency, explains the impact of failures, and provides contingency strategies.

---

## Step 2 Dependency Matrix

### Category 1: Data Flow Dependencies

#### Dependency 1.1: Diagnostic Atoms → Goal Generation

**Source**: Step 1 (Diagnostic Atom Framework)  
**Consumer**: Step 3 (Goal Generation)  
**Data Flow**: Diagnostic atoms → Goal atoms

**What Step 3 Needs**:
- Diagnostic atoms with standardized schema
- Atom types: ErrorDiagnostic, PredictionMismatch, StallCategory
- Atom properties: severity, timestamp, affected_component, root_cause
- Atom relationships: causality, temporal ordering

**What Step 2 Provides**:
- Repair suggestions derived from diagnostics
- Confidence scores for each suggestion
- Natural language explanations
- Resource requirement estimates

**Integration Point**:
```
Step 1: Diagnostic Atoms
    ↓
Step 2: LLM Analysis → Repair Suggestions
    ↓
Step 3: Goal Generation from Suggestions
```

**Failure Scenario**: If Step 1 diagnostic atoms are inaccurate or incomplete:
- Step 2 generates poor repair suggestions
- Step 3 generates incorrect goals
- Plans pursue wrong objectives
- System performance degrades

**Mitigation**:
- Validate diagnostic atoms before goal generation
- Cross-check goals against system state
- Implement goal feasibility checking
- Use LLM to validate goal relevance to diagnostics

---

#### Dependency 1.2: LLM Suggestions → Planning Heuristics

**Source**: Step 2 (Repair Suggestion Engine)  
**Consumer**: Step 3 (Planning Heuristics)  
**Data Flow**: Repair suggestions → Planning heuristics

**What Step 3 Needs**:
- Repair suggestions with confidence scores
- Success rates for different repair types
- Resource requirements (memory, CPU, time)
- Repair effectiveness metrics

**What Step 2 Provides**:
- Parsed repair suggestions from LLM
- Confidence calibration
- Quality metrics
- Feedback loop results

**Integration Point**:
```
Step 2: Repair Suggestions + Metrics
    ↓
Step 3: Extract Heuristics
    ↓
Step 3: Use Heuristics in Planning
```

**Failure Scenario**: If Step 2 suggestions are low quality or metrics are inaccurate:
- Step 3 heuristics are misleading
- Planning produces suboptimal plans
- Resource estimates are incorrect
- Plans fail more frequently

**Mitigation**:
- Validate heuristics against execution outcomes
- Implement heuristic confidence scoring
- Use conservative estimates when uncertain
- Continuously update heuristics with new data

---

#### Dependency 1.3: Execution Feedback → Planning Improvement

**Source**: Step 2 (Feedback Loop)  
**Consumer**: Step 3 (Continuous Improvement)  
**Data Flow**: Execution outcomes → Heuristic updates

**What Step 3 Needs**:
- Execution outcome data for each repair
- Success/failure indicators
- Resource consumption measurements
- Time measurements
- Impact on system performance

**What Step 2 Provides**:
- Feedback collection system
- Outcome tracking
- Metrics calculation
- Analysis framework

**Integration Point**:
```
Step 3: Execute Plans
    ↓
Step 2: Collect Feedback
    ↓
Step 3: Update Heuristics
    ↓
Step 3: Improve Future Planning
```

**Failure Scenario**: If Step 2 feedback loop is not working:
- Step 3 cannot learn from outcomes
- Heuristics become stale
- Planning quality degrades over time
- System becomes less effective

**Mitigation**:
- Implement independent outcome tracking in Step 3
- Validate feedback data quality
- Detect and handle feedback anomalies
- Use multiple feedback sources

---

### Category 2: Service Dependencies

#### Dependency 2.1: LLM Service for Goal Validation

**Source**: Step 2 (LLM Service)  
**Consumer**: Step 3 (Goal Validation)  
**Service**: LLM inference via gRPC/HTTP

**What Step 3 Needs**:
- LLM service responding to planning queries
- Response latency: <30 seconds
- Availability: >99.5%
- Throughput: >100 requests/second

**What Step 2 Provides**:
- TensorRT-optimized LLM service
- gRPC and REST endpoints
- Request queuing and load balancing
- Monitoring and alerting

**Integration Point**:
```
Step 3: Generate Plan
    ↓
Step 3: Call LLM Service
    ↓
Step 2: LLM Validates Plan
    ↓
Step 3: Adjust Plan if Needed
```

**Failure Scenario**: If LLM service is unavailable or slow:
- Step 3 cannot validate plans
- Planning latency increases
- System becomes unresponsive
- Goals cannot be achieved

**Mitigation**:
- Implement fallback to heuristic validation
- Cache validation results
- Use optimistic validation (assume valid)
- Implement circuit breaker pattern
- Queue requests during high load

---

#### Dependency 2.2: AtomSpace Storage for Goal Persistence

**Source**: Step 2 (AtomSpace Storage Integration)  
**Consumer**: Step 3 (Goal Storage)  
**Service**: AtomSpace Storage backend

**What Step 3 Needs**:
- Persistent storage for goal atoms
- Query performance: <100ms for typical queries
- Availability: >99.5%
- Capacity: >1M goal atoms

**What Step 2 Provides**:
- AtomSpace Storage backend configured
- Proper indexing for goal queries
- Persistence layer
- Backup and recovery

**Integration Point**:
```
Step 3: Create Goal Atoms
    ↓
Step 2: Store in AtomSpace Storage
    ↓
Step 3: Query Goals from Storage
    ↓
Step 3: Retrieve Plans
```

**Failure Scenario**: If AtomSpace Storage is slow or unavailable:
- Step 3 cannot store goals
- Step 3 cannot retrieve goals
- Planning becomes impossible
- System state is lost

**Mitigation**:
- Implement in-memory goal cache
- Use asynchronous storage writes
- Implement storage failover
- Batch storage operations
- Monitor storage performance

---

#### Dependency 2.3: CogServer for Distributed Planning

**Source**: Step 2 (CogServer Integration)  
**Consumer**: Step 3 (Distributed Planning)  
**Service**: CogServer networking and caching

**What Step 3 Needs**:
- CogServer fully operational
- Remote goal access via CogServer
- Caching for frequently accessed goals
- Distributed planning support

**What Step 2 Provides**:
- CogServer integration complete
- Atom type definitions for LLM requests/responses
- Client implementation
- Caching layer

**Integration Point**:
```
Step 3: Distributed Planning
    ↓
Step 2: CogServer Networking
    ↓
Step 3: Access Remote Goals
```

**Failure Scenario**: If CogServer is not properly integrated:
- Step 3 cannot access remote goals
- Distributed planning fails
- System becomes isolated
- Scalability is limited

**Mitigation**:
- Implement local-first planning
- Use CogServer as optimization, not requirement
- Implement fallback to local storage
- Monitor CogServer availability

---

### Category 3: Quality Dependencies

#### Dependency 3.1: LLM Suggestion Quality

**Source**: Step 2 (LLM Quality Metrics)  
**Consumer**: Step 3 (Planning Quality)  
**Metric**: Inter-rater reliability >0.7

**What Step 3 Needs**:
- High-quality repair suggestions
- Accurate confidence scores
- Reliable resource estimates
- Correct failure mode identification

**What Step 2 Provides**:
- Quality metrics (inter-rater reliability, confidence calibration)
- Feedback loop for quality improvement
- Suggestion filtering
- Confidence thresholds

**Integration Point**:
```
Step 2: Measure Suggestion Quality
    ↓
Step 3: Use Quality Metrics
    ↓
Step 3: Filter Low-Quality Suggestions
    ↓
Step 3: Improve Planning
```

**Failure Scenario**: If Step 2 suggestions are low quality:
- Step 3 heuristics are poor
- Plans are suboptimal
- Planning quality is low
- System performance degrades

**Mitigation**:
- Implement suggestion quality filtering
- Use conservative confidence thresholds
- Validate suggestions against execution outcomes
- Implement suggestion ranking
- Use multiple suggestion sources

---

#### Dependency 3.2: Diagnostic Accuracy

**Source**: Step 1 (Diagnostic Accuracy)  
**Consumer**: Step 3 (Goal Correctness)  
**Metric**: Diagnostic accuracy >80%

**What Step 3 Needs**:
- Accurate diagnostic atoms
- Correct root cause identification
- Complete diagnostic coverage
- Low false positive rate

**What Step 1 Provides**:
- Diagnostic atom framework
- ECAN/PLN integration
- Real-time diagnostics
- Diagnostic validation

**Integration Point**:
```
Step 1: Generate Diagnostics
    ↓
Step 3: Convert to Goals
    ↓
Step 3: Execute Plans
    ↓
Step 3: Validate Results
```

**Failure Scenario**: If Step 1 diagnostics are inaccurate:
- Step 3 generates incorrect goals
- Plans pursue wrong objectives
- System performance worsens
- Feedback loop is corrupted

**Mitigation**:
- Validate goals against system state
- Cross-check diagnostics with multiple sources
- Implement goal feasibility checking
- Use LLM to validate diagnostic relevance
- Monitor goal success rates

---

#### Dependency 3.3: Repair Effectiveness

**Source**: Step 2 (Repair Effectiveness Metrics)  
**Consumer**: Step 3 (Planning Heuristics)  
**Metric**: Repair success rate >80%

**What Step 3 Needs**:
- Accurate repair effectiveness data
- Resource requirement estimates
- Time estimates
- Success probability estimates

**What Step 2 Provides**:
- Feedback loop tracking repair outcomes
- Effectiveness metrics
- Resource measurement
- Time tracking

**Integration Point**:
```
Step 2: Track Repair Outcomes
    ↓
Step 3: Extract Effectiveness Data
    ↓
Step 3: Update Planning Heuristics
    ↓
Step 3: Improve Future Plans
```

**Failure Scenario**: If Step 2 effectiveness data is unreliable:
- Step 3 heuristics are misleading
- Plans are suboptimal
- Resource estimates are wrong
- Plans fail more frequently

**Mitigation**:
- Validate effectiveness data independently
- Use conservative estimates
- Implement confidence intervals
- Track estimate accuracy
- Update estimates frequently

---

### Category 4: Infrastructure Dependencies

#### Dependency 4.1: AtomSpace Performance

**Source**: Step 2 (AtomSpace Integration)  
**Consumer**: Step 3 (Goal Storage and Retrieval)  
**Metric**: Query latency <100ms

**What Step 3 Needs**:
- Fast goal atom queries
- Efficient goal hierarchy traversal
- Scalable to 1M+ atoms
- Proper indexing

**What Step 2 Provides**:
- AtomSpace fully integrated
- Proper indexing configured
- Storage backend optimized
- Query performance tuned

**Integration Point**:
```
Step 3: Query Goals
    ↓
Step 2: AtomSpace Performance
    ↓
Step 3: Receive Results
```

**Failure Scenario**: If AtomSpace queries are slow:
- Step 3 planning becomes bottleneck
- System latency increases
- Interactive planning is impossible
- System becomes unresponsive

**Mitigation**:
- Implement goal caching
- Use batch queries
- Optimize frequently accessed goals
- Monitor query performance
- Implement query timeouts

---

#### Dependency 4.2: Memory Constraints

**Source**: Step 2 (Memory Optimization)  
**Consumer**: Step 3 (Goal Storage)  
**Constraint**: Stay within 128GB memory

**What Step 3 Needs**:
- Memory available for goal atoms
- Memory for plan structures
- Memory for execution state
- Memory for caching

**What Step 2 Provides**:
- Memory optimization
- LLM model quantization
- Efficient storage
- Caching strategies

**Integration Point**:
```
Step 2: Optimize Memory Usage
    ↓
Step 3: Use Available Memory
    ↓
Step 3: Store Goals and Plans
```

**Failure Scenario**: If memory is exhausted:
- Step 3 cannot store goals
- Step 3 cannot generate plans
- System crashes
- Data is lost

**Mitigation**:
- Implement goal pruning
- Use hierarchical storage (hot/cold)
- Implement memory limits
- Monitor memory usage
- Implement garbage collection

---

## Dependency Checklist

### Before Step 3 Can Begin

**From Step 1: Diagnostic Atom Framework**
- [ ] Diagnostic atom types defined and tested
- [ ] ECAN/PLN integration complete
- [ ] Real-time diagnostic generation working
- [ ] Diagnostic accuracy >80%
- [ ] Diagnostic atoms stored in AtomSpace
- [ ] Diagnostic queries working (<100ms)

**From Step 2: Autognostic LLM Integration**
- [ ] TensorRT-LLM service deployed
- [ ] LLM service responding to requests
- [ ] Response latency <30 seconds
- [ ] Suggestion quality >0.7 inter-rater reliability
- [ ] Confidence scores well-calibrated
- [ ] Feedback loop operational
- [ ] AtomSpace Storage fully integrated
- [ ] CogServer integration complete
- [ ] Repair effectiveness tracking working
- [ ] Resource estimates available

**Infrastructure**
- [ ] AtomSpace queries <100ms
- [ ] Memory usage <100GB
- [ ] CogServer available and responsive
- [ ] Monitoring and alerting in place
- [ ] Backup and recovery procedures documented

---

## Contingency Strategies

### If Step 2 LLM Service is Unavailable

**Immediate Actions**:
1. Fall back to heuristic-based planning
2. Use cached validation results
3. Assume plans are valid (optimistic validation)
4. Queue planning requests
5. Alert operators

**Medium-term Actions**:
1. Implement local LLM fallback (smaller model)
2. Implement offline validation
3. Implement plan templates
4. Implement manual planning interface

---

### If Step 2 Suggestion Quality is Poor

**Immediate Actions**:
1. Increase confidence threshold
2. Filter low-confidence suggestions
3. Implement manual suggestion review
4. Use alternative suggestion sources

**Medium-term Actions**:
1. Improve prompt engineering
2. Fine-tune LLM model
3. Implement suggestion ranking
4. Implement suggestion voting

---

### If Step 1 Diagnostics are Inaccurate

**Immediate Actions**:
1. Validate goals against system state
2. Cross-check with multiple diagnostic sources
3. Implement goal feasibility checking
4. Use conservative goal selection

**Medium-term Actions**:
1. Improve diagnostic accuracy
2. Implement diagnostic validation
3. Implement diagnostic voting
4. Improve ECAN/PLN integration

---

### If AtomSpace Storage is Slow

**Immediate Actions**:
1. Implement in-memory goal cache
2. Use batch queries
3. Implement query timeouts
4. Implement query prioritization

**Medium-term Actions**:
1. Optimize indexing
2. Implement hierarchical storage
3. Implement query optimization
4. Implement storage sharding

---

## Success Criteria for Dependencies

### Step 1 Dependency Success

- [ ] Diagnostic atoms accurately represent system state
- [ ] Diagnostic generation latency <1 second
- [ ] Diagnostic accuracy >80%
- [ ] Diagnostic coverage >90%
- [ ] False positive rate <5%

### Step 2 Dependency Success

- [ ] LLM service response latency <30 seconds
- [ ] LLM service availability >99.5%
- [ ] Suggestion quality >0.7 inter-rater reliability
- [ ] Confidence calibration error <0.1
- [ ] Repair success rate >80%
- [ ] Resource estimates accurate within 20%
- [ ] Feedback loop operational
- [ ] AtomSpace Storage queries <100ms
- [ ] CogServer fully integrated
- [ ] Memory usage <100GB

### Infrastructure Dependency Success

- [ ] AtomSpace query latency <100ms
- [ ] Memory usage <100GB
- [ ] CogServer availability >99.5%
- [ ] Monitoring coverage >95%
- [ ] Alert accuracy >90%

---

## Conclusion

Step 3 has critical dependencies on Steps 1 and 2. The success of Step 3 depends on:

1. **Accurate diagnostics** from Step 1
2. **High-quality suggestions** from Step 2
3. **Reliable LLM service** from Step 2
4. **Effective feedback loop** from Step 2
5. **Proper infrastructure** from Step 2

With proper implementation of Steps 1 and 2, and careful attention to these dependencies, Step 3 can be successfully implemented and integrated into the AGI-OS architecture.

The contingency strategies provide fallback options if any dependency fails, ensuring system resilience and continued operation even under adverse conditions.
