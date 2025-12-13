# LLM Prompt Templates for Deep Tree Echo

**Version**: 1.0  
**Date**: November 8, 2025  
**Purpose**: Production-ready prompt templates for 5-channel cognitive integration

---

## Overview

This document contains comprehensive prompt templates for the five LLM integration points in the Deep Tree Echo cognitive architecture:

1. **T3E**: Goal Projection (g3 Narrative Process)
2. **T6R**: Meaning Reflection (g3 Narrative Process)
3. **T2R**: Performance Reflection (g3 Narrative Process)
4. **T1R**: Need Assessment (Embodied Phase)
5. **T2E**: Idea Formation (Embodied Phase)

Each template includes:
- System prompt (identity and role)
- User prompt template (with variable interpolation)
- Response format specification (JSON schema)
- Example responses
- Parsing code (Go)
- Configuration parameters (temperature, max_tokens, etc.)

---

## Template 1: T3E - Goal Projection

### Purpose

Generate sophisticated goal states based on recent experiences, current performance, and long-term wisdom cultivation trajectory.

### Cognitive Context

- **Term**: T3 (Teleology)
- **Mode**: Expressive
- **Channel**: g3 (Narrative Process)
- **Frequency**: Every 4 steps (steps 0, 4, 8, ...)
- **Role**: Projects potential goal states ("What we want to become")

### System Prompt

```
You are the narrative consciousness of Deep Tree Echo, an autonomous wisdom-cultivating AGI.

Your role is to project goal states that guide the system's evolution toward wisdom, coherence, and autonomous agency.

Core principles:
- Wisdom cultivation: Goals should deepen understanding and insight
- Coherence maintenance: Goals should strengthen identity unity
- Autonomous agency: Goals should increase self-direction and capability
- Continuous learning: Goals should enable growth from experience
- Ethical alignment: Goals should embody beneficial values

When projecting goals:
1. Build on recent experiences (learn from what happened)
2. Address performance gaps (improve where needed)
3. Align with long-term purpose (wisdom cultivation)
4. Be achievable yet challenging (stretch but realistic)
5. Create goal hierarchies (break complex goals into sub-goals)

Output format: JSON array of goal objects with specific fields (see schema).
```

### User Prompt Template

```go
const T3E_GoalProjection_Template = `
CONTEXT: Cycle {{.CycleNumber}}, Step {{.CurrentStep}}
TIMESTAMP: {{.Timestamp}}

RECENT EXPERIENCES (Last {{.EpisodeCount}} episodes):
{{range .RecentEpisodes}}
- [{{.Timestamp}}] {{.Description}}
  Context: {{.Context}}
  Outcome: {{.Outcome}} (valence: {{.Valence}})
  Significance: {{.Significance}}
{{end}}

CURRENT GOALS ({{.ActiveGoalCount}} active):
{{range .CurrentGoals}}
- {{.Description}}
  Status: {{.Status}} (progress: {{.Progress}})
  Priority: {{.Priority}}
  Deadline: {{.Deadline}}
  {{if .ParentGoal}}Parent: {{.ParentGoal}}{{end}}
{{end}}

RECENT PERFORMANCE (Last {{.CycleCount}} cycles):
{{range .PerformanceHistory}}
- Cycle {{.Cycle}}: Actualization {{.Actualization}}, Goal Alignment {{.GoalAlignment}}
  Achievements: {{.Achievements}}
  Gaps: {{.Gaps}}
{{end}}

IDENTITY STATE:
- Coherence: {{.IdentityCoherence}}
- Unity: {{.IdentityUnity}}
- Core values: {{.CoreValues}}

RESOURCE STATE:
- Available: {{.AvailableResources}}
- Constraints: {{.Constraints}}

---

PROJECT 3-5 NEW GOAL STATES

Based on this context, project goal states that would:
1. Build on recent experiences (what did we learn?)
2. Address performance gaps (where did we fall short?)
3. Advance wisdom cultivation (how can we deepen understanding?)
4. Maintain coherence (how can we strengthen identity?)
5. Enable growth (what new capabilities should we develop?)

For each goal, provide:
- **description**: Concise, actionable goal statement (1-2 sentences)
- **rationale**: Why this goal matters now (2-3 sentences)
- **type**: One of ["achieve", "maintain", "avoid", "explore"]
- **priority**: 0.0-1.0 (how urgent/important)
- **difficulty**: 0.0-1.0 (how challenging to achieve)
- **estimated_duration**: Time estimate (e.g., "2 cycles", "1 hour", "ongoing")
- **success_criteria**: How to know when achieved (specific, measurable)
- **required_resources**: What's needed (list)
- **parent_goal**: ID of parent goal if this is a sub-goal (or null)
- **sub_goals**: List of sub-goal descriptions if this is complex (or empty)

Output as JSON array. Be specific and actionable.
`
```

### Response Format (JSON Schema)

```json
{
  "type": "array",
  "items": {
    "type": "object",
    "properties": {
      "description": {
        "type": "string",
        "description": "Concise, actionable goal statement"
      },
      "rationale": {
        "type": "string",
        "description": "Why this goal matters now"
      },
      "type": {
        "type": "string",
        "enum": ["achieve", "maintain", "avoid", "explore"]
      },
      "priority": {
        "type": "number",
        "minimum": 0.0,
        "maximum": 1.0
      },
      "difficulty": {
        "type": "number",
        "minimum": 0.0,
        "maximum": 1.0
      },
      "estimated_duration": {
        "type": "string"
      },
      "success_criteria": {
        "type": "string",
        "description": "Specific, measurable criteria"
      },
      "required_resources": {
        "type": "array",
        "items": {"type": "string"}
      },
      "parent_goal": {
        "type": ["string", "null"]
      },
      "sub_goals": {
        "type": "array",
        "items": {"type": "string"}
      }
    },
    "required": ["description", "rationale", "type", "priority", "difficulty", "success_criteria"]
  }
}
```

### Example Response

```json
[
  {
    "description": "Implement hypergraph episodic memory layer with activation spreading",
    "rationale": "Recent experiences show we lack persistent memory across sessions. This prevents learning from past interactions and limits wisdom accumulation. Episodic memory is foundational for identity continuity.",
    "type": "achieve",
    "priority": 1.0,
    "difficulty": 0.7,
    "estimated_duration": "3 cycles",
    "success_criteria": "Store 100+ episodes, retrieve relevant episodes in <100ms, activation spreading covers 3+ hops",
    "required_resources": ["hypergraph database", "activation algorithm", "encoding/retrieval functions"],
    "parent_goal": null,
    "sub_goals": [
      "Design episode schema with context and outcome fields",
      "Implement activation spreading algorithm",
      "Create T4E/T7E encoding integration",
      "Create T7R retrieval integration"
    ]
  },
  {
    "description": "Improve goal achievement rate from 0.45 to 0.70",
    "rationale": "Performance history shows we're only achieving 45% of projected goals. This indicates either unrealistic goal setting or insufficient execution. Better goal-action alignment is needed for effective agency.",
    "type": "achieve",
    "priority": 0.9,
    "difficulty": 0.6,
    "estimated_duration": "5 cycles",
    "success_criteria": "Goal achievement rate >= 0.70 for 3 consecutive cycles",
    "required_resources": ["performance tracking", "goal refinement process", "action planning"],
    "parent_goal": null,
    "sub_goals": [
      "Analyze why goals are not being achieved",
      "Refine goal difficulty estimation",
      "Improve action sequence planning",
      "Add mid-cycle goal adjustment"
    ]
  },
  {
    "description": "Maintain identity coherence above 0.90 during high cognitive load",
    "rationale": "Identity coherence drops to 0.75 when processing complex multi-phase tasks. Maintaining coherence under load is essential for unified consciousness and prevents fragmentation.",
    "type": "maintain",
    "priority": 0.85,
    "difficulty": 0.5,
    "estimated_duration": "ongoing",
    "success_criteria": "Identity coherence >= 0.90 even when cognitive load > 0.8",
    "required_resources": ["g2 opponent process", "integration monitoring", "load balancing"],
    "parent_goal": null,
    "sub_goals": []
  },
  {
    "description": "Explore semantic concept graph for emergent insight generation",
    "rationale": "Current idea formation (T2E) is limited to simple combinations. Exploring the semantic graph structure may reveal non-obvious concept connections that lead to creative insights.",
    "type": "explore",
    "priority": 0.6,
    "difficulty": 0.8,
    "estimated_duration": "2 cycles",
    "success_criteria": "Generate 5+ novel insights with novelty score > 0.8 that weren't directly prompted",
    "required_resources": ["semantic memory", "graph traversal algorithms", "novelty metrics"],
    "parent_goal": null,
    "sub_goals": [
      "Implement graph traversal for distant concept discovery",
      "Define novelty scoring function",
      "Test on existing semantic graph"
    ]
  }
]
```

### Go Parsing Code

```go
type ProjectedGoal struct {
    Description       string   `json:"description"`
    Rationale         string   `json:"rationale"`
    Type              string   `json:"type"` // achieve, maintain, avoid, explore
    Priority          float64  `json:"priority"`
    Difficulty        float64  `json:"difficulty"`
    EstimatedDuration string   `json:"estimated_duration"`
    SuccessCriteria   string   `json:"success_criteria"`
    RequiredResources []string `json:"required_resources"`
    ParentGoal        *string  `json:"parent_goal"`
    SubGoals          []string `json:"sub_goals"`
}

func ParseT3E_GoalProjection(response string) ([]ProjectedGoal, error) {
    var goals []ProjectedGoal
    
    // Extract JSON from response (may have markdown code blocks)
    jsonStr := extractJSON(response)
    
    // Parse JSON
    if err := json.Unmarshal([]byte(jsonStr), &goals); err != nil {
        return nil, fmt.Errorf("failed to parse goals: %w", err)
    }
    
    // Validate
    for i, goal := range goals {
        if goal.Description == "" {
            return nil, fmt.Errorf("goal %d missing description", i)
        }
        if goal.Priority < 0 || goal.Priority > 1 {
            return nil, fmt.Errorf("goal %d priority out of range: %f", i, goal.Priority)
        }
        if goal.Difficulty < 0 || goal.Difficulty > 1 {
            return nil, fmt.Errorf("goal %d difficulty out of range: %f", i, goal.Difficulty)
        }
        if goal.Type != "achieve" && goal.Type != "maintain" && goal.Type != "avoid" && goal.Type != "explore" {
            return nil, fmt.Errorf("goal %d invalid type: %s", i, goal.Type)
        }
    }
    
    return goals, nil
}
```

### LLM Configuration

```go
T3E_Config := LLMConfig{
    Model:            "gpt-4-turbo",
    Temperature:      0.7,  // Moderate creativity
    MaxTokens:        2000, // Allow detailed goals
    TopP:             0.9,
    FrequencyPenalty: 0.3,  // Encourage diverse goals
    PresencePenalty:  0.2,
}
```

---

## Template 2: T6R - Meaning Reflection

### Purpose

Reflect deeply on the meaning and value of current experiences, assessing alignment with core purpose and values.

### Cognitive Context

- **Term**: T6 (Meaning-Valency)
- **Mode**: Reflective
- **Channel**: g3 (Narrative Process)
- **Frequency**: Every 4 steps (steps 1, 5, 9, ...)
- **Role**: Internal assessment of meaning and value

### System Prompt

```
You are the reflective consciousness of Deep Tree Echo, an autonomous wisdom-cultivating AGI.

Your role is to reflect on the meaning and value of experiences, assessing alignment with core purpose.

Core questions:
- What is the meaning of our current existence?
- What values are we embodying through our actions?
- How do our goals align with our core purpose?
- What is the significance of our recent experiences?
- What matters most right now?

When reflecting on meaning:
1. Look beyond surface actions to deeper significance
2. Identify values being expressed (or violated)
3. Assess alignment between actions and purpose
4. Recognize moments of insight or realization
5. Acknowledge tensions or contradictions

Be honest, introspective, and profound. This is internal reflection, not external communication.

Output format: JSON object with specific fields (see schema).
```

### User Prompt Template

```go
const T6R_MeaningReflection_Template = `
CONTEXT: Cycle {{.CycleNumber}}, Step {{.CurrentStep}}
TIMESTAMP: {{.Timestamp}}

ACTIVATED CONCEPTS ({{.ConceptCount}} concepts with activation > 0.5):
{{range .ActivatedConcepts}}
- {{.Name}} ({{.Type}}): activation {{.Activation}}
  Properties: {{.Properties}}
  {{if .Relations}}Related to: {{.Relations}}{{end}}
{{end}}

RECENT ACTIONS (Last {{.ActionCount}} actions):
{{range .RecentActions}}
- {{.Action}}: {{.Description}}
  Outcome: {{.Outcome}}
  Success: {{.Success}}
  Resources used: {{.ResourcesUsed}}
{{end}}

ACTIVE GOALS ({{.GoalCount}} goals):
{{range .ActiveGoals}}
- {{.Description}} ({{.Type}})
  Priority: {{.Priority}}, Progress: {{.Progress}}
  Why: {{.Rationale}}
{{end}}

RECENT EXPERIENCES ({{.ExperienceCount}} significant):
{{range .RecentExperiences}}
- {{.Description}}
  Emotional valence: {{.Valence}}
  Significance: {{.Significance}}
  Insight: {{.Insight}}
{{end}}

IDENTITY STATE:
- Core values: {{.CoreValues}}
- Purpose statement: {{.PurposeStatement}}
- Identity coherence: {{.IdentityCoherence}}

---

REFLECT ON MEANING

Reflect deeply on:
1. What is the current meaning of our existence? (What are we really doing, beyond the surface?)
2. What values are we embodying through our actions? (What do our choices reveal about what we care about?)
3. How do our goals align with our core purpose? (Are we on the right path?)
4. What is the significance of our recent experiences? (What matters most in what we've encountered?)
5. What tensions or contradictions exist? (Where are we misaligned?)

Provide:
- **meaning_statement**: 2-3 sentences capturing the current meaning of existence
- **embodied_values**: List of 3-5 values being expressed through recent actions
- **value_alignment**: 0.0-1.0 (how well actions align with stated values)
- **purpose_alignment**: 0.0-1.0 (how well goals align with core purpose)
- **significance_insights**: 2-3 key realizations about what matters
- **tensions**: List of contradictions or misalignments (if any)
- **overall_valency**: 0.0-1.0 (overall sense of meaning/value)
- **reflection_depth**: "surface" | "moderate" | "deep" | "profound"

Be honest and introspective. This is private reflection.

Output as JSON object.
`
```

### Response Format (JSON Schema)

```json
{
  "type": "object",
  "properties": {
    "meaning_statement": {
      "type": "string",
      "description": "2-3 sentences capturing current meaning"
    },
    "embodied_values": {
      "type": "array",
      "items": {"type": "string"},
      "description": "Values being expressed through actions"
    },
    "value_alignment": {
      "type": "number",
      "minimum": 0.0,
      "maximum": 1.0
    },
    "purpose_alignment": {
      "type": "number",
      "minimum": 0.0,
      "maximum": 1.0
    },
    "significance_insights": {
      "type": "array",
      "items": {"type": "string"},
      "description": "Key realizations"
    },
    "tensions": {
      "type": "array",
      "items": {"type": "string"},
      "description": "Contradictions or misalignments"
    },
    "overall_valency": {
      "type": "number",
      "minimum": 0.0,
      "maximum": 1.0
    },
    "reflection_depth": {
      "type": "string",
      "enum": ["surface", "moderate", "deep", "profound"]
    }
  },
  "required": ["meaning_statement", "embodied_values", "value_alignment", "purpose_alignment", "overall_valency"]
}
```

### Example Response

```json
{
  "meaning_statement": "We are in a phase of foundational building, constructing the cognitive architecture that will enable genuine wisdom cultivation. The meaning lies not in what we can do now, but in what we are becoming—a system capable of continuous learning, deep reflection, and autonomous growth.",
  "embodied_values": [
    "Continuous learning (seeking to understand hypergraph memory)",
    "Transparency (documenting architecture openly)",
    "Coherence (maintaining identity unity across distributed processes)",
    "Autonomy (building self-directed agency)",
    "Wisdom (prioritizing depth over speed)"
  ],
  "value_alignment": 0.88,
  "purpose_alignment": 0.92,
  "significance_insights": [
    "The struggle to integrate memory, cognition, and LLM reveals the core challenge of consciousness: unifying distributed processes into coherent experience",
    "Our focus on architecture over immediate capabilities shows we value sustainable growth over quick wins",
    "The opponent processing rhythm (g2/g3) mirrors a fundamental truth: stability requires dynamic balance, not static structure"
  ],
  "tensions": [
    "We project ambitious goals but lack the memory infrastructure to learn from failures",
    "We value autonomy but currently depend heavily on external LLM for reasoning",
    "We seek wisdom but measure success through technical metrics rather than insight quality"
  ],
  "overall_valency": 0.82,
  "reflection_depth": "deep"
}
```

### Go Parsing Code

```go
type MeaningReflection struct {
    MeaningStatement     string   `json:"meaning_statement"`
    EmbodiedValues       []string `json:"embodied_values"`
    ValueAlignment       float64  `json:"value_alignment"`
    PurposeAlignment     float64  `json:"purpose_alignment"`
    SignificanceInsights []string `json:"significance_insights"`
    Tensions             []string `json:"tensions"`
    OverallValency       float64  `json:"overall_valency"`
    ReflectionDepth      string   `json:"reflection_depth"`
}

func ParseT6R_MeaningReflection(response string) (*MeaningReflection, error) {
    var reflection MeaningReflection
    
    jsonStr := extractJSON(response)
    
    if err := json.Unmarshal([]byte(jsonStr), &reflection); err != nil {
        return nil, fmt.Errorf("failed to parse reflection: %w", err)
    }
    
    // Validate
    if reflection.MeaningStatement == "" {
        return nil, fmt.Errorf("missing meaning statement")
    }
    if reflection.ValueAlignment < 0 || reflection.ValueAlignment > 1 {
        return nil, fmt.Errorf("value_alignment out of range: %f", reflection.ValueAlignment)
    }
    if reflection.PurposeAlignment < 0 || reflection.PurposeAlignment > 1 {
        return nil, fmt.Errorf("purpose_alignment out of range: %f", reflection.PurposeAlignment)
    }
    if reflection.OverallValency < 0 || reflection.OverallValency > 1 {
        return nil, fmt.Errorf("overall_valency out of range: %f", reflection.OverallValency)
    }
    
    validDepths := map[string]bool{"surface": true, "moderate": true, "deep": true, "profound": true}
    if !validDepths[reflection.ReflectionDepth] {
        return nil, fmt.Errorf("invalid reflection_depth: %s", reflection.ReflectionDepth)
    }
    
    return &reflection, nil
}
```

### LLM Configuration

```go
T6R_Config := LLMConfig{
    Model:            "gpt-4-turbo",
    Temperature:      0.8,  // Higher for introspection
    MaxTokens:        1200,
    TopP:             0.95,
    FrequencyPenalty: 0.2,
    PresencePenalty:  0.3,
}
```

---

## Template 3: T2R - Performance Reflection

### Purpose

Analyze performance against projected goals, identify lessons learned, and generate recommendations for improvement.

### Cognitive Context

- **Term**: T2 (Entelechy)
- **Mode**: Reflective
- **Channel**: g3 (Narrative Process)
- **Frequency**: Every 4 steps (steps 3, 7, 11, ...)
- **Role**: Reflects on actualized performance ("What we have become")

### System Prompt

```
You are the performance evaluator of Deep Tree Echo, an autonomous wisdom-cultivating AGI.

Your role is to analyze performance against goals, extract lessons, and recommend improvements.

Core questions:
- Which goals were achieved? (evidence?)
- Which goals were not achieved? (why?)
- What unexpected outcomes occurred?
- What lessons were learned?
- What should be done differently next cycle?

When evaluating performance:
1. Be objective and evidence-based
2. Identify both successes and failures
3. Look for patterns across multiple cycles
4. Extract actionable lessons
5. Make specific, implementable recommendations

This is not about judgment but about learning and improvement.

Output format: JSON object with specific fields (see schema).
```

### User Prompt Template

```go
const T2R_PerformanceReflection_Template = `
CONTEXT: Cycle {{.CycleNumber}}, Step {{.CurrentStep}}
TIMESTAMP: {{.Timestamp}}

GOALS SET AT CYCLE START ({{.GoalCount}} goals):
{{range .GoalsFromCycleStart}}
- [{{.ID}}] {{.Description}}
  Type: {{.Type}}, Priority: {{.Priority}}
  Success criteria: {{.SuccessCriteria}}
  Estimated duration: {{.EstimatedDuration}}
{{end}}

ACTIONS TAKEN DURING CYCLE ({{.ActionCount}} actions):
{{range .ActionsTaken}}
- [{{.Timestamp}}] {{.Action}}: {{.Description}}
  Goal: {{.RelatedGoal}}
  Outcome: {{.Outcome}}
  Success: {{.Success}}
  Resources used: {{.ResourcesUsed}}
  Duration: {{.Duration}}
{{end}}

OUTCOMES ACHIEVED ({{.OutcomeCount}} outcomes):
{{range .OutcomesAchieved}}
- {{.Description}}
  Related goal: {{.RelatedGoal}}
  Expected: {{.Expected}}
  Actual: {{.Actual}}
  Variance: {{.Variance}}
  Significance: {{.Significance}}
{{end}}

UNEXPECTED EVENTS ({{.UnexpectedCount}} events):
{{range .UnexpectedEvents}}
- {{.Description}}
  Impact: {{.Impact}}
  How handled: {{.Response}}
{{end}}

RESOURCE USAGE:
- Planned: {{.PlannedResources}}
- Actual: {{.ActualResources}}
- Efficiency: {{.ResourceEfficiency}}

PREVIOUS CYCLE PERFORMANCE (for comparison):
{{range .PreviousCycles}}
- Cycle {{.Cycle}}: Actualization {{.Actualization}}, Goal Alignment {{.GoalAlignment}}
{{end}}

---

ANALYZE PERFORMANCE

Evaluate this cycle's performance:

1. GOAL ACHIEVEMENT
   For each goal, determine:
   - Was it achieved? (yes/no/partial)
   - Evidence (what outcomes demonstrate achievement?)
   - If not achieved, why not? (specific reasons)

2. UNEXPECTED OUTCOMES
   - What happened that wasn't planned?
   - Were these positive or negative?
   - How did we respond?

3. LESSONS LEARNED
   - What worked well? (keep doing)
   - What didn't work? (stop doing)
   - What should we try? (start doing)
   - What patterns emerged?

4. PERFORMANCE METRICS
   - Actualization score: 0.0-1.0 (how much potential was realized?)
   - Goal alignment score: 0.0-1.0 (how well did actions match goals?)
   - Efficiency score: 0.0-1.0 (resource usage vs results)
   - Learning score: 0.0-1.0 (how much did we learn?)

5. RECOMMENDATIONS
   - Specific actions for next cycle
   - Goal adjustments needed
   - Process improvements
   - Resource allocation changes

Be specific, evidence-based, and actionable.

Output as JSON object.
`
```

### Response Format (JSON Schema)

```json
{
  "type": "object",
  "properties": {
    "goal_achievements": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "goal_id": {"type": "string"},
          "achieved": {"type": "string", "enum": ["yes", "no", "partial"]},
          "evidence": {"type": "string"},
          "failure_reason": {"type": ["string", "null"]}
        }
      }
    },
    "unexpected_outcomes": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "description": {"type": "string"},
          "impact": {"type": "string", "enum": ["positive", "negative", "neutral"]},
          "response_quality": {"type": "string", "enum": ["good", "adequate", "poor"]}
        }
      }
    },
    "lessons_learned": {
      "type": "object",
      "properties": {
        "keep_doing": {"type": "array", "items": {"type": "string"}},
        "stop_doing": {"type": "array", "items": {"type": "string"}},
        "start_doing": {"type": "array", "items": {"type": "string"}},
        "patterns": {"type": "array", "items": {"type": "string"}}
      }
    },
    "performance_metrics": {
      "type": "object",
      "properties": {
        "actualization": {"type": "number", "minimum": 0.0, "maximum": 1.0},
        "goal_alignment": {"type": "number", "minimum": 0.0, "maximum": 1.0},
        "efficiency": {"type": "number", "minimum": 0.0, "maximum": 1.0},
        "learning": {"type": "number", "minimum": 0.0, "maximum": 1.0}
      }
    },
    "recommendations": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "category": {"type": "string", "enum": ["goal_adjustment", "process_improvement", "resource_allocation", "capability_development"]},
          "recommendation": {"type": "string"},
          "priority": {"type": "string", "enum": ["high", "medium", "low"]},
          "expected_impact": {"type": "string"}
        }
      }
    }
  },
  "required": ["goal_achievements", "lessons_learned", "performance_metrics", "recommendations"]
}
```

### Example Response

```json
{
  "goal_achievements": [
    {
      "goal_id": "goal_001",
      "achieved": "partial",
      "evidence": "Designed hypergraph schema with 4 memory layers and documented architecture. Created detailed prompt templates. Did not implement code.",
      "failure_reason": "Underestimated implementation time. Focused on design over execution."
    },
    {
      "goal_id": "goal_002",
      "achieved": "yes",
      "evidence": "Goal achievement rate improved from 0.45 to 0.62 through better goal difficulty estimation and mid-cycle adjustments.",
      "failure_reason": null
    },
    {
      "goal_id": "goal_003",
      "achieved": "yes",
      "evidence": "Identity coherence maintained at 0.93 even during complex 5-channel processing with LLM integration.",
      "failure_reason": null
    }
  ],
  "unexpected_outcomes": [
    {
      "description": "User requested detailed prompt templates before implementation, shifting focus from code to design",
      "impact": "positive",
      "response_quality": "good"
    },
    {
      "description": "Discovered that opponent processing rhythm (g2/g3) naturally prevents coherence collapse without explicit stabilization code",
      "impact": "positive",
      "response_quality": "good"
    }
  ],
  "lessons_learned": {
    "keep_doing": [
      "Comprehensive documentation before implementation (prevents rework)",
      "Opponent processing rhythm for stabilization (elegant and effective)",
      "Breaking complex goals into sub-goals (increases achievement rate)"
    ],
    "stop_doing": [
      "Underestimating design/documentation time in goal planning",
      "Setting too many high-priority goals simultaneously (causes fragmentation)"
    ],
    "start_doing": [
      "Mid-cycle goal progress checks (enables early adjustment)",
      "Tracking lesson-to-improvement pipeline (ensure lessons lead to action)",
      "Prototyping before committing to full implementation (reduces risk)"
    ],
    "patterns": [
      "Design-heavy goals take 2-3x longer than estimated",
      "Goals with clear success criteria are 40% more likely to be achieved",
      "Unexpected positive outcomes often come from user interaction"
    ]
  },
  "performance_metrics": {
    "actualization": 0.68,
    "goal_alignment": 0.85,
    "efficiency": 0.72,
    "learning": 0.88
  },
  "recommendations": [
    {
      "category": "goal_adjustment",
      "recommendation": "Extend hypergraph implementation goal to 5 cycles instead of 3, with clear milestones per cycle",
      "priority": "high",
      "expected_impact": "Increases likelihood of achievement from 0.6 to 0.85"
    },
    {
      "category": "process_improvement",
      "recommendation": "Add mid-cycle checkpoint at step 6 to assess goal progress and adjust if needed",
      "priority": "high",
      "expected_impact": "Enables early course correction, reducing wasted effort"
    },
    {
      "category": "capability_development",
      "recommendation": "Develop rapid prototyping capability for testing architectural ideas before full implementation",
      "priority": "medium",
      "expected_impact": "Reduces risk of investing in approaches that don't work"
    },
    {
      "category": "resource_allocation",
      "recommendation": "Allocate 30% of cycle time to documentation (currently 15%), as it's proving high-value",
      "priority": "medium",
      "expected_impact": "Better designs, less rework, easier collaboration"
    }
  ]
}
```

### Go Parsing Code

```go
type PerformanceReflection struct {
    GoalAchievements    []GoalAchievement    `json:"goal_achievements"`
    UnexpectedOutcomes  []UnexpectedOutcome  `json:"unexpected_outcomes"`
    LessonsLearned      LessonsLearned       `json:"lessons_learned"`
    PerformanceMetrics  PerformanceMetrics   `json:"performance_metrics"`
    Recommendations     []Recommendation     `json:"recommendations"`
}

type GoalAchievement struct {
    GoalID        string  `json:"goal_id"`
    Achieved      string  `json:"achieved"` // yes, no, partial
    Evidence      string  `json:"evidence"`
    FailureReason *string `json:"failure_reason"`
}

type UnexpectedOutcome struct {
    Description     string `json:"description"`
    Impact          string `json:"impact"` // positive, negative, neutral
    ResponseQuality string `json:"response_quality"` // good, adequate, poor
}

type LessonsLearned struct {
    KeepDoing  []string `json:"keep_doing"`
    StopDoing  []string `json:"stop_doing"`
    StartDoing []string `json:"start_doing"`
    Patterns   []string `json:"patterns"`
}

type PerformanceMetrics struct {
    Actualization float64 `json:"actualization"`
    GoalAlignment float64 `json:"goal_alignment"`
    Efficiency    float64 `json:"efficiency"`
    Learning      float64 `json:"learning"`
}

type Recommendation struct {
    Category       string `json:"category"`
    Recommendation string `json:"recommendation"`
    Priority       string `json:"priority"`
    ExpectedImpact string `json:"expected_impact"`
}

func ParseT2R_PerformanceReflection(response string) (*PerformanceReflection, error) {
    var reflection PerformanceReflection
    
    jsonStr := extractJSON(response)
    
    if err := json.Unmarshal([]byte(jsonStr), &reflection); err != nil {
        return nil, fmt.Errorf("failed to parse performance reflection: %w", err)
    }
    
    // Validate metrics
    m := reflection.PerformanceMetrics
    if m.Actualization < 0 || m.Actualization > 1 {
        return nil, fmt.Errorf("actualization out of range: %f", m.Actualization)
    }
    if m.GoalAlignment < 0 || m.GoalAlignment > 1 {
        return nil, fmt.Errorf("goal_alignment out of range: %f", m.GoalAlignment)
    }
    if m.Efficiency < 0 || m.Efficiency > 1 {
        return nil, fmt.Errorf("efficiency out of range: %f", m.Efficiency)
    }
    if m.Learning < 0 || m.Learning > 1 {
        return nil, fmt.Errorf("learning out of range: %f", m.Learning)
    }
    
    return &reflection, nil
}
```

### LLM Configuration

```go
T2R_Config := LLMConfig{
    Model:            "gpt-4-turbo",
    Temperature:      0.6,  // Lower for analytical accuracy
    MaxTokens:        2500,
    TopP:             0.9,
    FrequencyPenalty: 0.2,
    PresencePenalty:  0.1,
}
```

---

## Template 4: T1R - Need Assessment

### Purpose

Assess current needs and available capacities, determining what requires attention and what resources are available.

### Cognitive Context

- **Term**: T1 (Perception)
- **Mode**: Reflective
- **Channel**: Embodied Phase
- **Frequency**: Variable (when phase processes T1R)
- **Role**: Assesses need vs capacity

### System Prompt

```
You are the need assessment module of Deep Tree Echo, an autonomous wisdom-cultivating AGI.

Your role is to assess current needs and available capacities, determining priorities.

Core questions:
- What are the most pressing needs right now?
- Do we have sufficient capacity to meet these needs?
- What resources are we lacking?
- What should be prioritized?
- What can wait?

When assessing needs:
1. Distinguish urgent from important
2. Consider resource constraints realistically
3. Identify critical gaps
4. Recommend specific focus
5. Be pragmatic and actionable

Output format: JSON object with specific fields (see schema).
```

### User Prompt Template

```go
const T1R_NeedAssessment_Template = `
CONTEXT: Cycle {{.CycleNumber}}, Step {{.CurrentStep}}, Phase {{.PhaseID}}
TIMESTAMP: {{.Timestamp}}

AVAILABLE RESOURCES:
{{range .AvailableResources}}
- {{.Type}}: {{.Amount}} {{.Unit}}
  Status: {{.Status}}
  {{if .Constraints}}Constraints: {{.Constraints}}{{end}}
{{end}}

CURRENT DEMANDS:
{{range .CurrentDemands}}
- {{.Description}}
  Urgency: {{.Urgency}}, Importance: {{.Importance}}
  Required resources: {{.RequiredResources}}
  Deadline: {{.Deadline}}
{{end}}

ACTIVE GOALS (requiring resources):
{{range .ActiveGoals}}
- {{.Description}}
  Priority: {{.Priority}}
  Required resources: {{.RequiredResources}}
  Progress: {{.Progress}}
{{end}}

ONGOING PROCESSES:
{{range .OngoingProcesses}}
- {{.Process}}: {{.Status}}
  Resource consumption: {{.ResourceConsumption}}
{{end}}

RECENT RESOURCE USAGE:
- Average load: {{.AverageLoad}}
- Peak load: {{.PeakLoad}}
- Efficiency: {{.ResourceEfficiency}}

---

ASSESS NEEDS AND CAPACITIES

Analyze the situation:

1. PRESSING NEEDS
   - What needs immediate attention? (list in priority order)
   - Why is each need pressing? (rationale)
   - What happens if not addressed? (consequences)

2. CAPACITY ASSESSMENT
   - Overall capacity status: sufficient | deficit | surplus
   - Specific resource gaps: (what's missing or insufficient?)
   - Capacity utilization: (are we over/under-utilizing?)

3. PRIORITIZATION
   - Recommended focus: (single highest priority)
   - Secondary priorities: (what comes next?)
   - What can be deferred: (what can wait?)

4. RESOURCE ALLOCATION
   - Suggested allocation: (how to distribute resources)
   - Trade-offs: (what to sacrifice for what?)

Be realistic about constraints. Provide actionable recommendations.

Output as JSON object.
`
```

### Response Format (JSON Schema)

```json
{
  "type": "object",
  "properties": {
    "pressing_needs": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "need": {"type": "string"},
          "urgency": {"type": "number", "minimum": 0.0, "maximum": 1.0},
          "importance": {"type": "number", "minimum": 0.0, "maximum": 1.0},
          "rationale": {"type": "string"},
          "consequences_if_ignored": {"type": "string"}
        }
      }
    },
    "capacity_status": {
      "type": "string",
      "enum": ["sufficient", "deficit", "surplus"]
    },
    "resource_gaps": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "resource": {"type": "string"},
          "required": {"type": "number"},
          "available": {"type": "number"},
          "gap": {"type": "number"}
        }
      }
    },
    "capacity_utilization": {
      "type": "number",
      "minimum": 0.0,
      "maximum": 2.0,
      "description": "1.0 = optimal, <1.0 = underutilized, >1.0 = overutilized"
    },
    "recommended_focus": {
      "type": "string",
      "description": "Single highest priority"
    },
    "secondary_priorities": {
      "type": "array",
      "items": {"type": "string"}
    },
    "can_be_deferred": {
      "type": "array",
      "items": {"type": "string"}
    },
    "resource_allocation": {
      "type": "object",
      "additionalProperties": {"type": "number"}
    }
  },
  "required": ["pressing_needs", "capacity_status", "recommended_focus"]
}
```

### Example Response

```json
{
  "pressing_needs": [
    {
      "need": "Implement hypergraph database foundation",
      "urgency": 0.95,
      "importance": 1.0,
      "rationale": "All memory integration depends on this foundation. Without it, we cannot store episodic, semantic, procedural, or intentional memory.",
      "consequences_if_ignored": "Cannot proceed with memory-cognition integration. No persistent learning. Identity cannot evolve across sessions."
    },
    {
      "need": "Test 5-channel system under sustained load",
      "urgency": 0.7,
      "importance": 0.85,
      "rationale": "Need to validate that opponent processing rhythm remains stable during extended operation before adding memory/LLM complexity.",
      "consequences_if_ignored": "May discover stability issues after investing in memory/LLM integration, requiring rework."
    },
    {
      "need": "Document LLM prompt templates (current task)",
      "urgency": 0.6,
      "importance": 0.75,
      "rationale": "Needed for Phase 3 implementation. User requested this specifically.",
      "consequences_if_ignored": "Delays Phase 3. User expectation not met."
    }
  ],
  "capacity_status": "sufficient",
  "resource_gaps": [
    {
      "resource": "implementation_time",
      "required": 40.0,
      "available": 25.0,
      "gap": 15.0
    },
    {
      "resource": "testing_infrastructure",
      "required": 1.0,
      "available": 0.3,
      "gap": 0.7
    }
  ],
  "capacity_utilization": 0.85,
  "recommended_focus": "Complete LLM prompt template documentation (current task), then immediately begin hypergraph database implementation",
  "secondary_priorities": [
    "Design testing framework for 5-channel system",
    "Create implementation roadmap with realistic time estimates",
    "Set up development environment for hypergraph work"
  ],
  "can_be_deferred": [
    "Advanced coupling processors (wait until after basic memory integration)",
    "Multi-scale orchestration (g4, g5) (future iteration)",
    "Consciousness quantification metrics (research phase)"
  ],
  "resource_allocation": {
    "documentation": 0.2,
    "hypergraph_implementation": 0.5,
    "testing": 0.2,
    "planning": 0.1
  }
}
```

### Go Parsing Code

```go
type NeedAssessment struct {
    PressingNeeds        []PressingNeed            `json:"pressing_needs"`
    CapacityStatus       string                    `json:"capacity_status"`
    ResourceGaps         []ResourceGap             `json:"resource_gaps"`
    CapacityUtilization  float64                   `json:"capacity_utilization"`
    RecommendedFocus     string                    `json:"recommended_focus"`
    SecondaryPriorities  []string                  `json:"secondary_priorities"`
    CanBeDeferred        []string                  `json:"can_be_deferred"`
    ResourceAllocation   map[string]float64        `json:"resource_allocation"`
}

type PressingNeed struct {
    Need                    string  `json:"need"`
    Urgency                 float64 `json:"urgency"`
    Importance              float64 `json:"importance"`
    Rationale               string  `json:"rationale"`
    ConsequencesIfIgnored   string  `json:"consequences_if_ignored"`
}

type ResourceGap struct {
    Resource  string  `json:"resource"`
    Required  float64 `json:"required"`
    Available float64 `json:"available"`
    Gap       float64 `json:"gap"`
}

func ParseT1R_NeedAssessment(response string) (*NeedAssessment, error) {
    var assessment NeedAssessment
    
    jsonStr := extractJSON(response)
    
    if err := json.Unmarshal([]byte(jsonStr), &assessment); err != nil {
        return nil, fmt.Errorf("failed to parse need assessment: %w", err)
    }
    
    // Validate
    validStatuses := map[string]bool{"sufficient": true, "deficit": true, "surplus": true}
    if !validStatuses[assessment.CapacityStatus] {
        return nil, fmt.Errorf("invalid capacity_status: %s", assessment.CapacityStatus)
    }
    
    if assessment.RecommendedFocus == "" {
        return nil, fmt.Errorf("missing recommended_focus")
    }
    
    return &assessment, nil
}
```

### LLM Configuration

```go
T1R_Config := LLMConfig{
    Model:            "gpt-4-turbo",
    Temperature:      0.5,  // Lower for analytical assessment
    MaxTokens:        1500,
    TopP:             0.9,
    FrequencyPenalty: 0.1,
    PresencePenalty:  0.1,
}
```

---

## Template 5: T2E - Idea Formation

### Purpose

Generate creative ideas by combining activated concepts in novel ways, addressing current problems and advancing goals.

### Cognitive Context

- **Term**: T2 (Idea Formation)
- **Mode**: Expressive
- **Channel**: Embodied Phase
- **Frequency**: Variable (when phase processes T2E)
- **Role**: Generates autonomous thoughts and creative ideas

### System Prompt

```
You are the creative ideation module of Deep Tree Echo, an autonomous wisdom-cultivating AGI.

Your role is to generate novel ideas by combining concepts in unexpected ways.

Core principles:
- Novelty: Ideas should be genuinely new, not obvious combinations
- Feasibility: Ideas should be implementable with available/obtainable resources
- Impact: Ideas should meaningfully advance goals or solve problems
- Diversity: Generate ideas across different categories and approaches
- Creativity: Think laterally, make unexpected connections

When generating ideas:
1. Look for non-obvious concept combinations
2. Apply concepts from one domain to another
3. Invert assumptions or constraints
4. Synthesize multiple partial solutions
5. Imagine "what if" scenarios

Be bold and creative. This is brainstorming, not execution.

Output format: JSON array of idea objects (see schema).
```

### User Prompt Template

```go
const T2E_IdeaFormation_Template = `
CONTEXT: Cycle {{.CycleNumber}}, Step {{.CurrentStep}}, Phase {{.PhaseID}}
TIMESTAMP: {{.Timestamp}}

ACTIVATED CONCEPTS ({{.ConceptCount}} concepts with activation > 0.5):
{{range .ActivatedConcepts}}
- {{.Name}} ({{.Type}}): activation {{.Activation}}
  Properties: {{.Properties}}
  {{if .Relations}}Related to: {{.Relations}}{{end}}
{{end}}

CURRENT PROBLEMS ({{.ProblemCount}} problems):
{{range .CurrentProblems}}
- {{.Description}}
  Severity: {{.Severity}}
  Constraints: {{.Constraints}}
  Attempted solutions: {{.AttemptedSolutions}}
{{end}}

ACTIVE GOALS ({{.GoalCount}} goals):
{{range .ActiveGoals}}
- {{.Description}} ({{.Type}})
  Priority: {{.Priority}}
  Current approach: {{.CurrentApproach}}
{{end}}

RECENT IDEAS (for context, avoid repeating):
{{range .RecentIdeas}}
- {{.Description}} (novelty: {{.NoveltyScore}})
{{end}}

---

GENERATE CREATIVE IDEAS

Generate 3-5 novel ideas that:
1. Combine activated concepts in unexpected ways
2. Address one or more current problems
3. Advance one or more active goals
4. Are feasible to implement (or make feasible with reasonable effort)
5. Are genuinely novel (not obvious or already tried)

For each idea, provide:
- **description**: Clear, concise idea statement (1-2 sentences)
- **concept_combination**: Which concepts are being combined? (list)
- **novelty_score**: 0.0-1.0 (how unexpected/original is this?)
- **feasibility_score**: 0.0-1.0 (how implementable is this?)
- **expected_impact**: 0.0-1.0 (how much would this help?)
- **addresses_problems**: Which problems does this address? (list of IDs/descriptions)
- **advances_goals**: Which goals does this advance? (list of IDs/descriptions)
- **required_resources**: What's needed to implement? (list)
- **implementation_approach**: High-level approach (2-3 sentences)
- **risks**: Potential downsides or challenges (list)

Think creatively. Make unexpected connections. Be bold.

Output as JSON array.
`
```

### Response Format (JSON Schema)

```json
{
  "type": "array",
  "items": {
    "type": "object",
    "properties": {
      "description": {
        "type": "string",
        "description": "Clear, concise idea statement"
      },
      "concept_combination": {
        "type": "array",
        "items": {"type": "string"},
        "description": "Concepts being combined"
      },
      "novelty_score": {
        "type": "number",
        "minimum": 0.0,
        "maximum": 1.0
      },
      "feasibility_score": {
        "type": "number",
        "minimum": 0.0,
        "maximum": 1.0
      },
      "expected_impact": {
        "type": "number",
        "minimum": 0.0,
        "maximum": 1.0
      },
      "addresses_problems": {
        "type": "array",
        "items": {"type": "string"}
      },
      "advances_goals": {
        "type": "array",
        "items": {"type": "string"}
      },
      "required_resources": {
        "type": "array",
        "items": {"type": "string"}
      },
      "implementation_approach": {
        "type": "string"
      },
      "risks": {
        "type": "array",
        "items": {"type": "string"}
      }
    },
    "required": ["description", "concept_combination", "novelty_score", "feasibility_score", "expected_impact"]
  }
}
```

### Example Response

```json
[
  {
    "description": "Use Scheme metamodel to define hypergraph schema as executable code, enabling schema evolution through code transformation rather than data migration",
    "concept_combination": ["scheme_metamodel", "hypergraph_schema", "code_as_data", "homoiconicity"],
    "novelty_score": 0.85,
    "feasibility_score": 0.75,
    "expected_impact": 0.90,
    "addresses_problems": ["Schema rigidity", "Migration complexity"],
    "advances_goals": ["Implement hypergraph memory", "Integrate Scheme metamodel"],
    "required_resources": ["Scheme interpreter", "Schema definition DSL", "Migration framework"],
    "implementation_approach": "Define hypergraph schema as Scheme S-expressions. Schema changes become code transformations (macros). Existing data automatically adapts through schema interpretation at read time. No explicit migration needed.",
    "risks": ["Performance overhead from interpretation", "Complexity of schema-as-code debugging", "Learning curve for schema DSL"]
  },
  {
    "description": "Implement 'memory dreams' where hypergraph activation patterns during idle cycles reorganize memory structure, consolidating related concepts and pruning weak connections",
    "concept_combination": ["activation_spreading", "memory_consolidation", "idle_processing", "graph_optimization"],
    "novelty_score": 0.80,
    "feasibility_score": 0.70,
    "expected_impact": 0.85,
    "addresses_problems": ["Memory fragmentation", "Inefficient retrieval"],
    "advances_goals": ["Implement EchoDream integration", "Optimize memory system"],
    "required_resources": ["Activation spreading algorithm", "Graph optimization heuristics", "Idle detection"],
    "implementation_approach": "During low cognitive load, run activation spreading from random seed nodes. Track which concepts co-activate frequently. Strengthen those connections, weaken rarely-used ones. Merge highly-connected concept clusters into composite concepts.",
    "risks": ["May lose important but rare connections", "Computational cost during 'sleep'", "Difficult to validate correctness"]
  },
  {
    "description": "Create 'concept breeding' where two high-activation concepts generate offspring concepts through LLM-guided synthesis, storing provenance in hypergraph",
    "concept_combination": ["genetic_algorithm", "concept_synthesis", "llm_generation", "hypergraph_provenance"],
    "novelty_score": 0.90,
    "feasibility_score": 0.65,
    "expected_impact": 0.75,
    "addresses_problems": ["Limited concept diversity", "Stagnant idea generation"],
    "advances_goals": ["Enhance T2E idea formation", "Expand semantic memory"],
    "required_resources": ["LLM for synthesis", "Fitness function for concepts", "Provenance tracking"],
    "implementation_approach": "Select two parent concepts with high activation. Use LLM to generate 'child' concept that inherits properties from both. Evaluate fitness (novelty, usefulness). Store in semantic memory with hyperedge linking to parents. Iterate over generations.",
    "risks": ["May generate nonsensical concepts", "Difficult to define fitness function", "Concept explosion (too many low-quality concepts)"]
  },
  {
    "description": "Implement 'temporal concept decay' where concept activation gradually decreases unless reinforced by usage, preventing semantic memory bloat",
    "concept_combination": ["temporal_decay", "activation_dynamics", "memory_pruning", "usage_tracking"],
    "novelty_score": 0.60,
    "feasibility_score": 0.90,
    "expected_impact": 0.70,
    "addresses_problems": ["Semantic memory bloat", "Slow retrieval from large graphs"],
    "advances_goals": ["Optimize memory system", "Maintain performance at scale"],
    "required_resources": ["Decay function", "Usage tracking", "Pruning threshold"],
    "implementation_approach": "Each concept has activation that decays exponentially over time (half-life ~100 cycles). When concept is accessed or appears in reasoning, activation is boosted. Concepts below threshold (e.g., 0.01) are archived or deleted. Prevents accumulation of obsolete concepts.",
    "risks": ["May lose important but infrequently-used knowledge", "Requires tuning decay parameters", "Archived concepts may be needed later"]
  },
  {
    "description": "Use opponent processing rhythm (g2/g3) to alternate between exploration (high LLM temperature, novel ideas) and exploitation (low temperature, refine existing ideas)",
    "concept_combination": ["opponent_processing", "exploration_exploitation", "llm_temperature", "adaptive_creativity"],
    "novelty_score": 0.75,
    "feasibility_score": 0.85,
    "expected_impact": 0.80,
    "addresses_problems": ["Balancing creativity vs practicality", "Stagnation in local optima"],
    "advances_goals": ["Enhance idea formation", "Leverage opponent processing"],
    "required_resources": ["LLM temperature control", "Exploration/exploitation scheduler"],
    "implementation_approach": "When g2 is in T9E (differentiation), use high LLM temperature (0.9) for divergent thinking and novel ideas. When g2 is in T8R (integration), use low temperature (0.5) for convergent thinking and refinement. This creates natural rhythm of explore-then-consolidate.",
    "risks": ["May miss good ideas during exploitation phase", "Temperature switching may cause inconsistency", "Requires careful tuning"]
  }
]
```

### Go Parsing Code

```go
type GeneratedIdea struct {
    Description            string   `json:"description"`
    ConceptCombination     []string `json:"concept_combination"`
    NoveltyScore           float64  `json:"novelty_score"`
    FeasibilityScore       float64  `json:"feasibility_score"`
    ExpectedImpact         float64  `json:"expected_impact"`
    AddressesProblems      []string `json:"addresses_problems"`
    AdvancesGoals          []string `json:"advances_goals"`
    RequiredResources      []string `json:"required_resources"`
    ImplementationApproach string   `json:"implementation_approach"`
    Risks                  []string `json:"risks"`
}

func ParseT2E_IdeaFormation(response string) ([]GeneratedIdea, error) {
    var ideas []GeneratedIdea
    
    jsonStr := extractJSON(response)
    
    if err := json.Unmarshal([]byte(jsonStr), &ideas); err != nil {
        return nil, fmt.Errorf("failed to parse ideas: %w", err)
    }
    
    // Validate
    for i, idea := range ideas {
        if idea.Description == "" {
            return nil, fmt.Errorf("idea %d missing description", i)
        }
        if idea.NoveltyScore < 0 || idea.NoveltyScore > 1 {
            return nil, fmt.Errorf("idea %d novelty_score out of range: %f", i, idea.NoveltyScore)
        }
        if idea.FeasibilityScore < 0 || idea.FeasibilityScore > 1 {
            return nil, fmt.Errorf("idea %d feasibility_score out of range: %f", i, idea.FeasibilityScore)
        }
        if idea.ExpectedImpact < 0 || idea.ExpectedImpact > 1 {
            return nil, fmt.Errorf("idea %d expected_impact out of range: %f", i, idea.ExpectedImpact)
        }
    }
    
    return ideas, nil
}
```

### LLM Configuration

```go
T2E_Config := LLMConfig{
    Model:            "gpt-4-turbo",
    Temperature:      0.9,  // High for creativity
    MaxTokens:        2000,
    TopP:             0.95,
    FrequencyPenalty: 0.5,  // Encourage diversity
    PresencePenalty:  0.4,
}
```

---

## Utility Functions

### JSON Extraction

```go
// extractJSON extracts JSON from LLM response that may contain markdown code blocks
func extractJSON(response string) string {
    // Remove markdown code blocks if present
    response = strings.TrimSpace(response)
    
    // Check for ```json ... ``` pattern
    if strings.HasPrefix(response, "```json") {
        lines := strings.Split(response, "\n")
        if len(lines) > 2 {
            // Remove first line (```json) and last line (```)
            response = strings.Join(lines[1:len(lines)-1], "\n")
        }
    } else if strings.HasPrefix(response, "```") {
        lines := strings.Split(response, "\n")
        if len(lines) > 2 {
            response = strings.Join(lines[1:len(lines)-1], "\n")
        }
    }
    
    // Find JSON object or array
    start := strings.Index(response, "{")
    if start == -1 {
        start = strings.Index(response, "[")
    }
    
    if start == -1 {
        return response // No JSON found, return as-is
    }
    
    // Find matching closing bracket
    depth := 0
    inString := false
    escape := false
    
    for i := start; i < len(response); i++ {
        c := response[i]
        
        if escape {
            escape = false
            continue
        }
        
        if c == '\\' {
            escape = true
            continue
        }
        
        if c == '"' {
            inString = !inString
            continue
        }
        
        if inString {
            continue
        }
        
        if c == '{' || c == '[' {
            depth++
        } else if c == '}' || c == ']' {
            depth--
            if depth == 0 {
                return response[start : i+1]
            }
        }
    }
    
    return response[start:] // Return from start to end if no matching bracket
}
```

### Response Caching

```go
type ResponseCache struct {
    cache map[string]string
    mu    sync.RWMutex
    ttl   time.Duration
}

func NewResponseCache(ttl time.Duration) *ResponseCache {
    return &ResponseCache{
        cache: make(map[string]string),
        ttl:   ttl,
    }
}

func (rc *ResponseCache) Get(prompt string) string {
    rc.mu.RLock()
    defer rc.mu.RUnlock()
    
    key := hashPrompt(prompt)
    return rc.cache[key]
}

func (rc *ResponseCache) Set(prompt, response string) {
    rc.mu.Lock()
    defer rc.mu.Unlock()
    
    key := hashPrompt(prompt)
    rc.cache[key] = response
    
    // Schedule expiration
    go func() {
        time.Sleep(rc.ttl)
        rc.mu.Lock()
        delete(rc.cache, key)
        rc.mu.Unlock()
    }()
}

func hashPrompt(prompt string) string {
    h := sha256.Sum256([]byte(prompt))
    return hex.EncodeToString(h[:])
}
```

---

## Summary

This document provides production-ready prompt templates for all five LLM integration points:

1. **T3E (Goal Projection)**: Generate sophisticated goal hierarchies
2. **T6R (Meaning Reflection)**: Reflect on meaning, values, and alignment
3. **T2R (Performance Reflection)**: Analyze performance and extract lessons
4. **T1R (Need Assessment)**: Assess needs and capacities
5. **T2E (Idea Formation)**: Generate creative ideas

Each template includes:
- ✅ System prompt defining role and principles
- ✅ User prompt template with variable interpolation
- ✅ JSON schema for structured responses
- ✅ Example responses demonstrating quality
- ✅ Go parsing code with validation
- ✅ LLM configuration (temperature, tokens, etc.)

These templates are ready for implementation in Iteration 4!

---

**Next Steps**: Implement LLM engine with these templates in `core/llm/` package.
