# Nested Agency Architecture

This document provides a visual overview of the nested-agency pattern architecture.

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                           USER REQUEST                           │
│            "Analyze data and create documentation"               │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                   PARENT AGENT (Coordinator)                     │
│                  nested-agency-coordinator                       │
│                                                                   │
│  Role: Analyze request, plan approach, delegate to children     │
│  Tools: ['read', 'search', 'custom-agent']                      │
│  Location: .github/agents/nested-agency.md                      │
└─────────┬───────────────────────────────────────┬───────────────┘
          │                                       │
          │ Delegates via                         │ Delegates via
          │ custom-agent tool                     │ custom-agent tool
          │                                       │
          ▼                                       ▼
┌─────────────────────────┐           ┌─────────────────────────┐
│   CHILD AGENT 1         │           │   CHILD AGENT 2         │
│ nested-agency-child-1   │           │ nested-agency-child-2   │
│                         │           │                         │
│ Specialization:         │           │ Specialization:         │
│ - Data Analysis         │           │ - Documentation         │
│ - Processing            │           │ - Report Writing        │
│ - Computation           │           │ - Communication         │
│                         │           │                         │
│ Tools:                  │           │ Tools:                  │
│ ['read', 'edit',        │           │ ['read', 'edit',        │
│  'search', 'shell']     │           │  'search']              │
│                         │           │                         │
│ Location:               │           │ Location:               │
│ .github/agents/         │           │ .github/agents/         │
│ nested-agency/          │           │ nested-agency/          │
│ child-agent-1.md        │           │ child-agent-2.md        │
└─────────┬───────────────┘           └─────────┬───────────────┘
          │                                     │
          │ Results                             │ Results
          │                                     │
          └────────────────┬────────────────────┘
                           │
                           ▼
                  ┌────────────────┐
                  │  SYNTHESIS &   │
                  │  FINAL OUTPUT  │
                  └────────────────┘
```

## File Structure

```
.github/agents/
│
├── nested-agency.md                  # Parent Coordinator Agent
│
└── nested-agency/                    # Child Agents Directory
    ├── README.md                     # Pattern Documentation
    ├── USAGE.md                      # Usage Examples
    ├── ARCHITECTURE.md               # This File
    ├── child-agent-1.md              # Data Analysis Specialist
    ├── child-agent-2.md              # Documentation Specialist
    └── relevant-file.ext             # Shared Resources
```

## Communication Flow

### Sequential Delegation

```
User → Parent → Child-1 → Parent → Child-2 → Parent → User
       ────────────────────────────────────────────────
       [  1. Analyze  ] [  2. Document  ] [  3. Synthesize  ]
```

### Parallel Delegation

```
User → Parent ──┬─→ Child-1 (Task A) ──┐
                │                       │
                └─→ Child-2 (Task B) ──┤
                                        │
                            Results ← ──┘
                                ↓
                            Synthesis
                                ↓
                              User
```

## Agent Responsibilities Matrix

| Agent | Primary Role | Secondary Role | Key Tools |
|-------|-------------|----------------|-----------|
| **nested-agency-coordinator** | Coordinate tasks | Plan & synthesize | custom-agent, read, search |
| **nested-agency-child-1** | Data analysis | Processing | read, edit, search, shell |
| **nested-agency-child-2** | Documentation | Communication | read, edit, search |

## Tool Usage Matrix

| Tool | Parent | Child-1 | Child-2 | Purpose |
|------|--------|---------|---------|---------|
| `custom-agent` | ✅ | ❌ | ❌ | Delegate to child agents (parent only) |
| `read` | ✅ | ✅ | ✅ | Read files and content |
| `search` | ✅ | ✅ | ✅ | Find files and code |
| `edit` | ❌ | ✅ | ✅ | Modify files (children do the work) |
| `shell` | ❌ | ✅ | ❌ | Run analysis scripts (child-1 only) |

## Delegation Decision Tree

```
                    ┌─────────────┐
                    │ User Request │
                    └──────┬──────┘
                           │
                           ▼
                    ┌──────────────┐
                    │ Parent Agent │
                    │   Analyzes   │
                    └──────┬───────┘
                           │
              ┌────────────┴────────────┐
              │                         │
        ┌─────▼─────┐             ┌────▼─────┐
        │ Data Work? │             │ Doc Work? │
        └─────┬──────┘             └────┬─────┘
              │                         │
          YES │ NO                  YES │ NO
              │                         │
        ┌─────▼──────┐            ┌─────▼──────┐
        │ Delegate to │            │ Delegate to │
        │  Child-1    │            │  Child-2    │
        └─────┬───────┘            └─────┬───────┘
              │                          │
              └──────────┬───────────────┘
                         │
                    ┌────▼─────┐
                    │ Synthesize│
                    │  Results  │
                    └────┬──────┘
                         │
                    ┌────▼──────┐
                    │   Return   │
                    │  to User   │
                    └────────────┘
```

## Agent State Diagram

```
┌─────────────┐
│    IDLE     │ ← ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ┐
└──────┬──────┘                                  │
       │ User Request                            │
       ▼                                         │
┌─────────────┐                                  │
│  ANALYZING  │ (Parent)                         │
└──────┬──────┘                                  │
       │ Decision Made                           │
       ▼                                         │
┌─────────────┐                                  │
│ DELEGATING  │ (Parent → Child)                 │
└──────┬──────┘                                  │
       │ Child Invoked                           │
       ▼                                         │
┌─────────────┐                                  │
│  WORKING    │ (Child Processing)               │
└──────┬──────┘                                  │
       │ Work Complete                           │
       ▼                                         │
┌─────────────┐                                  │
│ COLLECTING  │ (Parent Gathers Results)         │
└──────┬──────┘                                  │
       │ All Results In                          │
       ▼                                         │
┌─────────────┐                                  │
│SYNTHESIZING │ (Parent Combines)                │
└──────┬──────┘                                  │
       │ Complete                                │
       ▼                                         │
┌─────────────┐                                  │
│  COMPLETE   │ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ┘
└─────────────┘ Return to Idle
```

## Information Flow

### Parent → Child

```
┌──────────────────┐
│  Parent Agent    │
│  - Task Context  │
│  - Requirements  │
│  - Constraints   │
└────────┬─────────┘
         │ via custom-agent tool
         ▼
┌──────────────────┐
│  Child Agent     │
│  - Receives Task │
│  - Has Context   │
│  - Executes Work │
└──────────────────┘
```

### Child → Parent

```
┌──────────────────┐
│  Child Agent     │
│  - Completes Work│
│  - Returns Data  │
│  - Status Info   │
└────────┬─────────┘
         │ return value
         ▼
┌──────────────────┐
│  Parent Agent    │
│  - Receives Data │
│  - Validates     │
│  - Continues     │
└──────────────────┘
```

## Scalability Pattern

### Adding New Children

```
Current:
Parent
  ├── Child-1 (Data)
  └── Child-2 (Docs)

Extended:
Parent
  ├── Child-1 (Data)
  ├── Child-2 (Docs)
  ├── Child-3 (Testing)     ← New
  └── Child-4 (Deployment)  ← New
```

### Multi-Level Nesting (Advanced)

```
Root-Parent
  ├── Domain-1-Parent
  │     ├── Child-1A
  │     └── Child-1B
  └── Domain-2-Parent
        ├── Child-2A
        └── Child-2B
```

## Key Architectural Principles

### 1. Separation of Concerns
- **Parent**: Coordination, planning, synthesis
- **Children**: Specialized execution

### 2. Clear Interfaces
- **Input**: Task context via `custom-agent` tool
- **Output**: Results returned to parent

### 3. Tool Specialization
- **Parent**: Minimal tools, focuses on `custom-agent`
- **Children**: Task-specific tools for their domain

### 4. Modularity
- Each agent is independent
- New agents can be added without modifying existing ones
- Clear naming convention prevents conflicts

### 5. Composability
- Agents can be combined in different ways
- Parent can delegate to any combination of children
- Sequential or parallel execution

## Configuration Summary

### Critical Requirements

1. **Parent Agent**:
   - MUST include `custom-agent` in tools
   - MUST have unique name
   - SHOULD be located at root of `.github/agents/`

2. **Child Agents**:
   - MUST have unique names
   - MUST have valid YAML frontmatter
   - SHOULD be in subdirectory
   - SHOULD have specialized tools for their role

3. **References**:
   - MUST use agent `name` from YAML
   - NOT file paths
   - Case-sensitive

## Performance Considerations

### Latency
```
Single Agent: Request → Processing → Response
             └─────── 1 invocation ────────┘

Nested Agency: Request → Parent → Child → Parent → Response
                        └─── 2-3 invocations ────────┘
```

**Trade-off**: Slight latency increase for better organization and specialization.

### Complexity Management
- Nested agency reduces complexity through specialization
- Each agent has simpler, focused responsibilities
- Easier to maintain and extend

---

**Last Updated:** 2025-11-16  
**Pattern Version:** 1.0  
**Diagram Format:** ASCII Art
