# Nested Agency Pattern

This folder demonstrates the **nested-agency pattern** for GitHub Copilot custom agents - a hierarchical organization of agents where a parent coordinator delegates work to specialized child agents.

## Overview

Nested agency allows you to:
- **Organize** complex agent architectures with clear separation of concerns
- **Specialize** child agents for specific domains or tasks
- **Coordinate** work across multiple agents through a parent agent
- **Scale** agent capabilities by adding new specialized child agents

## File Structure

```
.github/agents/
├── nested-agency.md              # Parent coordinator agent
└── nested-agency/                # Child agents and resources
    ├── README.md                 # This file - documentation
    ├── child-agent-1.md          # Child agent for data analysis
    ├── child-agent-2.md          # Child agent for documentation
    └── relevant-file.ext         # Example resource file
```

## Agent Roles

### Parent Agent: `nested-agency-coordinator`
**File**: `.github/agents/nested-agency.md`

The coordinator agent:
- Analyzes incoming requests
- Breaks down complex tasks into subtasks
- Delegates to appropriate child agents
- Synthesizes results from multiple agents
- **Must include** `custom-agent` in its tools list

### Child Agent 1: `nested-agency-child-1`
**File**: `.github/agents/nested-agency/child-agent-1.md`

Specializes in:
- Data analysis and processing
- Statistical operations
- Computational tasks
- Script execution

### Child Agent 2: `nested-agency-child-2`
**File**: `.github/agents/nested-agency/child-agent-2.md`

Specializes in:
- Documentation creation
- Report writing
- Communication artifacts
- Information organization

## How Nested Agency Works

### 1. Agent Invocation

Users interact with the parent agent by name:
```
@nested-agency-coordinator Please analyze the data and create a report
```

### 2. Task Analysis

The parent agent analyzes the request and determines which child agents to use:
- Does it need data analysis? → Delegate to `nested-agency-child-1`
- Does it need documentation? → Delegate to `nested-agency-child-2`
- Does it need both? → Coordinate work between both child agents

### 3. Delegation

The parent uses the `custom-agent` tool to delegate to children:
```
I'll delegate the data analysis to nested-agency-child-1...
Then I'll ask nested-agency-child-2 to document the results...
```

### 4. Result Synthesis

The parent collects results from child agents and synthesizes a complete response.

## Configuration Requirements

### Parent Agent Requirements

```yaml
---
name: nested-agency-coordinator
description: Coordinates work across specialized child agents
tools: ['read', 'search', 'custom-agent']  # Must include 'custom-agent'
---
```

**Critical**: The parent MUST include `custom-agent` in its tools list to delegate work.

### Child Agent Requirements

```yaml
---
name: nested-agency-child-1  # Must be unique
description: Specialized child agent for specific tasks
tools: ['read', 'edit', 'search', 'shell']  # As needed for specialization
---
```

**Critical**: Each child must have a unique name (no placeholders).

## Naming Conventions

### ✅ Correct Naming

- **Parent**: `nested-agency-coordinator` (descriptive, unique)
- **Child 1**: `nested-agency-child-1` (unique, indicates relationship)
- **Child 2**: `nested-agency-child-2` (unique, indicates relationship)

### ❌ Incorrect Naming

- ~~`{{nested-agency}}`~~ (placeholders not allowed)
- ~~`{{nested-agency}}-child-agent-1`~~ (placeholders not allowed)
- ~~`child-agent-1`~~ (not unique enough, could conflict)

## Referencing Child Agents

### ✅ Correct References

Reference by agent name from YAML frontmatter:
- `nested-agency-child-1`
- `nested-agency-child-2`

### ❌ Incorrect References

Do NOT reference by file path:
- ~~`.github/agents/nested-agency/child-agent-1.md`~~
- ~~`child-agent-1.md`~~

## Important Limitations

### GitHub Copilot vs VS Code

⚠️ **GitHub Copilot Coding Agent does NOT support**:
- `handoffs` property (VS Code only)
- Direct MCP server configuration in repository-level agents

✅ **GitHub Copilot Coding Agent DOES support**:
- `custom-agent` tool for delegation
- Child agents in subdirectories
- Unique agent names

### Tool Access

- Parent agents need `custom-agent` tool to delegate
- Child agents should have tools appropriate for their specialization
- All agents can use standard tools: `read`, `edit`, `search`, `shell`

## Example Workflows

### Example 1: Sequential Delegation

```
User: @nested-agency-coordinator Analyze data.csv and create documentation

Parent: I'll coordinate this task:
  1. First, I'll delegate data analysis to nested-agency-child-1
  2. Then, I'll ask nested-agency-child-2 to create documentation

Child 1: [performs analysis] Results: {...}

Parent: [receives results, delegates to child 2]

Child 2: [creates documentation] Documentation created

Parent: [synthesizes] Here's your analysis and documentation
```

### Example 2: Parallel Work

```
User: @nested-agency-coordinator Process multiple datasets

Parent: I'll coordinate parallel work:
  - nested-agency-child-1: Process dataset A
  - nested-agency-child-1: Process dataset B
  - nested-agency-child-2: Create summary docs

Parent: [synthesizes all results] Complete results: {...}
```

## Extending the Pattern

### Adding New Child Agents

1. Create new agent file: `.github/agents/nested-agency/child-agent-3.md`
2. Define unique name: `nested-agency-child-3`
3. Specify specialization in description
4. Configure appropriate tools
5. Update parent agent documentation to mention new child
6. Update this README

### Adding Resources

Place shared resources in the `nested-agency` folder:
- Data files
- Templates
- Configuration files
- Reference documentation

Child agents can reference these using relative paths or the `read` tool.

## Best Practices

### 1. Clear Specialization
Each child agent should have a clear, non-overlapping area of expertise.

### 2. Minimal Tool Sets
Give each agent only the tools it needs for its specialization.

### 3. Comprehensive Documentation
Document each agent's role, capabilities, and example use cases.

### 4. Unique Naming
Use descriptive, unique names that indicate the agent hierarchy.

### 5. Parent Coordination
The parent should focus on coordination, not doing the work itself.

### 6. Resource Sharing
Place shared resources in the nested-agency folder for all agents to access.

## Troubleshooting

### Problem: Child agent not found
**Solution**: Check that:
- Child agent name matches exactly (case-sensitive)
- No placeholders like `{{...}}` in name field
- Agent file has valid YAML frontmatter

### Problem: Parent can't delegate
**Solution**: Ensure parent agent includes `custom-agent` in tools list

### Problem: Agents conflict
**Solution**: Ensure all agent names are globally unique across repository

## Advanced Patterns

### Multi-Level Nesting

You can nest agents multiple levels deep:
```
Parent → Child → Grandchild
```

However, keep it simple - 1-2 levels is usually sufficient.

### Cross-Agent Communication

Currently, agents communicate through the parent coordinator. Direct child-to-child communication is not supported.

### Resource Access

All agents in the hierarchy can access:
- Repository files via `read` tool
- Shared resources in `nested-agency` folder
- Standard tools as configured

## References

- [GitHub Copilot Custom Agents Documentation](https://docs.github.com/en/copilot/customizing-copilot/creating-custom-agents)
- [Custom Agent Configuration Reference](../ my-custom-agent-with-mcp.md)
- [Model Context Protocol (MCP)](https://modelcontextprotocol.io/)

## License

This pattern and example configuration are provided as-is for educational purposes. Adapt as needed for your use case.

---

**Last Updated**: 2025-11-16  
**Pattern Version**: 1.0  
**Maintainer**: Repository maintainers
