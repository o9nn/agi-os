# Nested Agency Usage Examples

This document provides practical examples of how to use the nested-agency pattern with GitHub Copilot custom agents.

## Quick Start

### Invoking the Parent Agent

To use the nested agency, simply reference the parent coordinator:

```
@nested-agency-coordinator Please analyze the performance metrics in metrics.csv and create a comprehensive report
```

The coordinator will:
1. Analyze the request
2. Delegate data analysis to `nested-agency-child-1`
3. Delegate documentation to `nested-agency-child-2`
4. Synthesize the results

## Example Scenarios

### Example 1: Data Analysis Task

**User Request:**
```
@nested-agency-coordinator Analyze the user engagement data and provide insights
```

**Expected Flow:**
1. Parent receives request
2. Parent delegates to `nested-agency-child-1` for data analysis
3. Child-1 analyzes the data using shell/read tools
4. Child-1 returns analysis results
5. Parent synthesizes and presents insights

### Example 2: Documentation Task

**User Request:**
```
@nested-agency-coordinator Create documentation for the new API endpoints
```

**Expected Flow:**
1. Parent receives request
2. Parent delegates to `nested-agency-child-2` for documentation
3. Child-2 reads API code and creates documentation
4. Parent reviews and presents the documentation

### Example 3: Complex Multi-Step Task

**User Request:**
```
@nested-agency-coordinator Process the quarterly sales data, analyze trends, and create an executive summary
```

**Expected Flow:**
1. Parent receives request and plans approach
2. Parent delegates data processing to `nested-agency-child-1`
3. Child-1 processes and analyzes sales data
4. Parent receives analysis results
5. Parent delegates summary creation to `nested-agency-child-2`
6. Child-2 creates executive summary from analysis
7. Parent synthesizes final deliverable

## How the Delegation Works

### Parent Agent Delegation Syntax

The parent agent uses the `custom-agent` tool internally. From a user perspective, you just need to mention the child agent by name in your communication with the parent:

**Parent agent (internally):**
```
I'll delegate the data analysis portion to nested-agency-child-1...
[Uses custom-agent tool to invoke child]
```

### Child Agent Reference

Child agents are always referenced by their YAML `name` field:

✅ **Correct:**
- `nested-agency-child-1`
- `nested-agency-child-2`

❌ **Incorrect:**
- `.github/agents/nested-agency/child-agent-1.md`
- `child-agent-1`
- `child-agent-1.md`

## Testing the Configuration

### Verify Agent Names

Ensure all agents have unique, valid names:

```bash
# Check parent agent name
grep "^name:" .github/agents/nested-agency.md

# Check child agent names
grep "^name:" .github/agents/nested-agency/child-agent-*.md
```

Expected output:
```
name: nested-agency-coordinator
name: nested-agency-child-1
name: nested-agency-child-2
```

### Verify Parent Has Custom-Agent Tool

```bash
# Check parent agent tools
grep "^tools:" .github/agents/nested-agency.md
```

Expected output:
```
tools: ['read', 'search', 'custom-agent']
```

The `custom-agent` tool is **required** for the parent to delegate work.

### Validate YAML Syntax

```python
import yaml

def validate_agent(filepath):
    with open(filepath, 'r') as f:
        content = f.read()
    parts = content.split('---', 2)
    yaml_content = parts[1].strip()
    data = yaml.safe_load(yaml_content)
    
    print(f"Agent: {data['name']}")
    print(f"Tools: {data.get('tools', [])}")
    print(f"Valid: ✅")
    return data

# Validate all agents
validate_agent('.github/agents/nested-agency.md')
validate_agent('.github/agents/nested-agency/child-agent-1.md')
validate_agent('.github/agents/nested-agency/child-agent-2.md')
```

## Common Use Cases

### Use Case 1: Research and Documentation

**Scenario:** Research a topic and create documentation

**Request:**
```
@nested-agency-coordinator Research the latest trends in machine learning and create a technical document
```

**Delegation Pattern:**
1. Parent delegates research to child-1 (data analysis)
2. Parent delegates writing to child-2 (documentation)

### Use Case 2: Code Analysis and Reporting

**Scenario:** Analyze codebase and create report

**Request:**
```
@nested-agency-coordinator Analyze the test coverage in our codebase and create a quality report
```

**Delegation Pattern:**
1. Parent delegates code analysis to child-1 (analysis + shell)
2. Parent delegates report creation to child-2 (documentation)

### Use Case 3: Data Processing Pipeline

**Scenario:** Multi-stage data processing

**Request:**
```
@nested-agency-coordinator Process the raw sensor data, normalize it, and document the processing steps
```

**Delegation Pattern:**
1. Parent delegates data processing to child-1 (multiple iterations)
2. Parent delegates documentation to child-2
3. Parent synthesizes complete pipeline documentation

## Extending the Pattern

### Adding a Third Child Agent

To add a new specialized child agent:

1. Create `.github/agents/nested-agency/child-agent-3.md`:

```yaml
---
name: nested-agency-child-3
description: Specialized agent for testing and validation tasks
tools: ['read', 'edit', 'shell']
---

# Child Agent 3: Testing Specialist

I handle testing and validation tasks...
```

2. Update the parent agent to mention the new child in its documentation

3. Update this usage guide with new child's capabilities

### Creating a New Nested Agency

To create a completely new nested agency for a different domain:

1. Create parent: `.github/agents/my-new-agency.md`
2. Create folder: `.github/agents/my-new-agency/`
3. Create children: `.github/agents/my-new-agency/child-*.md`
4. Follow the same pattern with unique names

## Troubleshooting

### Problem: Child agent not invoked

**Check:**
- Parent has `custom-agent` in tools list
- Child agent name is correct (no typos)
- Child agent file has valid YAML frontmatter

### Problem: Name conflicts

**Solution:**
- Ensure all agent names are globally unique
- Use descriptive prefixes (e.g., `nested-agency-*`)
- Avoid generic names like `child-1`

### Problem: Tools not working

**Check:**
- Each agent has appropriate tools for its role
- Tool names are valid (see documentation)
- No typos in tool names

## Best Practices

1. **Clear Requests:** Be specific about what you want the nested agency to do
2. **Trust Delegation:** Let the parent agent decide which children to use
3. **Appropriate Specialization:** Only add new child agents for truly distinct capabilities
4. **Documentation:** Keep agent documentation updated as roles evolve
5. **Testing:** Test each agent individually before relying on complex delegation

## Resources

- [Nested Agency README](README.md) - Complete pattern documentation
- [Parent Agent](../nested-agency.md) - Coordinator agent
- [Child Agent 1](child-agent-1.md) - Data analysis specialist
- [Child Agent 2](child-agent-2.md) - Documentation specialist
- [Custom Agent Documentation](../my-custom-agent-with-mcp.md) - General custom agent reference

---

**Last Updated:** 2025-11-16  
**Pattern Version:** 1.0
