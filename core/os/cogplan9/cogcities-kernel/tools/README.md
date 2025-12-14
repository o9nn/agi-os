# Cognitive Cities Management Tools

This directory contains user-space tools for managing and interacting with the Plan9 Cognitive Cities kernel architecture.

## Tools Overview

### cogctl - Cognitive Control Utility
Command-line tool for managing cognitive cities components.

**Usage:**
```bash
# List cognitive domains
cogctl domains

# Create new cognitive namespace
cogctl create-namespace transportation /cognitive-cities/domains/transportation

# Bind neural channel between domains
cogctl bind-channel transportation energy 1000

# Start cognitive swarm
cogctl start-swarm traffic-optimizer transportation 5

# Monitor swarm status
cogctl swarm-status traffic-optimizer

# Detect emergent patterns
cogctl detect-emergence all 0.8

# Adapt namespace
cogctl adapt-namespace transportation auto

# Show statistics
cogctl stats
```

### cogmon - Cognitive Monitoring Tool
Real-time monitoring and visualization of cognitive cities architecture.

**Usage:**
```bash
# Live monitoring
cogmon -l

# Monitor specific domain
cogmon -d transportation -m

# Monitor neural channels
cogmon -c

# Display metrics
cogmon -m
```

### Demos

#### traffic-demo
Demonstrates the traffic optimization cognitive service with cross-domain coordination.

**Usage:**
```bash
traffic-demo
```

**What it demonstrates:**
- Creating cognitive namespaces
- Starting cognitive swarms
- Neural message passing between domains
- Traffic-energy coordination
- Emergent pattern detection

## Building the Tools

### Prerequisites
- Plan9 build environment
- Cognitive kernel device (devcognitive.c) loaded

### Build All Tools
```bash
cd tools
mk
```

### Build Individual Tool
```bash
cd cogctl
mk
```

### Clean Build Artifacts
```bash
cd tools
mk clean
```

## Integration with Kernel

The tools communicate with the cognitive kernel through the `/proc/cognitive` filesystem interface:

```
/proc/cognitive/
├── ctl          # Control commands (write)
├── domains      # List of domains (read)
├── monitor      # Live monitoring stream (read)
├── channels     # Neural channel list (read)
├── swarms       # Swarm status (read)
├── metrics      # System metrics (read)
└── stats        # Statistics (read)
```

## Example Workflow

1. **Start with the demo to see the system in action:**
   ```bash
   traffic-demo
   ```

2. **Check domain status:**
   ```bash
   cogctl domains
   ```

3. **Monitor the system:**
   ```bash
   cogmon -l
   ```

4. **Create a new cognitive namespace:**
   ```bash
   cogctl create-namespace governance /cognitive-cities/domains/governance
   ```

5. **Start a swarm:**
   ```bash
   cogctl start-swarm policy-sim governance 3
   ```

6. **View statistics:**
   ```bash
   cogctl stats
   ```

## Architecture

```
┌─────────────────────────────────────┐
│      User-Space Tools               │
│  ┌──────┐  ┌──────┐  ┌──────┐     │
│  │cogctl│  │cogmon│  │demos │     │
│  └──┬───┘  └──┬───┘  └──┬───┘     │
└─────┼─────────┼─────────┼──────────┘
      │         │         │
      └─────────┴─────────┘
              │
      /proc/cognitive/
              │
┌─────────────┴────────────────────────┐
│   Kernel-Space Components            │
│  ┌────────────────┐                  │
│  │ devcognitive.c │ Device Driver    │
│  └────────┬───────┘                  │
│           │                           │
│  ┌────────┴───────┐                  │
│  │  cognitive.c   │ Core Logic       │
│  └────────────────┘                  │
└──────────────────────────────────────┘
```

## Development

### Adding New Commands to cogctl

1. Add command function declaration
2. Add command to `commands[]` array
3. Implement command function
4. Update usage/help text

### Adding New Monitoring Views to cogmon

1. Add new display function
2. Add command-line flag for the view
3. Implement the display logic using `/proc/cognitive/*` files

### Creating New Demos

1. Create new `.c` file in `demos/` directory
2. Add to `demos/mkfile`
3. Demonstrate specific cognitive cities features
4. Include clear output and explanations

## Notes for Copilot Agents

**@copilot remembers:** These tools complete the cognitive cities architecture by providing:

1. **Command-line interface** for system administration
2. **Real-time monitoring** for observability
3. **Demonstration programs** to showcase capabilities
4. **Clean separation** between user-space and kernel-space

**Key design principles:**
- Follow Plan9 philosophy: simple, composable tools
- Everything through the filesystem interface
- Minimal dependencies
- Clear, informative output
- Easy to extend and modify

**Integration points:**
- Tools → `/proc/cognitive/*` → `devcognitive.c` → `cognitive.c`
- Commands flow down, status/metrics flow up
- Asynchronous monitoring via continuous reads
- Control via write operations to `/proc/cognitive/ctl`

This completes the user-space tooling for the cognitive cities architecture!
