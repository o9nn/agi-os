# Getting Started with Plan9 Cognitive Cities Kernel

This guide will help you get started with the Cognitive Cities architecture in Plan9.

## Overview

The Plan9 Cognitive Cities Kernel extends Plan9's elegant architecture to support distributed cognitive systems for smart cities. It provides:

- **Cognitive Namespaces**: Domain-specific organization (transportation, energy, governance, environment)
- **Neural Transport Channels**: High-bandwidth communication between domains
- **Cognitive Swarms**: Coordinated process groups for collective intelligence
- **Emergence Detection**: Automatic discovery of cross-domain patterns

## Architecture Components

### Kernel Components

1. **port/cognitive.c**: Core cognitive capabilities
   - Neural channel operations
   - Cognitive namespace management
   - Swarm coordination
   - Pattern detection

2. **port/devcognitive.c**: Filesystem device driver
   - Provides `/proc/cognitive` interface
   - Command processing via `/proc/cognitive/ctl`
   - Status monitoring via `/proc/cognitive/*`

### User-Space Tools

1. **cogctl**: Control and management utility
2. **cogmon**: Real-time monitoring tool
3. **traffic-demo**: Traffic optimization demonstration

## Quick Start

### 1. Build the Kernel (if building from source)

```bash
# Build for PC architecture
cd pc
mk
```

### 2. Build User-Space Tools

```bash
cd tools
mk
```

### 3. Run the Traffic Demo

The easiest way to see the system in action:

```bash
cd tools/demos
./traffic-demo
```

This will demonstrate:
- Creating cognitive domains (transportation, energy)
- Starting a traffic optimization swarm
- Cross-domain coordination
- Emergent pattern detection

### 4. Explore with cogctl

```bash
# List available cognitive domains
cogctl domains

# View current statistics
cogctl stats

# Create a new namespace
cogctl create-namespace governance /cognitive-cities/domains/governance

# Bind a neural channel
cogctl bind-channel governance transportation 500

# Start a cognitive swarm
cogctl start-swarm policy-sim governance 3

# Check swarm status
cogctl swarm-status policy-sim

# Detect emergent patterns
cogctl detect-emergence all 0.7

# Adapt a namespace
cogctl adapt-namespace transportation auto
```

### 5. Monitor the System

```bash
# Live monitoring
cogmon -l

# View metrics
cogmon -m

# Monitor neural channels
cogmon -c

# Monitor specific domain
cogmon -d transportation -m
```

## Understanding the System

### Cognitive Domains

The system provides four primary cognitive domains:

1. **Transportation**: Traffic flow, routing, public transit
2. **Energy**: Grid management, renewable sources, demand prediction
3. **Governance**: Policy simulation, citizen engagement, resource allocation
4. **Environment**: Air quality, waste management, green spaces

Each domain operates in its own namespace with:
- Isolated cognitive services
- Domain-specific optimization
- Controlled cross-domain communication
- Independent scaling

### Neural Transport Channels

Channels provide cognitive communication between domains:

```
transportation <--[bandwidth: 500]--> energy
transportation <--[bandwidth: 300]--> governance
energy <--[bandwidth: 400]--> environment
governance <--[bandwidth: 200]--> environment
```

Channels support:
- Adaptive bandwidth allocation
- Priority-based message routing
- QoS management
- Automatic load balancing

### Cognitive Swarms

Swarms are coordinated process groups that provide:
- Collective intelligence
- Distributed decision making
- Emergent behaviors
- Self-organization

Example swarms:
- `traffic-optimizer`: Optimizes traffic flow in transportation domain
- `grid-manager`: Manages energy grid in energy domain
- `policy-simulator`: Simulates policies in governance domain

### Emergent Patterns

The system automatically detects cross-domain patterns:

**Example: Traffic-Energy Synchronization**
- Traffic optimization coordinates with energy grid
- Reduces peak demand during rush hours
- Detected automatically by the system
- Significance score: 0.85 (high impact)

## System Interface

### /proc/cognitive Filesystem

```
/proc/cognitive/
├── ctl          # Control commands (write)
├── domains      # List of domains (read)
├── monitor      # Live monitoring (read)
├── channels     # Channel status (read)
├── swarms       # Swarm status (read)
├── metrics      # System metrics (read)
└── stats        # Statistics (read)
```

### Control Commands

Write commands to `/proc/cognitive/ctl`:

```bash
# Create namespace
echo "create-namespace domain path" > /proc/cognitive/ctl

# Bind channel
echo "bind-channel source target bandwidth" > /proc/cognitive/ctl

# Start swarm
echo "start-swarm id domain agents" > /proc/cognitive/ctl

# Detect emergence
echo "detect-emergence domain threshold" > /proc/cognitive/ctl

# Adapt namespace
echo "adapt-namespace domain mode" > /proc/cognitive/ctl
```

### Reading Status

```bash
# List domains
cat /proc/cognitive/domains

# View channel status
cat /proc/cognitive/channels

# Monitor system
cat /proc/cognitive/monitor

# View metrics
cat /proc/cognitive/metrics

# Check statistics
cat /proc/cognitive/stats
```

## Example Workflows

### Workflow 1: Traffic Optimization

```bash
# 1. Create transportation namespace
cogctl create-namespace transportation /cognitive-cities/domains/transportation

# 2. Start traffic optimization swarm
cogctl start-swarm traffic-optimizer transportation 5

# 3. Monitor traffic domain
cogmon -d transportation -m

# 4. Create energy coordination
cogctl create-namespace energy /cognitive-cities/domains/energy
cogctl bind-channel transportation energy 1000

# 5. Detect emergence
cogctl detect-emergence all 0.8
```

### Workflow 2: Energy Management

```bash
# 1. Create energy namespace
cogctl create-namespace energy /cognitive-cities/domains/energy

# 2. Start grid management swarm
cogctl start-swarm grid-manager energy 3

# 3. Bind to transportation for coordination
cogctl bind-channel energy transportation 1000

# 4. Monitor energy metrics
cogmon -d energy -m

# 5. Adapt based on load
cogctl adapt-namespace energy auto
```

### Workflow 3: Policy Simulation

```bash
# 1. Create governance namespace
cogctl create-namespace governance /cognitive-cities/domains/governance

# 2. Start policy simulation swarm
cogctl start-swarm policy-sim governance 4

# 3. Create cross-domain channels for impact analysis
cogctl bind-channel governance transportation 300
cogctl bind-channel governance energy 300
cogctl bind-channel governance environment 300

# 4. Monitor policy simulations
cogmon -d governance -m

# 5. Detect cross-domain policy impacts
cogctl detect-emergence governance 0.7
```

## Development Guide

### Adding New Cognitive Services

1. Define service in appropriate domain namespace
2. Implement service logic using neural channels
3. Register service with domain
4. Create demo/test program

### Extending Neural Transport

1. Add new message types to `cognitive.c`
2. Implement routing logic
3. Add QoS policies
4. Update channel monitoring

### Creating Custom Swarms

1. Define swarm coordination protocol
2. Implement agent behavior
3. Add coherence calculation
4. Test with multiple agents

## Troubleshooting

### Cannot open /proc/cognitive files

**Solution**: Ensure the cognitive device is loaded in the kernel.

### Commands to /proc/cognitive/ctl fail

**Solution**: Check command syntax. Use `cogctl help` for correct format.

### Swarms not starting

**Solution**: Verify domain exists first. Use `cogctl domains` to list.

### No emergent patterns detected

**Solution**: Need cross-domain activity. Run traffic-demo to generate activity.

## Next Steps

1. **Explore the documentation**:
   - `docs/cognitive-architecture/namespace-design.md`
   - `docs/cognitive-architecture/neural-transport.md`
   - `docs/cognitive-architecture/swarm-coordination.md`

2. **Run demos**:
   - Traffic optimization demo
   - Create your own cognitive service

3. **Monitor the system**:
   - Use cogmon for real-time monitoring
   - Watch for emergent patterns

4. **Extend the architecture**:
   - Add new cognitive domains
   - Implement custom swarm behaviors
   - Create domain-specific services

## Resources

- **Main README**: `/README.md` - Project overview
- **Documentation**: `/docs/` - Detailed architecture docs
- **Tools README**: `/tools/README.md` - Tool-specific documentation
- **Note2Self**: `/docs/note2self-copilot.md` - Development context

## Support

For questions or issues:
1. Review the documentation in `/docs`
2. Check the note2self file for implementation context
3. Examine example programs in `/tools/demos`

---

**Welcome to the Cognitive Cities revolution!** 🏙️🧠

This architecture demonstrates how Plan9's elegant design can be extended to support distributed cognitive systems, creating truly intelligent urban infrastructure that adapts, learns, and evolves.
