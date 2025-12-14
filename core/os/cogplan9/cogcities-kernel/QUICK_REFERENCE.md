# Cognitive Cities Quick Reference

## Essential Commands

### cogctl - Control Utility

```bash
# List cognitive domains
cogctl domains

# Create namespace
cogctl create-namespace <domain> <path>

# Bind neural channel
cogctl bind-channel <source> <target> [bandwidth]

# Start swarm
cogctl start-swarm <id> <domain> [agents]

# Check swarm status
cogctl swarm-status <id>

# Detect emergent patterns
cogctl detect-emergence [domain] [threshold]

# Adapt namespace
cogctl adapt-namespace <domain> [auto|manual]

# View statistics
cogctl stats
```

### Rooted Shell Commands

```bash
# Create shell from parentheses notation
cogctl rooted-create <domain> <parens>
cogctl rooted-create transportation '(()())'

# Enumerate all n-trees for domain
cogctl rooted-enumerate <domain> <max_size>
cogctl rooted-enumerate energy 5

# List rooted shell commands
cogctl rooted-list

# Get shell information
cogctl rooted-info <shell_id>

# Show rooted tree statistics
cogctl rooted-stats
```

### cogmon - Monitoring Tool

```bash
# Live monitoring
cogmon -l

# Monitor specific domain
cogmon -d <domain> -m

# Monitor neural channels
cogmon -c

# Display metrics
cogmon -m
```

### Demo Programs

```bash
# Run traffic optimization demo
traffic-demo

# Run energy management demo
energy-demo

# Run governance simulation demo
governance-demo

# Run full integration demo
integration-demo

# Run rooted shell namespace demo
rooted-shell-demo
```

## Filesystem Interface

### Read Operations

```bash
# List cognitive domains
cat /proc/cognitive/domains

# View neural channels
cat /proc/cognitive/channels

# Monitor live
cat /proc/cognitive/monitor

# Check swarms
cat /proc/cognitive/swarms

# View metrics
cat /proc/cognitive/metrics

# See statistics
cat /proc/cognitive/stats

# Rooted shell interface
cat /proc/cognitive/rooted/list
cat /proc/cognitive/rooted/trees
cat /proc/cognitive/rooted/shells
```

### Write Operations

```bash
# Create namespace
echo 'create-namespace domain path' > /proc/cognitive/ctl

# Bind channel
echo 'bind-channel src dst bandwidth' > /proc/cognitive/ctl

# Start swarm
echo 'start-swarm id domain agents' > /proc/cognitive/ctl

# Rooted shell operations
echo 'create transportation (()())' > /proc/cognitive/rooted/ctl
echo 'enumerate energy 5' > /proc/cognitive/rooted/ctl
echo 'stats' > /proc/cognitive/rooted/ctl

# Detect emergence
echo 'detect-emergence domain threshold' > /proc/cognitive/ctl

# Adapt namespace
echo 'adapt-namespace domain mode' > /proc/cognitive/ctl
```

## Cognitive Domains

| Domain | Focus | Services |
|--------|-------|----------|
| **transportation** | Traffic, Transit | Route optimization, Traffic flow, Public transit |
| **energy** | Grid, Renewables | Grid management, Demand prediction, EV charging |
| **governance** | Policy, Resources | Policy simulation, Resource allocation, Transparency |
| **environment** | Monitoring, Health | Air quality, Waste management, Green spaces |

## Neural Channels

Default inter-domain channels:

```
transportation <--[500]--> energy
transportation <--[300]--> governance
energy <--[400]--> environment
governance <--[200]--> environment
```

Bandwidth units: messages per second

## Swarm Types

| Swarm ID | Domain | Purpose | Agents |
|----------|--------|---------|--------|
| traffic-optimizer | transportation | Traffic flow optimization | 4-5 |
| grid-manager | energy | Smart grid management | 5-7 |
| policy-sim | governance | Policy impact simulation | 4-6 |
| env-monitor | environment | Environmental monitoring | 3-4 |

## Emergent Pattern Examples

| Pattern | Significance | Domains | Impact |
|---------|--------------|---------|--------|
| traffic-energy-sync | 0.85 | transport, energy | Peak demand -18% |
| renewable-storage-opt | 0.89 | energy, environment | Renewable +23% |
| coordinated-heat-response | 0.94 | all domains | 0 fatalities |
| equity-optimized | 0.91 | governance, all | 98% reach |

Significance scale: 0.0 (none) to 1.0 (exceptional)

## Common Workflows

### 1. Create New Domain

```bash
# Create namespace
cogctl create-namespace my-domain /cognitive-cities/domains/my-domain

# Bind to existing domain
cogctl bind-channel my-domain transportation 600

# Start swarm
cogctl start-swarm my-swarm my-domain 3

# Monitor
cogmon -d my-domain -m
```

### 2. Monitor System

```bash
# Start live monitoring
cogmon -l

# In another terminal, view domains
cogctl domains

# Check statistics
cogctl stats

# View channels
cogmon -c
```

### 3. Run Complete Demo

```bash
# Run integration demo (all domains)
integration-demo

# View results
cogctl stats

# Monitor live
cogmon -l
```

### 4. Emergency Response

```bash
# Declare emergency (via governance)
cogctl create-namespace emergency /cognitive-cities/emergency

# Connect all domains
cogctl bind-channel emergency transportation 1000
cogctl bind-channel emergency energy 1000
cogctl bind-channel emergency governance 1000
cogctl bind-channel emergency environment 1000

# Start emergency coordinator
cogctl start-swarm emergency-coord emergency 8

# Monitor response
cogmon -d emergency -l
```

## Architecture Quick View

```
User Tools (cogctl, cogmon)
         ↓
  /proc/cognitive/
         ↓
  devcognitive.c (device driver)
         ↓
  cognitive.c (core logic)
         ↓
  Plan9 Kernel
```

## Key Metrics

| Metric | Target | Typical |
|--------|--------|---------|
| Neural message latency | < 100ms | 50-80ms |
| Channel throughput | > 1000/sec | 1200-1500/sec |
| Swarm coherence | > 0.7 | 0.85-0.95 |
| Emergence detection | < 24 hours | 2-6 hours |
| System overhead | < 5% CPU | 2-4% CPU |

## Error Messages

| Error | Meaning | Solution |
|-------|---------|----------|
| cannot open /proc/cognitive | Device not loaded | Load devcognitive.c |
| write failed: permission denied | Need write access | Check /proc/cognitive/ctl permissions |
| unknown command | Invalid command | Check cogctl help |
| domain not found | Domain doesn't exist | Create with create-namespace |
| bandwidth exceeded | Channel overloaded | Increase bandwidth or optimize |

## Performance Tips

1. **Optimize bandwidth**: Monitor channel load, adjust bandwidth as needed
2. **Right-size swarms**: Start with 3-5 agents, scale based on load
3. **Use adaptive mode**: Enable auto adaptation for dynamic loads
4. **Monitor regularly**: Use cogmon to catch issues early
5. **Detect emergence**: Set threshold to 0.7-0.8 for meaningful patterns

## Security Notes

- Namespace isolation provides security boundaries
- Control operations require write permission to /proc/cognitive/ctl
- Monitoring operations are read-only
- Audit all control commands
- Review emergent patterns for unexpected behaviors

## Troubleshooting

**Problem**: No output from cogmon
- **Solution**: Check /proc/cognitive/monitor exists and is readable

**Problem**: Swarm won't start
- **Solution**: Verify domain exists first with `cogctl domains`

**Problem**: High latency
- **Solution**: Check channel bandwidth with `cogmon -c`, increase if needed

**Problem**: Low coherence
- **Solution**: Reduce swarm size or increase coordination bandwidth

**Problem**: No emergence detected
- **Solution**: Need cross-domain activity; run a demo to generate traffic

## Resources

- Full Guide: `/GETTING_STARTED.md`
- Architecture: `/docs/ARCHITECTURE.md`
- Tools: `/tools/README.md`
- Implementation: `/IMPLEMENTATION_SUMMARY.md`

## Support

Run demos to see the system in action:
```bash
cd tools/demos
./traffic-demo
./energy-demo
./governance-demo
./integration-demo
```

Check comprehensive documentation in `/docs` directory.

---

**Quick Start**: Run `traffic-demo` to see the system in action!
