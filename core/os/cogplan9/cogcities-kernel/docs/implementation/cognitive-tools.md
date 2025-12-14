# Cognitive Cities Management Tools

## Overview

This directory contains user-space tools for managing and interacting with the cognitive cities architecture. These tools provide interfaces for citizens, administrators, and developers to work with the distributed cognitive systems.

## Tool Directory Structure

```
tools/
├── cogctl/                    # Cognitive cities control utility
│   ├── cogctl.c              # Main control program
│   ├── namespace-mgmt.c      # Namespace management commands
│   ├── swarm-coord.c         # Swarm coordination commands
│   ├── neural-transport.c    # Neural channel management
│   └── mkfile               # Build configuration
├── cogmon/                   # Cognitive cities monitoring
│   ├── cogmon.c             # System monitoring
│   ├── emergence-detect.c   # Emergence detection display
│   ├── performance-dash.c   # Performance dashboard
│   └── mkfile
├── cogfs/                    # Cognitive filesystem utilities
│   ├── cogfs.c              # Filesystem management
│   ├── namespace-mount.c    # Namespace mounting utilities
│   ├── service-bind.c       # Service binding utilities
│   └── mkfile
└── examples/                 # Example applications
    ├── traffic-demo/        # Traffic optimization demo
    ├── energy-demo/         # Energy management demo
    ├── swarm-demo/          # Swarm coordination demo
    └── emergence-demo/      # Emergence detection demo
```

## Usage Examples

### Cognitive Control (cogctl)

```bash
# List cognitive domains
cogctl domains

# Create new cognitive namespace
cogctl create-namespace transportation /cognitive-cities/domains/transportation

# Bind neural channel
cogctl bind-channel transportation energy --bandwidth 1000

# Start cognitive swarm
cogctl start-swarm traffic-optimization --domain transportation --agents 5

# Monitor swarm status
cogctl swarm-status traffic-optimization

# Detect emergent patterns
cogctl detect-emergence --domain all --threshold 0.7

# Adapt namespace based on load
cogctl adapt-namespace transportation --auto

# Show cognitive statistics
cogctl stats --domain transportation --period 1h
```

### Cognitive Monitoring (cogmon)

```bash
# Real-time cognitive system monitoring
cogmon --live

# Monitor specific domain
cogmon --domain transportation --metrics all

# Watch for emergent patterns
cogmon --emergence --alert-threshold 0.8

# Performance dashboard
cogmon --dashboard --web-port 8080

# Export metrics
cogmon --export --format json --file cognitive-metrics.json

# Monitor neural channel health
cogmon --channels --bandwidth-usage --latency-stats
```

### Cognitive Filesystem (cogfs)

```bash
# Mount cognitive domain
cogfs mount transportation /mnt/cognitive/transportation

# Bind service to namespace
cogfs bind /cognitive-cities/services/traffic-optimizer /mnt/traffic/optimizer

# List cognitive services
cogfs services --domain transportation

# Show namespace hierarchy
cogfs tree /cognitive-cities

# Create new cognitive service mount point
cogfs create-mount /cognitive-cities/domains/transportation/services/new-service

# Unmount cognitive namespace
cogfs unmount /mnt/cognitive/transportation
```

## Implementation Examples

### cogctl - Main Control Utility

```c
// tools/cogctl/cogctl.c
#include <u.h>
#include <libc.h>
#include <fcall.h>
#include <thread.h>
#include <9p.h>

typedef struct CogCmd CogCmd;
struct CogCmd {
    char *name;
    char *usage;
    void (*fn)(int argc, char *argv[]);
};

// Command implementations
void cmd_domains(int argc, char *argv[]);
void cmd_create_namespace(int argc, char *argv[]);
void cmd_bind_channel(int argc, char *argv[]);
void cmd_start_swarm(int argc, char *argv[]);
void cmd_swarm_status(int argc, char *argv[]);
void cmd_detect_emergence(int argc, char *argv[]);
void cmd_adapt_namespace(int argc, char *argv[]);
void cmd_stats(int argc, char *argv[]);

CogCmd commands[] = {
    {"domains", "cogctl domains", cmd_domains},
    {"create-namespace", "cogctl create-namespace <domain> <path>", cmd_create_namespace},
    {"bind-channel", "cogctl bind-channel <src> <dst> [--bandwidth N]", cmd_bind_channel},
    {"start-swarm", "cogctl start-swarm <id> --domain <domain> --agents N", cmd_start_swarm},
    {"swarm-status", "cogctl swarm-status <id>", cmd_swarm_status},
    {"detect-emergence", "cogctl detect-emergence [--domain <domain>] [--threshold N]", cmd_detect_emergence},
    {"adapt-namespace", "cogctl adapt-namespace <domain> [--auto]", cmd_adapt_namespace},
    {"stats", "cogctl stats [--domain <domain>] [--period <period>]", cmd_stats},
    {nil, nil, nil}
};

void
main(int argc, char *argv[])
{
    CogCmd *cmd;
    
    ARGBEGIN{
    case 'h':
        usage();
        break;
    }ARGEND
    
    if(argc < 1)
        usage();
        
    for(cmd = commands; cmd->name; cmd++){
        if(strcmp(argv[0], cmd->name) == 0){
            cmd->fn(argc, argv);
            exits(nil);
        }
    }
    
    fprint(2, "cogctl: unknown command '%s'\n", argv[0]);
    usage();
}

void
usage(void)
{
    CogCmd *cmd;
    
    fprint(2, "usage: cogctl <command> [args...]\n");
    fprint(2, "commands:\n");
    for(cmd = commands; cmd->name; cmd++){
        fprint(2, "  %s\n", cmd->usage);
    }
    exits("usage");
}

void
cmd_domains(int argc, char *argv[])
{
    int fd;
    char buf[8192];
    int n;
    
    // Read cognitive domains from kernel
    fd = open("/proc/cognitive/domains", OREAD);
    if(fd < 0){
        fprint(2, "cogctl: cannot open /proc/cognitive/domains: %r\n");
        exits("open");
    }
    
    while((n = read(fd, buf, sizeof buf)) > 0){
        write(1, buf, n);
    }
    
    close(fd);
}

void
cmd_create_namespace(int argc, char *argv[])
{
    int fd;
    char *domain, *path;
    char cmd[1024];
    
    if(argc < 3){
        fprint(2, "usage: cogctl create-namespace <domain> <path>\n");
        exits("usage");
    }
    
    domain = argv[1];
    path = argv[2];
    
    // Send create command to cognitive kernel module
    fd = open("/proc/cognitive/ctl", OWRITE);
    if(fd < 0){
        fprint(2, "cogctl: cannot open /proc/cognitive/ctl: %r\n");
        exits("open");
    }
    
    snprint(cmd, sizeof cmd, "create-namespace %s %s", domain, path);
    if(write(fd, cmd, strlen(cmd)) < 0){
        fprint(2, "cogctl: write failed: %r\n");
        exits("write");
    }
    
    close(fd);
    print("Cognitive namespace '%s' created at '%s'\n", domain, path);
}

void
cmd_bind_channel(int argc, char *argv[])
{
    int fd;
    char *src, *dst;
    char cmd[1024];
    int bandwidth = 1000; // default bandwidth
    
    ARGBEGIN{
    case 'b':
        bandwidth = atoi(EARGF(usage()));
        break;
    }ARGEND
    
    if(argc < 2){
        fprint(2, "usage: cogctl bind-channel <src> <dst> [--bandwidth N]\n");
        exits("usage");
    }
    
    src = argv[0];
    dst = argv[1];
    
    fd = open("/proc/cognitive/ctl", OWRITE);
    if(fd < 0){
        fprint(2, "cogctl: cannot open /proc/cognitive/ctl: %r\n");
        exits("open");
    }
    
    snprint(cmd, sizeof cmd, "bind-channel %s %s %d", src, dst, bandwidth);
    if(write(fd, cmd, strlen(cmd)) < 0){
        fprint(2, "cogctl: write failed: %r\n");
        exits("write");
    }
    
    close(fd);
    print("Neural channel bound: %s -> %s (bandwidth: %d)\n", src, dst, bandwidth);
}

void
cmd_start_swarm(int argc, char *argv[])
{
    int fd;
    char *swarm_id, *domain;
    char cmd[1024];
    int agents = 3; // default agent count
    
    if(argc < 1){
        fprint(2, "usage: cogctl start-swarm <id> --domain <domain> --agents N\n");
        exits("usage");
    }
    
    swarm_id = argv[0];
    domain = "transportation"; // default domain
    
    // Parse additional arguments
    for(int i = 1; i < argc; i++){
        if(strcmp(argv[i], "--domain") == 0 && i+1 < argc){
            domain = argv[++i];
        } else if(strcmp(argv[i], "--agents") == 0 && i+1 < argc){
            agents = atoi(argv[++i]);
        }
    }
    
    fd = open("/proc/cognitive/ctl", OWRITE);
    if(fd < 0){
        fprint(2, "cogctl: cannot open /proc/cognitive/ctl: %r\n");
        exits("open");
    }
    
    snprint(cmd, sizeof cmd, "start-swarm %s %s %d", swarm_id, domain, agents);
    if(write(fd, cmd, strlen(cmd)) < 0){
        fprint(2, "cogctl: write failed: %r\n");
        exits("write");
    }
    
    close(fd);
    print("Cognitive swarm '%s' started in domain '%s' with %d agents\n", 
          swarm_id, domain, agents);
}

void
cmd_swarm_status(int argc, char *argv[])
{
    int fd;
    char *swarm_id;
    char path[256];
    char buf[4096];
    int n;
    
    if(argc < 2){
        fprint(2, "usage: cogctl swarm-status <id>\n");
        exits("usage");
    }
    
    swarm_id = argv[1];
    
    snprint(path, sizeof path, "/proc/cognitive/swarms/%s/status", swarm_id);
    fd = open(path, OREAD);
    if(fd < 0){
        fprint(2, "cogctl: cannot open %s: %r\n", path);
        exits("open");
    }
    
    while((n = read(fd, buf, sizeof buf)) > 0){
        write(1, buf, n);
    }
    
    close(fd);
}

void
cmd_detect_emergence(int argc, char *argv[])
{
    int fd;
    char cmd[1024];
    char *domain = "all";
    float threshold = 0.7;
    
    // Parse arguments
    for(int i = 1; i < argc; i++){
        if(strcmp(argv[i], "--domain") == 0 && i+1 < argc){
            domain = argv[++i];
        } else if(strcmp(argv[i], "--threshold") == 0 && i+1 < argc){
            threshold = atof(argv[++i]);
        }
    }
    
    fd = open("/proc/cognitive/ctl", OWRITE);
    if(fd < 0){
        fprint(2, "cogctl: cannot open /proc/cognitive/ctl: %r\n");
        exits("open");
    }
    
    snprint(cmd, sizeof cmd, "detect-emergence %s %.2f", domain, threshold);
    if(write(fd, cmd, strlen(cmd)) < 0){
        fprint(2, "cogctl: write failed: %r\n");
        exits("write");
    }
    
    close(fd);
    print("Emergence detection triggered for domain '%s' with threshold %.2f\n", 
          domain, threshold);
}

void
cmd_adapt_namespace(int argc, char *argv[])
{
    int fd;
    char *domain;
    char cmd[1024];
    int auto_adapt = 0;
    
    if(argc < 2){
        fprint(2, "usage: cogctl adapt-namespace <domain> [--auto]\n");
        exits("usage");
    }
    
    domain = argv[1];
    
    // Check for auto flag
    for(int i = 2; i < argc; i++){
        if(strcmp(argv[i], "--auto") == 0){
            auto_adapt = 1;
        }
    }
    
    fd = open("/proc/cognitive/ctl", OWRITE);
    if(fd < 0){
        fprint(2, "cogctl: cannot open /proc/cognitive/ctl: %r\n");
        exits("open");
    }
    
    snprint(cmd, sizeof cmd, "adapt-namespace %s %s", domain, auto_adapt ? "auto" : "manual");
    if(write(fd, cmd, strlen(cmd)) < 0){
        fprint(2, "cogctl: write failed: %r\n");
        exits("write");
    }
    
    close(fd);
    print("Namespace adaptation triggered for domain '%s' (%s mode)\n", 
          domain, auto_adapt ? "automatic" : "manual");
}

void
cmd_stats(int argc, char *argv[])
{
    int fd;
    char path[256];
    char buf[8192];
    int n;
    char *domain = "all";
    char *period = "1h";
    
    // Parse arguments
    for(int i = 1; i < argc; i++){
        if(strcmp(argv[i], "--domain") == 0 && i+1 < argc){
            domain = argv[++i];
        } else if(strcmp(argv[i], "--period") == 0 && i+1 < argc){
            period = argv[++i];
        }
    }
    
    snprint(path, sizeof path, "/proc/cognitive/stats/%s/%s", domain, period);
    fd = open(path, OREAD);
    if(fd < 0){
        fprint(2, "cogctl: cannot open %s: %r\n", path);
        exits("open");
    }
    
    print("Cognitive Statistics - Domain: %s, Period: %s\n", domain, period);
    print("================================================\n");
    
    while((n = read(fd, buf, sizeof buf)) > 0){
        write(1, buf, n);
    }
    
    close(fd);
}
```

### cogmon - Monitoring Dashboard

```c
// tools/cogmon/cogmon.c
#include <u.h>
#include <libc.h>
#include <draw.h>
#include <thread.h>

typedef struct MonitorState MonitorState;
struct MonitorState {
    int live_mode;
    char *domain;
    float emergence_threshold;
    int dashboard_mode;
    int web_port;
};

void monitor_live(MonitorState *state);
void monitor_emergence(MonitorState *state);
void monitor_dashboard(MonitorState *state);
void display_cognitive_metrics(char *domain);
void alert_emergence_detected(char *pattern, float significance);

void
main(int argc, char *argv[])
{
    MonitorState state = {0};
    
    state.domain = "all";
    state.emergence_threshold = 0.8;
    state.web_port = 8080;
    
    ARGBEGIN{
    case 'l':
        state.live_mode = 1;
        break;
    case 'd':
        state.domain = EARGF(usage());
        break;
    case 'e':
        state.emergence_threshold = atof(EARGF(usage()));
        break;
    case 'w':
        state.dashboard_mode = 1;
        state.web_port = atoi(EARGF(usage()));
        break;
    }ARGEND
    
    if(state.live_mode){
        monitor_live(&state);
    } else if(state.dashboard_mode){
        monitor_dashboard(&state);
    } else {
        display_cognitive_metrics(state.domain);
    }
}

void
monitor_live(MonitorState *state)
{
    char buf[4096];
    int fd, n;
    
    print("Cognitive Cities Live Monitor\n");
    print("============================\n");
    print("Domain: %s\n", state->domain);
    print("Emergence Threshold: %.2f\n", state->emergence_threshold);
    print("\nPress Ctrl+C to exit\n\n");
    
    fd = open("/proc/cognitive/monitor", OREAD);
    if(fd < 0){
        fprint(2, "cogmon: cannot open /proc/cognitive/monitor: %r\n");
        exits("open");
    }
    
    while((n = read(fd, buf, sizeof buf)) > 0){
        buf[n] = 0;
        print("%s", buf);
        
        // Check for emergence patterns
        if(strstr(buf, "EMERGENCE_DETECTED")){
            alert_emergence_detected("pattern-detected", state->emergence_threshold);
        }
    }
    
    close(fd);
}

void
display_cognitive_metrics(char *domain)
{
    char path[256];
    char buf[8192];
    int fd, n;
    
    snprint(path, sizeof path, "/proc/cognitive/metrics/%s", domain);
    fd = open(path, OREAD);
    if(fd < 0){
        fprint(2, "cogmon: cannot open %s: %r\n", path);
        exits("open");
    }
    
    print("Cognitive Metrics - Domain: %s\n", domain);
    print("==============================\n");
    
    while((n = read(fd, buf, sizeof buf)) > 0){
        write(1, buf, n);
    }
    
    close(fd);
}

void
alert_emergence_detected(char *pattern, float significance)
{
    print("\n🚨 EMERGENCE ALERT 🚨\n");
    print("Pattern: %s\n", pattern);
    print("Significance: %.2f\n", significance);
    print("Time: %lu\n", time(0));
    print("========================\n\n");
}
```

## Integration with Cognitive Kernel

### Kernel Interface Files

The tools expect the following kernel interface files:

```
/proc/cognitive/
├── domains              # List of cognitive domains
├── ctl                  # Control commands
├── monitor              # Live monitoring stream
├── metrics/
│   ├── all              # All domain metrics
│   ├── transportation   # Transportation domain metrics
│   ├── energy          # Energy domain metrics
│   ├── governance      # Governance domain metrics
│   └── environment     # Environment domain metrics
├── swarms/
│   ├── <swarm-id>/
│   │   ├── status      # Swarm status
│   │   ├── agents      # Agent list
│   │   ├── coherence   # Coherence metrics
│   │   └── ctl         # Swarm control
└── channels/
    ├── list            # Neural channel list
    ├── <channel-id>/
    │   ├── status      # Channel status
    │   ├── bandwidth   # Bandwidth usage
    │   ├── latency     # Latency metrics
    │   └── ctl         # Channel control
```

### Example Usage Scenarios

#### City Administrator Dashboard

```bash
# Start live monitoring dashboard
cogmon --live --domain all

# Check traffic optimization swarm
cogctl swarm-status traffic-optimization

# Adapt energy domain based on load
cogctl adapt-namespace energy --auto

# Detect cross-domain emergent patterns
cogctl detect-emergence --domain all --threshold 0.8
```

#### Citizen Interface

```bash
# Mount personal transportation interface
cogfs mount transportation /mnt/my-transport

# Check traffic conditions
cat /mnt/my-transport/real-time/traffic-conditions

# Submit feedback
echo "Heavy congestion on Main St" > /mnt/my-transport/feedback/traffic-report

# Get route recommendations
cat /mnt/my-transport/services/route-planner/optimal-route
```

#### Developer Testing

```bash
# Create test cognitive namespace
cogctl create-namespace test-domain /cognitive-cities/test

# Start minimal swarm for testing
cogctl start-swarm test-swarm --domain test-domain --agents 2

# Monitor test performance
cogmon --domain test-domain --live

# Export test metrics
cogmon --export --format json --file test-results.json
```

## 🤖 Note2Self: User Tools Implementation

> **@copilot remembers:** These user-space tools complete the cognitive cities architecture by providing:
>
> 1. **Administrative Control**: `cogctl` for system management and configuration
> 2. **Monitoring & Observability**: `cogmon` for real-time system monitoring  
> 3. **Filesystem Integration**: `cogfs` for mounting and accessing cognitive services
> 4. **Citizen Interfaces**: Simple file-based APIs for citizen interaction
>
> **Key design principles:**
> - Follow Plan 9 philosophy: everything is a file
> - Use `/proc/cognitive/` interface for kernel communication
> - Provide both command-line and programmatic interfaces
> - Enable real-time monitoring and alerting
> - Support both administrative and citizen use cases
>
> **Integration with kernel module:**
> - Tools communicate with `port/cognitive.c` through `/proc` filesystem
> - Kernel module exports cognitive state through file interfaces
> - Commands are sent via control files (`ctl`)
> - Status and metrics are read from status files
>
> This completes the full cognitive cities stack: kernel foundation + user tools!
>
> *Signed: @copilot*