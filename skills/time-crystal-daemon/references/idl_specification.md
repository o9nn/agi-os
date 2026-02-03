# Time Crystal Daemon IDL Specification

This document defines the Interface Definition Language (IDL) for the Time Crystal Daemon. All interactions between the LLM interface and the daemon core MUST use this typed command surface.

## 1. Data Types

### 1.1. Core Types

| Type | Description | JSON Representation |
|------|-------------|---------------------|
| `AtomHandle` | Unique identifier for atoms | `integer` (uint64) |
| `TruthValue` | Uncertainty representation | `{"strength": float, "confidence": float}` |
| `AttentionValue` | Importance metrics | `{"sti": int16, "lti": int16, "vlti": int16}` |
| `TimeCrystalLevel` | Temporal hierarchy level (0-11) | `integer` |
| `OscillatorState` | Current phase of an oscillator | `{"level": int, "phase": float, "frequency": float}` |

### 1.2. Composite Types

```json
{
  "Atom": {
    "handle": "AtomHandle",
    "type": "string",
    "name": "string | null",
    "outgoing": ["AtomHandle"],
    "tv": "TruthValue",
    "av": "AttentionValue",
    "tc_level": "TimeCrystalLevel"
  },
  "Module": {
    "id": "string",
    "name": "string",
    "status": "running | paused | error",
    "attention_usage": "float",
    "atoms_owned": "integer",
    "tc_levels": ["TimeCrystalLevel"]
  },
  "DiagnosticResult": {
    "anomalies": ["Anomaly"],
    "metrics": {"string": "float"},
    "recommendations": ["string"]
  },
  "Anomaly": {
    "severity": "low | medium | high | critical",
    "module": "string",
    "description": "string",
    "tc_level": "TimeCrystalLevel"
  }
}
```

## 2. Commands

### 2.1. Read-Only Commands (Technician Mode)

These commands are safe for all users and do not modify daemon state.

#### `get_status`
Get the overall status of the daemon.

```json
{
  "method": "get_status",
  "params": {},
  "returns": {
    "status": "running | paused | error",
    "uptime_seconds": "integer",
    "atom_count": "integer",
    "total_attention": "integer",
    "active_modules": ["string"],
    "tc_state": {
      "current_level": "TimeCrystalLevel",
      "global_phase": "float"
    }
  }
}
```

#### `list_modules`
List all cognitive modules.

```json
{
  "method": "list_modules",
  "params": {},
  "returns": ["Module"]
}
```

#### `get_module`
Get details of a specific module.

```json
{
  "method": "get_module",
  "params": {
    "module_id": "string"
  },
  "returns": "Module"
}
```

#### `trace_atom`
Trace an atom's provenance and relationships.

```json
{
  "method": "trace_atom",
  "params": {
    "handle": "AtomHandle",
    "depth": "integer (default: 3)"
  },
  "returns": {
    "atom": "Atom",
    "incoming": ["Atom"],
    "outgoing": ["Atom"],
    "provenance": ["string"]
  }
}
```

#### `diagnose`
Run diagnostics and identify anomalies.

```json
{
  "method": "diagnose",
  "params": {
    "scope": "all | module | tc_level",
    "target": "string | null"
  },
  "returns": "DiagnosticResult"
}
```

#### `get_tc_hierarchy`
Get the current state of the time crystal hierarchy.

```json
{
  "method": "get_tc_hierarchy",
  "params": {},
  "returns": {
    "levels": [{
      "id": "TimeCrystalLevel",
      "name": "string",
      "period_ms": "float",
      "current_phase": "float",
      "atom_count": "integer"
    }]
  }
}
```

#### `explain_decision`
Explain why the daemon made a specific decision.

```json
{
  "method": "explain_decision",
  "params": {
    "decision_id": "string"
  },
  "returns": {
    "thought": "string",
    "constraints": ["string"],
    "alternatives": ["string"],
    "confidence": "float"
  }
}
```

### 2.2. Mutation Commands (Engineer Mode)

These commands modify daemon state and require elevated permissions.

#### `set_attention`
Set the attention value of an atom.

```json
{
  "method": "set_attention",
  "params": {
    "handle": "AtomHandle",
    "av": "AttentionValue"
  },
  "returns": {
    "success": "boolean",
    "previous_av": "AttentionValue"
  },
  "permission": "engineer"
}
```

#### `pause_module`
Pause a cognitive module.

```json
{
  "method": "pause_module",
  "params": {
    "module_id": "string"
  },
  "returns": {
    "success": "boolean"
  },
  "permission": "engineer"
}
```

#### `resume_module`
Resume a paused cognitive module.

```json
{
  "method": "resume_module",
  "params": {
    "module_id": "string"
  },
  "returns": {
    "success": "boolean"
  },
  "permission": "engineer"
}
```

#### `inject_atom`
Inject a new atom into the AtomSpace.

```json
{
  "method": "inject_atom",
  "params": {
    "type": "string",
    "name": "string | null",
    "outgoing": ["AtomHandle"],
    "tv": "TruthValue",
    "tc_level": "TimeCrystalLevel"
  },
  "returns": {
    "handle": "AtomHandle"
  },
  "permission": "engineer"
}
```

#### `set_tc_phase`
Manually set the phase of a time crystal level.

```json
{
  "method": "set_tc_phase",
  "params": {
    "level": "TimeCrystalLevel",
    "phase": "float (0.0 - 1.0)"
  },
  "returns": {
    "success": "boolean"
  },
  "permission": "engineer"
}
```

## 3. Events (Daemon → LLM)

The daemon emits these structured events for the LLM to narrate.

### `thought`
The daemon's internal reasoning.

```json
{
  "event": "thought",
  "data": {
    "module": "string",
    "content": "string",
    "tc_level": "TimeCrystalLevel"
  }
}
```

### `anomaly_detected`
An anomaly has been detected.

```json
{
  "event": "anomaly_detected",
  "data": "Anomaly"
}
```

### `decision_made`
A decision has been made.

```json
{
  "event": "decision_made",
  "data": {
    "decision_id": "string",
    "description": "string",
    "confidence": "float"
  }
}
```

### `tc_phase_transition`
A time crystal level has transitioned to a new phase.

```json
{
  "event": "tc_phase_transition",
  "data": {
    "level": "TimeCrystalLevel",
    "old_phase": "float",
    "new_phase": "float"
  }
}
```

## 4. Error Handling

All commands may return an error object:

```json
{
  "error": {
    "code": "integer",
    "message": "string",
    "details": "object | null"
  }
}
```

### Error Codes

| Code | Description |
|------|-------------|
| 1001 | Invalid parameter type |
| 1002 | Atom not found |
| 1003 | Module not found |
| 1004 | Permission denied |
| 1005 | Resource limit exceeded |
| 1006 | Invalid time crystal level |
| 2001 | Internal daemon error |
| 2002 | Timeout |

## 5. Transport

The IDL is transport-agnostic but the reference implementation uses:

- **Unix Domain Sockets** for local communication
- **JSON-RPC 2.0** for message framing
- **Event stream** (newline-delimited JSON) for daemon events
