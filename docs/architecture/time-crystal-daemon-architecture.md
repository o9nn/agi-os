# Time Crystal Daemon Architecture

This document outlines the architecture for the `time-crystal-daemon` skill, which integrates a deterministic cognitive kernel with a natural language interface powered by a Large Language Model (LLM). The design is based on the principles of the `opencog-inferno-kernel` and `time-crystal-neuron` skills, and the LLM interface pattern provided by the user.

## 1. Core Principles

- **Deterministic Core, Non-Deterministic Interface:** The core cognitive functions are implemented in a deterministic, long-running daemon. The LLM provides a natural language interface but does not have direct control over the core logic.
- **Separation of Concerns:** The daemon and the LLM interface are separate processes (sidecar pattern) to ensure stability and independent evolution.
- **Typed Command Surface:** All interactions between the LLM interface and the daemon are mediated by a well-defined Interface Definition Language (IDL), ensuring safety and control.
- **Cognition as an OS Service:** The daemon exposes cognitive capabilities as a service that can be interrogated and controlled through the LLM interface.

## 2. System Components

The system consists of three main components:

1.  **Time Crystal Daemon (Core):** The deterministic cognitive engine.
2.  **LLM Interface (Sidecar):** The natural language-to-IDL translator.
3.  **Interface Definition Language (IDL):** The typed command set for daemon interaction.

### 2.1. Time Crystal Daemon (Core)

The daemon is a long-running process built on the `opencog-inferno-kernel` template. It will be responsible for:

- **Implementing the Time Crystal Model:** The daemon will use the `time-crystal-neuron` model to create a hierarchical temporal structure for its internal cognitive processes.
- **Managing Cognitive Resources:** It will manage the AtomSpace, attention allocation, and other cognitive resources as defined in `cognitive_kernel.h`.
- **Executing Deterministic Commands:** The daemon will expose a set of deterministic commands through the IDL for introspection and control.
- **Emitting Structured Events:** The daemon will emit structured events (e.g., JSON, S-expressions) to report its internal state, which the LLM can then narrate.

### 2.2. LLM Interface (Sidecar)

The LLM interface is a separate service that acts as a bridge between the user and the daemon. Its responsibilities include:

- **Natural Language Understanding:** It will take natural language queries from the user.
- **Command Compilation:** It will compile the user's intent into a sequence of IDL commands.
- **Plan Generation and Review:** For complex requests, it will generate a plan of IDL commands for the user to review and approve.
- **Narration of Daemon Output:** It will receive structured data from the daemon and translate it into human-readable explanations.
- **Access Control:** It will enforce different access levels (e.g., "technician mode" vs. "engineer mode") to control the user's ability to execute privileged commands.

### 2.3. Interface Definition Language (IDL)

The IDL is a critical component that defines the boundary between the LLM and the daemon. It will be a typed, structured language (likely based on JSON-RPC or a similar standard) that specifies:

- **Commands:** A set of allowed operations that can be performed on the daemon (e.g., `get_status`, `trace_atom`, `set_attention`).
- **Data Types:** The data types for the command parameters and return values (e.g., `AtomHandle`, `TruthValue`, `AttentionValue`).
- **Permissions:** The permissions required to execute each command.

## 3. Workflow

A typical user interaction will follow this workflow:

1.  **User Query:** The user sends a natural language query to the LLM interface (e.g., "Why is the `pln` module using so much attention?").
2.  **Command Compilation:** The LLM interface compiles the query into a series of IDL commands (e.g., `get_module_by_name("pln")`, `get_attention_usage(moduleId)`).
3.  **Plan Review (Optional):** For commands that modify the daemon's state, the LLM interface will present a plan to the user for approval.
4.  **Daemon Execution:** The LLM interface sends the IDL commands to the daemon, which executes them deterministically.
5.  **Structured Output:** The daemon returns the results as structured data (e.g., a JSON object with attention values).
6.  **Narration:** The LLM interface narrates the structured output back to the user in natural language (e.g., "The `pln` module is using 80% of the available attention. This is likely due to a complex inference chain that was recently initiated.").

## 4. Skill Packaging

The entire system will be packaged as a Manus skill named `time-crystal-daemon`. The skill directory will contain:

- **`SKILL.md`:** The main skill file, which will explain how to use the daemon and its LLM interface.
- **`scripts/`:** Scripts to build, run, and interact with the daemon and the LLM interface.
- **`templates/`:** The source code for the daemon and the LLM interface, based on the `opencog-inferno-kernel` template.
- **`references/`:** The IDL specification and other relevant documentation.
