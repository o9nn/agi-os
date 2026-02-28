#!/usr/bin/env python3
"""
AGI-OS Engineer Assistant
========================
KoboldCpp-powered engineering assistant with deep knowledge of the
agi-os repository structure, build system, and three-layer architecture.

Usage:
    python3 config/assistants/engineer_assistant.py [--endpoint URL]

    # Or import as module:
    from config.assistants.engineer_assistant import EngineerAssistant
    eng = EngineerAssistant()
    eng.ask("How do I add a new CogServer module?")
"""

import sys
import os
import json
import argparse

# Add koboldcpp-cog to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__),
    '../../core/cognition/llm/koboldcpp-cog/python'))

from koboldcpp_cog import CognitiveInference, GenerationParams

# ============================================================================
# AGI-OS Knowledge Base
# ============================================================================

REPO_STRUCTURE = """
AGI-OS Repository Structure:
├── build-tools/mig/          # Unified MIG build entry point
├── core/
│   ├── microkernel/cognumach/ # Layer 1: CogNUMach microkernel (autotools)
│   │   └── mig/              # Mach Interface Generator
│   ├── os/hurdcog/           # Layer 2: HurdCog cognitive OS
│   │   └── cogkernel/        # Cognitive kernel extensions
│   ├── cognition/
│   │   ├── foundation/       # cogutil, atomspace, cogserver, etc.
│   │   ├── reasoning/        # PLN, URE, unify, spacetime
│   │   ├── learning/         # miner, moses, learn
│   │   ├── storage/          # atomspace-rocks, -cog, -pgres
│   │   ├── llm/              # koboldcpp-cog, aphroditecho
│   │   └── language/         # link-grammar, relex, lg-atomese
│   ├── integration/          # cognitive-grip, unified-cog-interface
│   └── inferno-kernel/       # Layer 0: Inferno Kernel + 9P
├── cogbolt/                  # Layer 4: AI-Powered IDE
├── config/
│   ├── cogserver/            # CogServer + Guile shell config
│   └── assistants/           # KoboldCpp assistant modules
├── infrastructure/packaging/ # Debian packaging (52 packages)
└── .github/workflows/        # CI/CD pipelines
"""

BUILD_DEPENDENCY_ORDER = """
Build Dependency Order (11 stages):
  Stage 0:  mig (Mach Interface Generator)
  Stage 1:  cogutil, ggml-tensor, inferno-kernel
  Stage 2:  cognumach (requires MIG)
  Stage 3:  atomspace (requires cogutil)
  Stage 3.5: atomspace-storage, cogserver, matrix, sensory
  Stage 4:  hurdcog, hurdcog-cogkernel-core, hurdcog-machspace
  Stage 5:  attention, pln, miner, unify, koboldcpp-cog, cognitive-grip
  Stage 6:  learn, generate, cogbolt, das, hyperon-metta
  Stage 7:  lg-atomese, relex, das-atomspace
  Stage 8:  atomspace-rocks, atomspace-cog, atomspace-pgres
  Stage 9:  deep-tree-echo, aphroditecho, agi-os-unified
"""

SYSTEM_PROMPT = f"""You are the AGI-OS Engineering Assistant. You have expert knowledge of:

1. The agi-os repository structure and all subsystems
2. The three-layer architecture: CogNUMach (microkernel), HurdCog (OS), OpenCog (cognition)
3. Build systems: CMake for OpenCog, autotools for CogNUMach/MIG, mixed for HurdCog
4. Debian packaging infrastructure (52 packages across 11 build stages)
5. The cognitive-grip integration layer that bridges all subsystems
6. KoboldCpp-Cog LLM inference bridge
7. CogServer Guile shell and module system

{REPO_STRUCTURE}

{BUILD_DEPENDENCY_ORDER}

Key technical details:
- MIG exists in 5 locations; use build-tools/mig/ as unified entry
- CogServer REQUIRES atomspace-storage
- HurdCog REQUIRES cognumach
- KoboldCpp-Cog REQUIRES cogutil + atomspace + ggml-tensor
- All Debian packages need: control, rules, changelog, compat, copyright, source/format
- Component paths use .github/scripts/component-paths.sh for workflow resolution

Provide precise, actionable engineering guidance with code examples when appropriate.
"""


class EngineerAssistant:
    """AGI-OS Engineering Assistant powered by KoboldCpp."""

    def __init__(self, endpoint: str = "http://localhost:5001"):
        self.engine = CognitiveInference(endpoint)
        self.params = GenerationParams(
            max_tokens=1024,
            temperature=0.3,
            top_p=0.9,
        )

    def ask(self, question: str) -> str:
        """Ask an engineering question about AGI-OS."""
        prompt = f"{SYSTEM_PROMPT}\n\nEngineering Question: {question}\n\nAnswer:"
        result = self.engine.client.generate(prompt, self.params)
        return result.text if result.success else f"Error: {result.error}"

    def diagnose_build(self, component: str, error: str) -> str:
        """Diagnose a build error for a specific component."""
        prompt = (
            f"{SYSTEM_PROMPT}\n\n"
            f"Build Error Diagnosis:\n"
            f"Component: {component}\n"
            f"Error: {error}\n\n"
            f"Diagnose the root cause and provide step-by-step fix:"
        )
        result = self.engine.client.generate(prompt, self.params)
        return result.text if result.success else f"Error: {result.error}"

    def suggest_improvement(self, area: str) -> str:
        """Suggest improvements for a specific area of AGI-OS."""
        prompt = (
            f"{SYSTEM_PROMPT}\n\n"
            f"Suggest concrete improvements for: {area}\n\n"
            f"Provide specific file changes, new components, or architectural changes:"
        )
        result = self.engine.client.generate(prompt, self.params)
        return result.text if result.success else f"Error: {result.error}"

    def explain_subsystem(self, subsystem: str) -> str:
        """Explain how a subsystem works and its integration points."""
        prompt = (
            f"{SYSTEM_PROMPT}\n\n"
            f"Explain the {subsystem} subsystem in detail:\n"
            f"- Architecture and key components\n"
            f"- Build dependencies and order\n"
            f"- Integration points with other subsystems\n"
            f"- Key files and directories\n"
        )
        result = self.engine.client.generate(prompt, self.params)
        return result.text if result.success else f"Error: {result.error}"


def main():
    parser = argparse.ArgumentParser(description="AGI-OS Engineer Assistant")
    parser.add_argument("--endpoint", default="http://localhost:5001",
                        help="KoboldCpp endpoint URL")
    parser.add_argument("--mode", choices=["ask", "diagnose", "improve", "explain"],
                        default="ask", help="Assistant mode")
    parser.add_argument("question", nargs="?", help="Question to ask")
    args = parser.parse_args()

    assistant = EngineerAssistant(args.endpoint)

    if args.question:
        if args.mode == "ask":
            print(assistant.ask(args.question))
        elif args.mode == "diagnose":
            parts = args.question.split(":", 1)
            print(assistant.diagnose_build(parts[0], parts[1] if len(parts) > 1 else ""))
        elif args.mode == "improve":
            print(assistant.suggest_improvement(args.question))
        elif args.mode == "explain":
            print(assistant.explain_subsystem(args.question))
    else:
        # Interactive mode
        print("AGI-OS Engineer Assistant (type 'quit' to exit)")
        print("=" * 50)
        while True:
            try:
                q = input("\nengineer> ").strip()
                if q.lower() in ("quit", "exit", "q"):
                    break
                if q:
                    print(assistant.ask(q))
            except (EOFError, KeyboardInterrupt):
                break


if __name__ == "__main__":
    main()
