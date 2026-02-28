#!/usr/bin/env python3
"""
AGI-OS Development Assistant
============================
KoboldCpp-powered development assistant for coding within the agi-os
ecosystem. Specializes in C++, Guile Scheme, and Python for OpenCog,
CogServer modules, PLN rules, and KoboldCpp integration.

Usage:
    python3 config/assistants/dev_assistant.py [--endpoint URL]

    # Or import:
    from config.assistants.dev_assistant import DevAssistant
    dev = DevAssistant()
    dev.write_code("Create a PLN rule for inheritance reasoning")
"""

import sys
import os
import argparse

sys.path.insert(0, os.path.join(os.path.dirname(__file__),
    '../../core/cognition/llm/koboldcpp-cog/python'))

from koboldcpp_cog import CognitiveInference, GenerationParams

SYSTEM_PROMPT = """You are the AGI-OS Development Assistant. You write production-quality code for:

1. **C++ OpenCog modules**: AtomSpace types, CogServer modules, cognitive-grip bridges
2. **Guile Scheme**: AtomSpace operations, PLN rules, URE configurations, CogServer shell scripts
3. **Python**: opencog.atomspace bindings, KoboldCpp-Cog integration, test scripts
4. **CMake**: Build system configuration, find modules, packaging
5. **Debian packaging**: control files, rules, changelogs

Code conventions:
- C++: Use opencog namespace, follow OpenCog coding style, include proper headers
- Scheme: Use (opencog) module system, define-public for exports
- Python: Type hints, docstrings, follow PEP 8
- CMake: Use target-based commands, proper find_package

Key APIs:
- AtomSpace: ConceptNode, PredicateNode, EvaluationLink, BindLink, GetLink
- CogServer: DECLARE_MODULE, Module::init(), Module::run()
- PLN: DefinedSchemaNode, BindLink with PLN rules
- KoboldCpp-Cog: CognitiveInference, KoboldCppClient, AtomSpaceContextBuilder

Always provide complete, compilable/runnable code with proper error handling.
"""


class DevAssistant:
    """AGI-OS Development Assistant powered by KoboldCpp."""

    def __init__(self, endpoint: str = "http://localhost:5001"):
        self.engine = CognitiveInference(endpoint)
        self.params = GenerationParams(
            max_tokens=2048,
            temperature=0.2,
            top_p=0.95,
        )

    def write_code(self, task: str, language: str = "auto") -> str:
        """Generate code for a development task."""
        prompt = (
            f"{SYSTEM_PROMPT}\n\n"
            f"Language: {language}\n"
            f"Task: {task}\n\n"
            f"Provide complete, production-ready code:\n"
        )
        result = self.engine.client.generate(prompt, self.params)
        return result.text if result.success else f"Error: {result.error}"

    def write_module(self, name: str, description: str) -> str:
        """Generate a complete CogServer module."""
        prompt = (
            f"{SYSTEM_PROMPT}\n\n"
            f"Create a complete CogServer module:\n"
            f"Module name: {name}\n"
            f"Description: {description}\n\n"
            f"Provide:\n"
            f"1. Header file (.h)\n"
            f"2. Implementation file (.cpp)\n"
            f"3. CMakeLists.txt\n"
            f"4. Scheme wrapper (.scm)\n"
        )
        result = self.engine.client.generate(prompt, self.params)
        return result.text if result.success else f"Error: {result.error}"

    def write_pln_rule(self, name: str, description: str) -> str:
        """Generate a PLN reasoning rule."""
        prompt = (
            f"{SYSTEM_PROMPT}\n\n"
            f"Create a PLN reasoning rule in Guile Scheme:\n"
            f"Rule name: {name}\n"
            f"Description: {description}\n\n"
            f"Provide the complete rule definition with:\n"
            f"1. BindLink pattern\n"
            f"2. Variable declarations\n"
            f"3. Pattern body\n"
            f"4. Rewrite term\n"
            f"5. URE configuration\n"
        )
        result = self.engine.client.generate(prompt, self.params)
        return result.text if result.success else f"Error: {result.error}"

    def write_test(self, component: str, test_type: str = "unit") -> str:
        """Generate tests for a component."""
        prompt = (
            f"{SYSTEM_PROMPT}\n\n"
            f"Write {test_type} tests for: {component}\n\n"
            f"Provide complete test file with setup, teardown, and assertions.\n"
        )
        result = self.engine.client.generate(prompt, self.params)
        return result.text if result.success else f"Error: {result.error}"

    def review_code(self, code: str) -> str:
        """Review code and suggest improvements."""
        prompt = (
            f"{SYSTEM_PROMPT}\n\n"
            f"Review this code and suggest improvements:\n"
            f"```\n{code}\n```\n\n"
            f"Provide:\n"
            f"1. Issues found\n"
            f"2. Suggested fixes\n"
            f"3. Improved version\n"
        )
        result = self.engine.client.generate(prompt, self.params)
        return result.text if result.success else f"Error: {result.error}"

    def fix_error(self, error_msg: str, code_context: str = "") -> str:
        """Diagnose and fix a code error."""
        prompt = (
            f"{SYSTEM_PROMPT}\n\n"
            f"Fix this error:\n"
            f"Error: {error_msg}\n"
        )
        if code_context:
            prompt += f"\nCode context:\n```\n{code_context}\n```\n"
        prompt += "\nProvide the fix with explanation:\n"
        result = self.engine.client.generate(prompt, self.params)
        return result.text if result.success else f"Error: {result.error}"


def main():
    parser = argparse.ArgumentParser(description="AGI-OS Dev Assistant")
    parser.add_argument("--endpoint", default="http://localhost:5001")
    parser.add_argument("--mode", choices=["code", "module", "pln", "test", "review", "fix"],
                        default="code")
    parser.add_argument("task", nargs="?")
    args = parser.parse_args()

    dev = DevAssistant(args.endpoint)

    if args.task:
        if args.mode == "code":
            print(dev.write_code(args.task))
        elif args.mode == "module":
            print(dev.write_module(args.task, "Auto-generated module"))
        elif args.mode == "pln":
            print(dev.write_pln_rule(args.task, "Auto-generated rule"))
        elif args.mode == "test":
            print(dev.write_test(args.task))
        elif args.mode == "fix":
            print(dev.fix_error(args.task))
    else:
        print("AGI-OS Dev Assistant (type 'quit' to exit)")
        print("=" * 50)
        while True:
            try:
                q = input("\ndev> ").strip()
                if q.lower() in ("quit", "exit", "q"):
                    break
                if q:
                    print(dev.write_code(q))
            except (EOFError, KeyboardInterrupt):
                break


if __name__ == "__main__":
    main()
