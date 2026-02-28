#!/usr/bin/env python3
"""
AGI-OS Management Assistant
============================
KoboldCpp-powered management assistant with optimal grip on all AGI-OS
subsystems. Handles monitoring, configuration, orchestration, health
checks, and operational guidance.

Usage:
    python3 config/assistants/mgmt_assistant.py [--endpoint URL]

    # Or import:
    from config.assistants.mgmt_assistant import MgmtAssistant
    mgmt = MgmtAssistant()
    mgmt.status()
    mgmt.health_check()
"""

import sys
import os
import json
import subprocess
import argparse
from dataclasses import dataclass, field
from typing import Dict, List, Optional
from pathlib import Path

sys.path.insert(0, os.path.join(os.path.dirname(__file__),
    '../../core/cognition/llm/koboldcpp-cog/python'))

from koboldcpp_cog import CognitiveInference, GenerationParams

# ============================================================================
# Subsystem Registry
# ============================================================================

@dataclass
class Subsystem:
    """Represents an AGI-OS subsystem."""
    name: str
    layer: str
    path: str
    build_system: str
    dependencies: List[str]
    status: str = "unknown"
    health: str = "unchecked"
    description: str = ""

SUBSYSTEMS = {
    "mig": Subsystem(
        name="MIG", layer="0", path="core/microkernel/cognumach/mig",
        build_system="autotools", dependencies=[],
        description="Mach Interface Generator - build tool for microkernel interfaces"
    ),
    "inferno-kernel": Subsystem(
        name="Inferno Kernel", layer="0", path="core/inferno-kernel",
        build_system="cmake", dependencies=[],
        description="Inferno OS kernel with 9P protocol support"
    ),
    "cognumach": Subsystem(
        name="CogNUMach", layer="1", path="core/microkernel/cognumach",
        build_system="autotools", dependencies=["mig"],
        description="Cognitive GNU Mach microkernel with AtomSpace-aware IPC"
    ),
    "hurdcog": Subsystem(
        name="HurdCog", layer="2", path="core/os/hurdcog",
        build_system="autotools+cmake", dependencies=["cognumach", "cogutil", "atomspace"],
        description="Cognitive GNU Hurd OS with MachSpace and cognitive kernel"
    ),
    "cogutil": Subsystem(
        name="CogUtil", layer="3", path="core/cognition/foundation/cogutil",
        build_system="cmake", dependencies=[],
        description="OpenCog foundation utilities and configuration"
    ),
    "atomspace": Subsystem(
        name="AtomSpace", layer="3", path="core/cognition/foundation/atomspace",
        build_system="cmake", dependencies=["cogutil"],
        description="Hypergraph knowledge representation engine"
    ),
    "atomspace-storage": Subsystem(
        name="AtomSpace Storage", layer="3", path="core/cognition/foundation/atomspace-storage",
        build_system="cmake", dependencies=["atomspace"],
        description="Storage API for AtomSpace persistence"
    ),
    "cogserver": Subsystem(
        name="CogServer", layer="3", path="core/cognition/foundation/cogserver",
        build_system="cmake", dependencies=["atomspace-storage"],
        description="Network shell and module loader with Guile REPL"
    ),
    "ure": Subsystem(
        name="URE", layer="3", path="core/cognition/reasoning/ure",
        build_system="cmake", dependencies=["atomspace"],
        description="Unified Rule Engine for forward/backward chaining"
    ),
    "pln": Subsystem(
        name="PLN", layer="3", path="core/cognition/reasoning/pln",
        build_system="cmake", dependencies=["ure"],
        description="Probabilistic Logic Networks for uncertain reasoning"
    ),
    "attention": Subsystem(
        name="Attention (ECAN)", layer="3", path="core/cognition/foundation/attention",
        build_system="cmake", dependencies=["cogserver"],
        description="Economic Attention Networks for resource allocation"
    ),
    "koboldcpp-cog": Subsystem(
        name="KoboldCpp-Cog", layer="3.7", path="core/cognition/llm/koboldcpp-cog",
        build_system="cmake", dependencies=["cogutil", "atomspace"],
        description="Cognitive LLM inference bridge via KoboldCpp"
    ),
    "cognitive-grip": Subsystem(
        name="Cognitive Grip", layer="integration",
        path="core/integration/cognitive-grip",
        build_system="cmake", dependencies=["cogutil", "atomspace"],
        description="Unified abstraction layer bridging all subsystems"
    ),
    "cogbolt": Subsystem(
        name="CogBolt", layer="4", path="cogbolt",
        build_system="cmake", dependencies=["cogutil", "atomspace"],
        description="AI-Powered IDE for cognitive development"
    ),
}


class MgmtAssistant:
    """AGI-OS Management Assistant with optimal subsystem grip."""

    def __init__(self, endpoint: str = "http://localhost:5001",
                 repo_root: Optional[str] = None):
        self.engine = CognitiveInference(endpoint)
        self.params = GenerationParams(max_tokens=1024, temperature=0.3)
        self.repo_root = repo_root or self._find_repo_root()
        self.subsystems = SUBSYSTEMS.copy()

    def _find_repo_root(self) -> str:
        """Find the agi-os repository root."""
        # Walk up from this file's location
        current = Path(__file__).resolve().parent
        for _ in range(5):
            if (current / "CMakeLists.txt").exists() and (current / "core").exists():
                return str(current)
            current = current.parent
        return os.getcwd()

    # ========================================================================
    # Status & Health
    # ========================================================================

    def status(self) -> Dict:
        """Get comprehensive status of all subsystems."""
        report = {"subsystems": {}, "summary": {}}
        total = len(self.subsystems)
        present = 0
        buildable = 0

        for key, sub in self.subsystems.items():
            full_path = os.path.join(self.repo_root, sub.path)
            exists = os.path.isdir(full_path)
            has_build = False

            if exists:
                present += 1
                if sub.build_system == "cmake":
                    has_build = os.path.isfile(os.path.join(full_path, "CMakeLists.txt"))
                elif sub.build_system == "autotools":
                    has_build = os.path.isfile(os.path.join(full_path, "configure.ac"))
                elif sub.build_system == "autotools+cmake":
                    has_build = (os.path.isfile(os.path.join(full_path, "configure.ac")) or
                                 os.path.isfile(os.path.join(full_path, "CMakeLists.txt")))
                if has_build:
                    buildable += 1

            sub.status = "present" if exists else "missing"
            sub.health = "buildable" if has_build else ("no-build-file" if exists else "missing")

            report["subsystems"][key] = {
                "name": sub.name,
                "layer": sub.layer,
                "path": sub.path,
                "status": sub.status,
                "health": sub.health,
                "build_system": sub.build_system,
                "dependencies": sub.dependencies,
            }

        report["summary"] = {
            "total": total,
            "present": present,
            "buildable": buildable,
            "missing": total - present,
        }
        return report

    def health_check(self) -> str:
        """Run a comprehensive health check and return human-readable report."""
        status = self.status()
        lines = []
        lines.append("╔══════════════════════════════════════════════════════════╗")
        lines.append("║           AGI-OS Management Health Report               ║")
        lines.append("╠══════════════════════════════════════════════════════════╣")

        s = status["summary"]
        lines.append(f"║  Subsystems: {s['present']}/{s['total']} present, "
                      f"{s['buildable']} buildable, {s['missing']} missing")
        lines.append("╠══════════════════════════════════════════════════════════╣")
        lines.append("║  Layer │ Subsystem          │ Status    │ Health        ║")
        lines.append("╠═══════╪════════════════════╪═══════════╪═══════════════╣")

        for key, info in status["subsystems"].items():
            layer = info["layer"].ljust(5)
            name = info["name"][:18].ljust(18)
            st = info["status"][:9].ljust(9)
            health = info["health"][:13].ljust(13)
            lines.append(f"║  {layer} │ {name} │ {st} │ {health} ║")

        lines.append("╚══════════════════════════════════════════════════════════╝")

        # Check packaging
        pkg_path = os.path.join(self.repo_root, "infrastructure/packaging/debian")
        if os.path.isdir(pkg_path):
            pkg_count = len([d for d in os.listdir(pkg_path)
                            if os.path.isdir(os.path.join(pkg_path, d, "debian"))])
            lines.append(f"\nDebian Packages: {pkg_count} configured")

        # Check workflows
        wf_path = os.path.join(self.repo_root, ".github/workflows")
        if os.path.isdir(wf_path):
            wf_count = len([f for f in os.listdir(wf_path) if f.endswith('.yml')])
            lines.append(f"GitHub Workflows: {wf_count} configured")

        # Check CogServer config
        cs_path = os.path.join(self.repo_root, "config/cogserver/cogserver.conf")
        lines.append(f"CogServer Config: {'present' if os.path.isfile(cs_path) else 'missing'}")

        return "\n".join(lines)

    # ========================================================================
    # Dependency Analysis
    # ========================================================================

    def dependency_order(self) -> List[List[str]]:
        """Compute build order respecting dependencies."""
        resolved = set()
        stages = []

        remaining = set(self.subsystems.keys())
        while remaining:
            stage = []
            for key in list(remaining):
                deps = set(self.subsystems[key].dependencies)
                if deps.issubset(resolved):
                    stage.append(key)
            if not stage:
                # Circular dependency or missing deps - add remaining
                stage = list(remaining)
            for key in stage:
                remaining.discard(key)
                resolved.add(key)
            stages.append(stage)

        return stages

    def print_build_order(self) -> str:
        """Print the build order as a human-readable plan."""
        stages = self.dependency_order()
        lines = ["Build Order:"]
        for i, stage in enumerate(stages):
            names = [self.subsystems[k].name for k in stage]
            lines.append(f"  Stage {i}: {', '.join(names)}")
        return "\n".join(lines)

    # ========================================================================
    # Packaging Validation
    # ========================================================================

    def validate_packaging(self) -> Dict:
        """Validate Debian packaging for all components."""
        pkg_path = os.path.join(self.repo_root, "infrastructure/packaging/debian")
        results = {"valid": [], "invalid": [], "missing_files": {}}

        required_files = ["control", "rules", "changelog", "compat",
                          "copyright", "source/format"]

        if not os.path.isdir(pkg_path):
            return {"error": "Packaging directory not found"}

        for pkg_dir in sorted(os.listdir(pkg_path)):
            debian_dir = os.path.join(pkg_path, pkg_dir, "debian")
            if not os.path.isdir(debian_dir):
                continue

            missing = []
            for f in required_files:
                if not os.path.isfile(os.path.join(debian_dir, f)):
                    missing.append(f)

            if missing:
                results["invalid"].append(pkg_dir)
                results["missing_files"][pkg_dir] = missing
            else:
                results["valid"].append(pkg_dir)

        return results

    # ========================================================================
    # Workflow Analysis
    # ========================================================================

    def analyze_workflows(self) -> Dict:
        """Analyze GitHub Actions workflows for issues."""
        wf_path = os.path.join(self.repo_root, ".github/workflows")
        results = {"total": 0, "issues": []}

        if not os.path.isdir(wf_path):
            return {"error": "Workflows directory not found"}

        for wf_file in sorted(os.listdir(wf_path)):
            if not wf_file.endswith('.yml'):
                continue
            results["total"] += 1
            full_path = os.path.join(wf_path, wf_file)

            with open(full_path, 'r') as f:
                content = f.read()

            # Check for wrong paths
            wrong_paths = []
            for wrong in ["cd cogutil/", "cd atomspace/", "cd cogserver/",
                          "cd cognumach/mig", "cd hurdcog/"]:
                if wrong in content and "core/" not in content.split(wrong)[0][-50:]:
                    wrong_paths.append(wrong.strip())

            if wrong_paths:
                results["issues"].append({
                    "file": wf_file,
                    "type": "wrong_path",
                    "details": wrong_paths,
                })

        return results

    # ========================================================================
    # LLM-Powered Management
    # ========================================================================

    def ask(self, question: str) -> str:
        """Ask the management assistant a question."""
        status = self.status()
        context = json.dumps(status["summary"], indent=2)

        prompt = (
            f"You are the AGI-OS Management Assistant. Current system status:\n"
            f"{context}\n\n"
            f"Management question: {question}\n\n"
            f"Provide operational guidance:"
        )
        result = self.engine.client.generate(prompt, self.params)
        return result.text if result.success else f"Error: {result.error}"

    def recommend_next_action(self) -> str:
        """Recommend the next action to improve AGI-OS."""
        status = self.status()
        pkg = self.validate_packaging()

        context = (
            f"System status: {json.dumps(status['summary'])}\n"
            f"Packaging: {len(pkg.get('valid', []))} valid, "
            f"{len(pkg.get('invalid', []))} invalid\n"
        )

        prompt = (
            f"You are the AGI-OS Management Assistant.\n{context}\n"
            f"What is the single most impactful next action to improve AGI-OS? "
            f"Be specific about files to change and commands to run."
        )
        result = self.engine.client.generate(prompt, self.params)
        return result.text if result.success else self._offline_recommendation(status, pkg)

    def _offline_recommendation(self, status: Dict, pkg: Dict) -> str:
        """Provide recommendation without LLM."""
        missing = [k for k, v in status["subsystems"].items()
                   if v["status"] == "missing"]
        no_build = [k for k, v in status["subsystems"].items()
                    if v["health"] == "no-build-file"]
        invalid_pkg = pkg.get("invalid", [])

        if missing:
            return f"Priority: Add missing subsystems: {', '.join(missing)}"
        if no_build:
            return f"Priority: Add build files for: {', '.join(no_build)}"
        if invalid_pkg:
            return f"Priority: Fix Debian packaging for: {', '.join(invalid_pkg[:5])}"
        return "All subsystems present and buildable. Focus on integration testing."


def main():
    parser = argparse.ArgumentParser(description="AGI-OS Management Assistant")
    parser.add_argument("--endpoint", default="http://localhost:5001")
    parser.add_argument("--repo", default=None, help="Repository root path")
    parser.add_argument("command", nargs="?", default="status",
                        choices=["status", "health", "build-order", "packaging",
                                 "workflows", "recommend", "ask"])
    parser.add_argument("question", nargs="?", default=None)
    args = parser.parse_args()

    mgmt = MgmtAssistant(args.endpoint, args.repo)

    if args.command == "status":
        status = mgmt.status()
        print(json.dumps(status, indent=2))
    elif args.command == "health":
        print(mgmt.health_check())
    elif args.command == "build-order":
        print(mgmt.print_build_order())
    elif args.command == "packaging":
        pkg = mgmt.validate_packaging()
        print(json.dumps(pkg, indent=2))
    elif args.command == "workflows":
        wf = mgmt.analyze_workflows()
        print(json.dumps(wf, indent=2))
    elif args.command == "recommend":
        print(mgmt.recommend_next_action())
    elif args.command == "ask":
        if args.question:
            print(mgmt.ask(args.question))
        else:
            print("AGI-OS Management Assistant (type 'quit' to exit)")
            while True:
                try:
                    q = input("\nmgmt> ").strip()
                    if q.lower() in ("quit", "exit", "q"):
                        break
                    if q:
                        print(mgmt.ask(q))
                except (EOFError, KeyboardInterrupt):
                    break


if __name__ == "__main__":
    main()
