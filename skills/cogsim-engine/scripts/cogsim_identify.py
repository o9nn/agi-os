#!/usr/bin/env python3
"""Identify the CogSim composition pattern and paradigm for a domain.

Combines semiring identification with simulation paradigm selection
and endocrine integration recommendations.

Usage:
    python cogsim_identify.py "<domain description>"

Examples:
    python cogsim_identify.py "hospital emergency department with stressed staff"
    python cogsim_identify.py "social network with emotional contagion"
    python cogsim_identify.py "supply chain with feedback loops and worker fatigue"
"""

import sys

# ============================================================================
# Semiring catalog (from circled-operators, extended for CogSim)
# ============================================================================

SEMIRINGS = [
    {
        "name": "Cognitive Simulation",
        "keywords": ["cognitive", "simulation", "cogsim", "affective", "emotional", "mood",
                      "stress", "fatigue", "motivation", "endocrine", "hormone"],
        "set": "CogSim Models",
        "oplus": "⊕ (alternative models / parallel branches)",
        "otimes": "⊗ (composed models / sequential pipeline)",
        "zero": "∅ (empty model)",
        "one": "Id (identity pass-through)",
    },
    {
        "name": "Process Flow (DES)",
        "keywords": ["queue", "process", "service", "throughput", "waiting", "arrival",
                      "resource", "entity", "flowchart", "bottleneck", "utilization"],
        "set": "Process Blocks",
        "oplus": "⊕ (alternative routes / parallel paths)",
        "otimes": "⊗ (sequential process steps)",
        "zero": "∅ (no process)",
        "one": "PassThrough (identity block)",
    },
    {
        "name": "Agent Population (ABM)",
        "keywords": ["agent", "individual", "behavior", "interaction", "emergent",
                      "population", "social", "autonomous", "state", "rule"],
        "set": "Agent Types",
        "oplus": "⊕ (agent type alternatives)",
        "otimes": "⊗ (agent interaction / composition)",
        "zero": "∅ (no agents)",
        "one": "PassiveAgent (no behavior)",
    },
    {
        "name": "System Dynamics (SD)",
        "keywords": ["feedback", "loop", "stock", "flow", "accumulation", "rate",
                      "aggregate", "system", "dynamics", "equilibrium", "growth"],
        "set": "Stock-Flow Diagrams",
        "oplus": "⊕ (independent subsystems)",
        "otimes": "⊗ (coupled feedback loops)",
        "zero": "∅ (no stocks)",
        "one": "ConstantStock (equilibrium)",
    },
    {
        "name": "Endocrine Dynamics",
        "keywords": ["hormone", "cortisol", "dopamine", "serotonin", "oxytocin",
                      "stress", "reward", "arousal", "valence", "mood", "affect",
                      "endocrine", "gland", "neuro"],
        "set": "Hormone Channels",
        "oplus": "⊕ (independent channels / mood alternatives)",
        "otimes": "⊗ (hormone cascades / coupled dynamics)",
        "zero": "∅ (no hormones)",
        "one": "Baseline (homeostatic setpoint)",
    },
]

# ============================================================================
# Paradigm selection
# ============================================================================

PARADIGM_INDICATORS = {
    "DES": {
        "keywords": ["queue", "process", "service", "throughput", "waiting", "arrival",
                      "resource", "entity", "flowchart", "bottleneck", "delay", "routing"],
        "description": "Discrete-Event Simulation: entities flow through process blocks",
        "endo_pattern": "Hormones modulate service times, routing decisions, queue priority",
    },
    "ABM": {
        "keywords": ["agent", "individual", "behavior", "interaction", "emergent",
                      "population", "social", "autonomous", "rule", "contagion", "spread"],
        "description": "Agent-Based Modeling: autonomous agents with individual behavior",
        "endo_pattern": "Each agent owns an EndocrineSystem; social interactions trigger cascades",
    },
    "SD": {
        "keywords": ["feedback", "loop", "stock", "flow", "accumulation", "rate",
                      "aggregate", "system", "dynamics", "equilibrium", "growth", "decay"],
        "description": "System Dynamics: aggregate behavior with feedback loops",
        "endo_pattern": "Hormone concentrations modeled as stock variables with flow equations",
    },
}

# ============================================================================
# Endocrine integration recommendations
# ============================================================================

ENDO_TRIGGERS = {
    "stress": ("THREAT_DETECTED", "Cortisol, CRH, ACTH cascade"),
    "fatigue": ("RESOURCE_DEPLETED", "Insulin/glucagon energy balance"),
    "reward": ("REWARD_RECEIVED", "Phasic dopamine burst"),
    "social": ("SOCIAL_BOND_SIGNAL", "Oxytocin release"),
    "novel": ("NOVELTY_ENCOUNTERED", "Norepinephrine arousal"),
    "error": ("ERROR_DETECTED", "IL-6 immune response"),
    "conflict": ("CONFLICT_DETECTED", "Moderate HPA + norepinephrine"),
    "uncertain": ("UNCERTAINTY_HIGH", "Mild HPA + norepinephrine"),
    "noise": ("NOISE_EXCESSIVE", "Anandamide dampening"),
    "motivation": ("REWARD_RECEIVED", "Tonic dopamine elevation"),
    "trust": ("SOCIAL_BOND_SIGNAL", "Oxytocin bonding"),
    "anxiety": ("THREAT_DETECTED", "CRH-driven stress cascade"),
    "burnout": ("RESOURCE_DEPLETED", "Chronic cortisol + low dopamine"),
    "cooperation": ("SOCIAL_BOND_SIGNAL", "Oxytocin + serotonin"),
    "panic": ("THREAT_DETECTED", "Full HPA activation"),
}


def score_match(domain: str, entry: dict) -> int:
    domain_lower = domain.lower()
    return sum(1 for kw in entry["keywords"] if kw.lower() in domain_lower)


def identify_paradigm(domain: str) -> list:
    domain_lower = domain.lower()
    results = []
    for name, info in PARADIGM_INDICATORS.items():
        score = sum(1 for kw in info["keywords"] if kw in domain_lower)
        if score > 0:
            results.append((score, name, info))
    results.sort(key=lambda x: x[0], reverse=True)
    return results


def identify_endo_triggers(domain: str) -> list:
    domain_lower = domain.lower()
    triggers = []
    for keyword, (event, description) in ENDO_TRIGGERS.items():
        if keyword in domain_lower:
            triggers.append((keyword, event, description))
    return triggers


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    domain = " ".join(sys.argv[1:])
    print(f"{'═' * 70}")
    print(f"  CogSim Engine — Domain Analysis")
    print(f"  Domain: '{domain}'")
    print(f"{'═' * 70}\n")

    # --- Semiring identification ---
    scored = [(score_match(domain, s), s) for s in SEMIRINGS]
    scored.sort(key=lambda x: x[0], reverse=True)
    matches = [(sc, s) for sc, s in scored if sc > 0]

    if matches:
        print("▸ SEMIRING IDENTIFICATION\n")
        for i, (score, s) in enumerate(matches[:3], 1):
            print(f"  #{i} {s['name']}  (confidence: {'█' * score}{'░' * (5 - score)})")
            print(f"      Set:  {s['set']}")
            print(f"      ⊕:    {s['oplus']}")
            print(f"      ⊗:    {s['otimes']}")
            print(f"      0:    {s['zero']}")
            print(f"      1:    {s['one']}")
            print()

    # --- Paradigm selection ---
    paradigms = identify_paradigm(domain)
    print("▸ PARADIGM RECOMMENDATION\n")
    if not paradigms:
        print("  No strong paradigm match. Consider multimethod modeling.")
    elif len(paradigms) > 1:
        print(f"  Recommended: MULTIMETHOD ({' ⊗ '.join(name for _, name, _ in paradigms)})")
        for score, name, info in paradigms:
            print(f"    • {name}: {info['description']}")
            print(f"      Endocrine: {info['endo_pattern']}")
    else:
        _, name, info = paradigms[0]
        print(f"  Recommended: {name}")
        print(f"  {info['description']}")
        print(f"  Endocrine: {info['endo_pattern']}")
    print()

    # --- Endocrine triggers ---
    triggers = identify_endo_triggers(domain)
    if triggers:
        print("▸ ENDOCRINE EVENT MAPPING\n")
        for keyword, event, desc in triggers:
            print(f"  '{keyword}' → {event}")
            print(f"    {desc}")
        print()

    # --- Composition suggestion ---
    print("▸ SUGGESTED COMPOSITION\n")
    if len(paradigms) > 1:
        parts = [name for _, name, _ in paradigms]
        expr = " ⊗ ".join(parts)
        print(f"  model = Endo ⊗ ({expr})")
        print(f"  # Endocrine layer multiplicatively composed with multimethod model")
    elif paradigms:
        _, name, _ = paradigms[0]
        print(f"  model = Endo ⊗ {name}")
        print(f"  # Endocrine layer multiplicatively composed with {name} model")
    else:
        print("  model = Endo ⊗ (DES ⊕ ABM ⊕ SD)")
        print("  # Choose paradigm based on further analysis")
    print()


if __name__ == "__main__":
    main()
