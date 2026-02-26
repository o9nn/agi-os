#!/usr/bin/env python3
"""Analyze CogSim simulation output traces.

Reads hormone trace CSV files and produces analysis including:
- Hormone concentration time series plots
- Cognitive mode transition timeline
- Valence-arousal scatter plots
- Mode duration statistics
- Stress cascade detection

Usage:
    python cogsim_analyze.py <trace_file.csv> [--output-dir <dir>]

Trace CSV format:
    tick,crh,acth,cortisol,dopamine_tonic,dopamine_phasic,serotonin,
    norepinephrine,oxytocin,t3_t4,melatonin,insulin,glucagon,il6,
    anandamide,reserved1,reserved2,mode,valence,arousal

If no trace file exists, generates a synthetic demo trace for illustration.
"""

import argparse
import csv
import os
import sys
from pathlib import Path

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np

HORMONE_NAMES = [
    "CRH", "ACTH", "Cortisol", "Dopamine(tonic)", "Dopamine(phasic)",
    "Serotonin", "Norepinephrine", "Oxytocin", "T3/T4", "Melatonin",
    "Insulin", "Glucagon", "IL-6", "Anandamide", "Reserved1", "Reserved2"
]

MODE_NAMES = [
    "Resting", "Exploratory", "Focused", "Stressed", "Social",
    "Reflective", "Vigilant", "Maintenance", "Reward", "Threat"
]

MODE_COLORS = [
    "#808080", "#4CAF50", "#2196F3", "#F44336", "#E91E63",
    "#9C27B0", "#FF9800", "#607D8B", "#FFEB3B", "#B71C1C"
]

BASELINES = [0.05, 0.05, 0.15, 0.3, 0.0, 0.4, 0.1, 0.1, 0.5, 0.0,
             0.2, 0.1, 0.05, 0.1, 0.0, 0.0]
HALF_LIVES = [5, 10, 30, 20, 3, 50, 8, 15, 100, 12, 10, 8, 20, 6, 10, 10]


def generate_synthetic_trace(n_ticks=500):
    """Generate a synthetic trace demonstrating key CogSim dynamics."""
    np.random.seed(42)
    concentrations = np.zeros((n_ticks, 16))
    modes = np.zeros(n_ticks, dtype=int)
    valences = np.zeros(n_ticks)
    arousals = np.zeros(n_ticks)

    # Initialize at baselines
    conc = np.array(BASELINES, dtype=float)
    decay_factors = np.array([np.exp(-0.693147 / hl) for hl in HALF_LIVES])

    # Mode prototypes (simplified)
    prototypes = np.zeros((10, 16))
    prototypes[1, 3] = 0.6; prototypes[1, 5] = 0.4  # Exploratory
    prototypes[2, 6] = 0.6; prototypes[2, 2] = 0.3   # Focused
    prototypes[3, 2] = 0.7; prototypes[3, 6] = 0.6   # Stressed
    prototypes[4, 7] = 0.7; prototypes[4, 5] = 0.5   # Social
    prototypes[8, 4] = 0.8; prototypes[8, 3] = 0.5   # Reward
    prototypes[9, 2] = 0.9; prototypes[9, 6] = 0.8   # Threat

    for t in range(n_ticks):
        # Simulate events
        if 50 <= t <= 70:  # Stress event
            conc[0] += 0.15  # CRH
        if 55 <= t <= 80:  # Cascading ACTH
            conc[1] += 0.1
        if 60 <= t <= 120:  # Cortisol buildup
            conc[2] += 0.05
        if 150 <= t <= 170:  # Reward event
            conc[4] += 0.2  # Phasic dopamine
            conc[3] += 0.05  # Tonic dopamine
        if 250 <= t <= 280:  # Social interaction
            conc[7] += 0.1  # Oxytocin
            conc[5] += 0.03  # Serotonin
        if 350 <= t <= 380:  # Threat event
            conc[0] += 0.2
            conc[1] += 0.15
            conc[2] += 0.1
            conc[6] += 0.15

        # Add noise
        conc += np.random.normal(0, 0.005, 16)

        # Decay toward baselines
        baselines = np.array(BASELINES)
        conc = baselines + (conc - baselines) * decay_factors
        conc = np.clip(conc, 0, 1)

        concentrations[t] = conc

        # Mode classification (nearest centroid)
        dists = [np.linalg.norm(conc - prototypes[m]) for m in range(10)]
        modes[t] = np.argmin(dists)

        # Valence-arousal from hormone state
        valences[t] = (conc[3] + conc[4] + conc[7]) * 0.5 - (conc[2] + conc[0]) * 0.5
        arousals[t] = (conc[6] + conc[2] + conc[4]) * 0.5

    return concentrations, modes, valences, arousals


def load_trace(filepath):
    """Load trace from CSV file."""
    data = []
    with open(filepath, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            data.append(row)

    n = len(data)
    concentrations = np.zeros((n, 16))
    modes = np.zeros(n, dtype=int)
    valences = np.zeros(n)
    arousals = np.zeros(n)

    hormone_keys = ['crh', 'acth', 'cortisol', 'dopamine_tonic', 'dopamine_phasic',
                    'serotonin', 'norepinephrine', 'oxytocin', 't3_t4', 'melatonin',
                    'insulin', 'glucagon', 'il6', 'anandamide', 'reserved1', 'reserved2']

    for i, row in enumerate(data):
        for j, key in enumerate(hormone_keys):
            concentrations[i, j] = float(row.get(key, 0))
        modes[i] = int(row.get('mode', 0))
        valences[i] = float(row.get('valence', 0))
        arousals[i] = float(row.get('arousal', 0))

    return concentrations, modes, valences, arousals


def plot_hormone_traces(concentrations, output_dir):
    """Plot hormone concentration time series."""
    fig, axes = plt.subplots(4, 4, figsize=(20, 16), sharex=True)
    fig.suptitle('CogSim Hormone Traces', fontsize=16, fontweight='bold')

    ticks = np.arange(len(concentrations))

    for i in range(16):
        ax = axes[i // 4, i % 4]
        ax.plot(ticks, concentrations[:, i], linewidth=1.0, color='#2196F3')
        ax.axhline(y=BASELINES[i], color='#F44336', linestyle='--', alpha=0.5, linewidth=0.8)
        ax.set_title(HORMONE_NAMES[i], fontsize=10)
        ax.set_ylim(-0.05, 1.05)
        ax.grid(True, alpha=0.3)
        if i >= 12:
            ax.set_xlabel('Tick')

    plt.tight_layout()
    path = os.path.join(output_dir, 'hormone_traces.png')
    plt.savefig(path, dpi=150, bbox_inches='tight')
    plt.close()
    print(f"  Saved: {path}")


def plot_mode_timeline(modes, output_dir):
    """Plot cognitive mode transition timeline."""
    fig, ax = plt.subplots(figsize=(16, 4))
    fig.suptitle('CogSim Cognitive Mode Timeline', fontsize=14, fontweight='bold')

    ticks = np.arange(len(modes))
    for t in range(len(modes)):
        ax.axvspan(t, t + 1, color=MODE_COLORS[modes[t]], alpha=0.7)

    # Legend
    from matplotlib.patches import Patch
    patches = [Patch(color=MODE_COLORS[i], label=MODE_NAMES[i]) for i in range(10)]
    ax.legend(handles=patches, loc='upper right', ncol=5, fontsize=8)

    ax.set_xlim(0, len(modes))
    ax.set_xlabel('Tick')
    ax.set_ylabel('Mode')
    ax.set_yticks([])

    plt.tight_layout()
    path = os.path.join(output_dir, 'mode_timeline.png')
    plt.savefig(path, dpi=150, bbox_inches='tight')
    plt.close()
    print(f"  Saved: {path}")


def plot_valence_arousal(valences, arousals, output_dir):
    """Plot valence-arousal scatter (Russell's circumplex)."""
    fig, ax = plt.subplots(figsize=(8, 8))
    fig.suptitle('CogSim Valence-Arousal Space', fontsize=14, fontweight='bold')

    scatter = ax.scatter(valences, arousals, c=np.arange(len(valences)),
                         cmap='viridis', alpha=0.6, s=10)
    plt.colorbar(scatter, label='Tick')

    ax.axhline(y=0, color='gray', linestyle='-', alpha=0.3)
    ax.axvline(x=0, color='gray', linestyle='-', alpha=0.3)
    ax.set_xlabel('Valence (negative ← → positive)')
    ax.set_ylabel('Arousal (calm ← → activated)')
    ax.set_xlim(-1, 1)
    ax.set_ylim(-0.1, 1)

    # Quadrant labels
    ax.text(0.5, 0.9, 'Excited/Happy', ha='center', fontsize=9, alpha=0.5)
    ax.text(-0.5, 0.9, 'Angry/Fearful', ha='center', fontsize=9, alpha=0.5)
    ax.text(0.5, 0.05, 'Calm/Content', ha='center', fontsize=9, alpha=0.5)
    ax.text(-0.5, 0.05, 'Sad/Bored', ha='center', fontsize=9, alpha=0.5)

    plt.tight_layout()
    path = os.path.join(output_dir, 'valence_arousal.png')
    plt.savefig(path, dpi=150, bbox_inches='tight')
    plt.close()
    print(f"  Saved: {path}")


def compute_statistics(concentrations, modes, valences, arousals):
    """Compute and print summary statistics."""
    print("\n▸ SUMMARY STATISTICS\n")

    # Mode durations
    print("  Mode Distribution:")
    total = len(modes)
    for i in range(10):
        count = np.sum(modes == i)
        pct = count / total * 100
        bar = '█' * int(pct / 2)
        print(f"    {MODE_NAMES[i]:15s} {pct:5.1f}% {bar}")

    # Hormone statistics
    print("\n  Hormone Statistics (mean ± std):")
    for i in range(14):  # Skip reserved
        mean = np.mean(concentrations[:, i])
        std = np.std(concentrations[:, i])
        mx = np.max(concentrations[:, i])
        print(f"    {HORMONE_NAMES[i]:20s}  μ={mean:.3f}  σ={std:.3f}  max={mx:.3f}")

    # Stress cascades
    print("\n  Stress Cascade Detection:")
    cortisol = concentrations[:, 2]
    threshold = 0.5
    in_cascade = False
    cascades = []
    start = 0
    for t in range(len(cortisol)):
        if cortisol[t] > threshold and not in_cascade:
            in_cascade = True
            start = t
        elif cortisol[t] <= threshold and in_cascade:
            in_cascade = False
            cascades.append((start, t, t - start))
    if cascades:
        for s, e, d in cascades:
            print(f"    Cascade: ticks {s}-{e} (duration: {d} ticks)")
    else:
        print("    No stress cascades detected (cortisol never exceeded 0.5)")

    # Valence summary
    print(f"\n  Valence: mean={np.mean(valences):.3f}, range=[{np.min(valences):.3f}, {np.max(valences):.3f}]")
    print(f"  Arousal: mean={np.mean(arousals):.3f}, range=[{np.min(arousals):.3f}, {np.max(arousals):.3f}]")


def main():
    parser = argparse.ArgumentParser(description='Analyze CogSim simulation traces')
    parser.add_argument('trace_file', nargs='?', help='Path to trace CSV file')
    parser.add_argument('--output-dir', default='.', help='Output directory for plots')
    parser.add_argument('--demo', action='store_true', help='Generate and analyze synthetic demo trace')
    args = parser.parse_args()

    output_dir = args.output_dir
    os.makedirs(output_dir, exist_ok=True)

    if args.trace_file and os.path.exists(args.trace_file):
        print(f"Loading trace: {args.trace_file}")
        concentrations, modes, valences, arousals = load_trace(args.trace_file)
    else:
        if args.trace_file:
            print(f"Trace file not found: {args.trace_file}")
        print("Generating synthetic demo trace (500 ticks)...\n")
        concentrations, modes, valences, arousals = generate_synthetic_trace()

    print("Generating analysis plots...")
    plot_hormone_traces(concentrations, output_dir)
    plot_mode_timeline(modes, output_dir)
    plot_valence_arousal(valences, arousals, output_dir)
    compute_statistics(concentrations, modes, valences, arousals)
    print("\nAnalysis complete.")


if __name__ == "__main__":
    main()
