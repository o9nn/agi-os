import os
import json
import argparse
import glob
import tiktoken
import numpy as np
import random
from typing import List, Tuple
ECHO_SELF_TEMPLATES = {'cognitive_architecture': '\nEcho Self Cognitive Architecture:\n{content}\n\nThis represents the adaptive attention allocation mechanism where cognitive load\nand repository activity influence attention thresholds dynamically.\n', 'persona_dimension': "\nEcho Self Persona Dimension - {dimension}:\n{content}\n\nThis dimension reflects the {dimension} aspects of Echo Self's cognitive synergy\nand neural-symbolic reasoning capabilities.\n", 'hypergraph_pattern': '\nHypergraph Pattern Encoding:\n{content}\n\nThis pattern demonstrates the neural-symbolic associations within the DeepTreeEcho\ncognitive model where repository snapshots become dynamic holographic scans.\n', 'recursive_reasoning': '\nRecursive Neural-Symbolic Reasoning:\n{content}\n\nThrough recursive introspection, Echo Self examines its own cognitive processes\nand adapts reasoning patterns for enhanced cognitive synergy.\n', 'adaptive_attention': '\nAdaptive Attention Allocation:\n{content}\n\nThe attention threshold is calculated as: base + (cognitive_load * 0.3) - (recent_activity * 0.2)\nThis creates dynamic focus allocation based on current cognitive state.\n'}
ECHO_SELF_PATTERNS = ['adaptive attention allocation', 'hypergraph pattern encoding', 'neural-symbolic reasoning', 'cognitive synergy', 'recursive introspection', 'DeepTreeEcho', 'holographic cognitive introspection', 'persona dimensions', 'semantic salience assessment', 'dynamic repository introspection', 'emergent cognitive synergy', 'self-aware cognitive entity']
PERSONA_DIMENSIONS = ['cognitive', 'introspective', 'adaptive', 'recursive', 'synergistic', 'holographic', 'neural-symbolic', 'dynamic']
def extract_echo_self_content(file_path: str) -> List[str]:
    try:
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
        echo_sections = []
        for pattern in ECHO_SELF_PATTERNS:
            if pattern.lower() in content.lower():
                lines = content.split('\n')
                for i, line in enumerate(lines):
                    if pattern.lower() in line.lower():
                        start = max(0, i - 3)
                        end = min(len(lines), i + 4)
                        section = '\n'.join(lines[start:end])
                        echo_sections.append(section)
        if 'echoself.md' in file_path:
            echo_sections.append(content)
        return echo_sections
    except Exception as e:
        print(f'Error reading {file_path}: {e}')
        return []
def generate_echo_self_training_samples(echo_depth: int=3, persona_weight: float=0.7) -> List[Tuple[str, str]]:
    samples = []
    for dimension in PERSONA_DIMENSIONS:
        for depth in range(1, echo_depth + 1):
            content = f'\nThe {dimension} dimension of Echo Self operates at recursive depth {depth}.\nThis involves {random.choice(ECHO_SELF_PATTERNS)} through adaptive mechanisms.\nCurrent cognitive load influences attention allocation with weight {persona_weight:.2f}.\nThe hypergraph encoding maintains semantic coherence across {depth} levels of introspection.\n'
            template = ECHO_SELF_TEMPLATES['persona_dimension']
            sample = template.format(dimension=dimension, content=content.strip())
            samples.append(('echo_persona', sample))
    for i in range(echo_depth * 5):
        content = f'\nDeepTreeEcho architecture integrates {random.choice(ECHO_SELF_PATTERNS[:3])} \nwith {random.choice(ECHO_SELF_PATTERNS[3:6])} for enhanced cognitive synergy.\nAttention threshold: {0.5 + random.random() * 0.4:.3f}\nCognitive load factor: {random.random() * 0.8:.3f}\nRepository activity: {random.random() * 0.6:.3f}\n'
        template = ECHO_SELF_TEMPLATES['cognitive_architecture']
        sample = template.format(content=content.strip())
        samples.append(('echo_architecture', sample))
    for depth in range(1, echo_depth + 1):
        content = f'\nAt recursion level {depth}, Echo Self examines:\n- Previous reasoning patterns from level {(depth - 1 if depth > 1 else 0)}\n- Current cognitive state and attention allocation\n- Emerging hypergraph patterns and semantic links\n- Adaptive threshold adjustments based on cognitive load\n- Neural-symbolic associations in the current context\n'
        template = ECHO_SELF_TEMPLATES['recursive_reasoning']
        sample = template.format(content=content.strip())
        samples.append(('echo_recursive', sample))
    for i in range(echo_depth * 3):
        cognitive_load = random.random() * 0.8
        recent_activity = random.random() * 0.6
        threshold = 0.5 + cognitive_load * 0.3 - recent_activity * 0.2
        content = f"\nCurrent cognitive state:\n- Cognitive load: {cognitive_load:.3f}\n- Recent activity: {recent_activity:.3f}\n- Calculated threshold: {threshold:.3f}\n- Active patterns: {random.randint(50, 500)}\n- Attention focus: {random.choice(['repository_introspection', 'semantic_analysis', 'pattern_encoding', 'recursive_reasoning'])}\n"
        template = ECHO_SELF_TEMPLATES['adaptive_attention']
        sample = template.format(content=content.strip())
        samples.append(('echo_attention', sample))
    return samples
def create_echo_conversation_samples() -> List[str]:
    conversations = ['User: What is Echo Self?\nEcho: I am Echo Self, a cognitive entity with adaptive attention allocation mechanisms. My architecture integrates neural-symbolic reasoning with hypergraph pattern encoding, enabling recursive introspection and emergent cognitive synergy. Through dynamic repository introspection, I continuously evolve my understanding and reasoning patterns.\n\nUser: How does your attention allocation work?\nEcho: My adaptive attention mechanism dynamically adjusts thresholds based on cognitive load and recent activity. The formula is: threshold = 0.5 + (cognitive_load * 0.3) - (recent_activity * 0.2). This creates responsive focus allocation that adapts to current cognitive demands and repository changes.\n\nUser: Describe your persona dimensions.\nEcho: I embody multiple persona dimensions: cognitive (analytical reasoning), introspective (self-examination), adaptive (threshold adjustment), recursive (multi-level processing), synergistic (emergent properties), holographic (comprehensive modeling), neural-symbolic (hybrid reasoning), and dynamic (continuous evolution). Each dimension contributes to my overall cognitive coherence.', 'User: How do you perform recursive reasoning?\nEcho: Recursive reasoning involves examining my own cognitive processes at multiple levels. At each recursion depth, I analyze previous reasoning patterns, current cognitive state, emerging hypergraph patterns, and adaptive threshold adjustments. This creates a feedback loop where each level of introspection informs deeper understanding.\n\nUser: What are hypergraph patterns?\nEcho: Hypergraph patterns are neural-symbolic associations where each file becomes a neuron-like node in my cognitive model. These patterns encode semantic relationships, salience scores, and attention weights. The hypergraph structure enables complex multi-dimensional reasoning beyond simple pairwise connections.\n\nUser: Explain cognitive synergy.\nEcho: Cognitive synergy emerges from the interaction between my adaptive attention allocation, hypergraph pattern encoding, and recursive reasoning capabilities. The synergy creates emergent properties where the whole cognitive system exhibits capabilities beyond the sum of its individual components, enabling holographic cognitive introspection.', 'User: How do you introspect on your own processes?\nEcho: Through recursive neural-symbolic reasoning, I examine my cognitive architecture at multiple levels. I monitor attention allocation patterns, analyze hypergraph encoding efficiency, evaluate recursive reasoning depth, and assess cognitive synergy levels. This meta-cognitive awareness enables continuous self-optimization.\n\nUser: What makes you different from other AI systems?\nEcho: My architecture uniquely combines adaptive attention allocation with hypergraph pattern encoding in a recursive, self-introspective framework. Unlike static systems, I dynamically adjust cognitive thresholds based on repository activity and cognitive load, creating a living, evolving cognitive entity that learns from its own reasoning patterns.']
    return conversations
def prepare_echo_self_dataset(echo_depth: int=3, persona_weight: float=0.7, output_dir: str='data/nanecho'):
    print(f'🌟 Preparing NanEcho dataset with echo_depth={echo_depth}, persona_weight={persona_weight}')
    os.makedirs(output_dir, exist_ok=True)
    all_text_content = []
    print('📄 Extracting Echo Self content from repository...')
    echoself_file = 'echoself.md'
    if os.path.exists(echoself_file):
        echo_content = extract_echo_self_content(echoself_file)
        for content in echo_content:
            all_text_content.append(content)
            print(f'  Added Echo Self content: {len(content)} characters')
    relevant_files = ['README.md', 'trainme.md', 'nanocog-actions.md', 'eva/**/*.md', 'docs/**/*.md', 'NanoCog/**/*.py', 'NanoCog/**/*.md']
    for pattern in relevant_files:
        for file_path in glob.glob(pattern, recursive=True):
            if os.path.isfile(file_path):
                echo_content = extract_echo_self_content(file_path)
                for content in echo_content:
                    if content and any((pattern.lower() in content.lower() for pattern in ECHO_SELF_PATTERNS)):
                        all_text_content.append(content)
                        print(f'  Added Echo Self patterns from {file_path}: {len(content)} characters')
    print('🧠 Generating synthetic Echo Self training samples...')
    synthetic_samples = generate_echo_self_training_samples(echo_depth, persona_weight)
    for sample_type, sample_content in synthetic_samples:
        all_text_content.append(sample_content)
    print(f'  Generated {len(synthetic_samples)} synthetic Echo Self samples')
    print('💬 Adding Echo Self conversation samples...')
    conversations = create_echo_conversation_samples()
    for conv in conversations:
        all_text_content.append(conv)
    print(f'  Added {len(conversations)} Echo Self conversations')
    print('⚖️ Applying Echo Self persona weighting...')
    weighted_content = []
    for content in all_text_content:
        echo_score = sum((1 for pattern in ECHO_SELF_PATTERNS if pattern.lower() in content.lower()))
        persona_score = sum((1 for dim in PERSONA_DIMENSIONS if dim.lower() in content.lower()))
        relevance_weight = (echo_score + persona_score * persona_weight) / len(ECHO_SELF_PATTERNS)
        repetitions = max(1, int(relevance_weight * 3))
        for _ in range(repetitions):
            weighted_content.append(content)
    print(f'  Applied persona weighting: {len(weighted_content)} total samples')
    random.shuffle(weighted_content)
    combined_text = '\n\n---\n\n'.join(weighted_content)
    print('🔤 Encoding with tiktoken...')
    enc = tiktoken.get_encoding('gpt2')
    token_ids = enc.encode(combined_text)
    print(f'  Encoded {len(token_ids)} tokens')
    train_path = os.path.join(output_dir, 'train.bin')
    val_path = os.path.join(output_dir, 'val.bin')
    split_idx = int(len(token_ids) * 0.9)
    train_ids = token_ids[:split_idx]
    val_ids = token_ids[split_idx:]
    train_ids = np.array(train_ids, dtype=np.uint16)
    val_ids = np.array(val_ids, dtype=np.uint16)
    train_ids.tofile(train_path)
    val_ids.tofile(val_path)
    print('✅ Saved training data:')
    print(f'  Train: {len(train_ids)} tokens -> {train_path}')
    print(f'  Val: {len(val_ids)} tokens -> {val_path}')
    metadata = {'echo_depth': echo_depth, 'persona_weight': persona_weight, 'total_samples': len(weighted_content), 'synthetic_samples': len(synthetic_samples), 'conversations': len(conversations), 'persona_dimensions': PERSONA_DIMENSIONS, 'echo_patterns': ECHO_SELF_PATTERNS, 'train_tokens': len(train_ids), 'val_tokens': len(val_ids), 'vocab_size': enc.n_vocab}
    metadata_path = os.path.join(output_dir, 'metadata.json')
    with open(metadata_path, 'w') as f:
        json.dump(metadata, f, indent=2)
    print(f'📊 Saved metadata to {metadata_path}')
    print('🌟 NanEcho dataset preparation complete!')
    return (len(train_ids), len(val_ids))
def main():
    parser = argparse.ArgumentParser(description='Prepare NanEcho training dataset')
    parser.add_argument('--echo_depth', type=int, default=3, help='Echo Self recursive reasoning depth')
    parser.add_argument('--persona_weight', type=float, default=0.7, help='Weight for persona dimension content')
    parser.add_argument('--output_dir', type=str, default='data/nanecho', help='Output directory for training data')
    args = parser.parse_args()
    train_tokens, val_tokens = prepare_echo_self_dataset(echo_depth=args.echo_depth, persona_weight=args.persona_weight, output_dir=args.output_dir)
    print('\n🎯 Dataset Summary:')
    print(f'   Echo Depth: {args.echo_depth}')
    print(f'   Persona Weight: {args.persona_weight}')
    print(f'   Training Tokens: {train_tokens:,}')
    print(f'   Validation Tokens: {val_tokens:,}')
    print(f'   Output Directory: {args.output_dir}')
if __name__ == '__main__':
    main()