import os
import sys
import glob
import json
import time
import requests
import tiktoken
import numpy as np
from collections import Counter, defaultdict
from datetime import datetime
COGPRIME_PAPER_URL = 'https://raw.githubusercontent.com/drzo/cogprime/main/CogPrime%20-%20An%20Integrative%20Architecture%20for%20Embodied%20Artificial%20General%20Intelligence.md'
DOCUMENT_SEPARATOR = '\n\n<|endofdocument|>\n\n'
SECTION_SEPARATOR = '\n\n---\n\n'
FILE_TYPE_MARKERS = {'.md': '\n\n<!-- Markdown Document: {filename} -->\n\n', '.scm': '\n\n;; Scheme File: {filename}\n;; Path: {filepath}\n\n', '.py': '\n\n# Python File: {filename}\n# Path: {filepath}\n\n', '.txt': '\n\n# Text File: {filename}\n\n'}
DEFAULT_MARKER = '\n\n# File: {filename}\n# Path: {filepath}\n\n'
TRAIN_RATIO = 0.9
COGNITIVE_SCHEMATIC_TEMPLATES = {'context_procedure_goal': '\n;; Cognitive Schematic: Context → Procedure → Goal\n(ImplicationLink (stv 0.85 0.92)\n  (AndLink\n    (StateLink (ConceptNode "Context-{context}") (ConceptNode "active"))\n    (EvaluationLink (PredicateNode "condition-{condition}") \n                   (ListLink (VariableNode "$X") (ConceptNode "parameter-{param}"))))\n  (SequentialLink\n    (ExecutionLink (SchemaNode "procedure-{proc1}") (VariableNode "$X"))\n    (ExecutionLink (SchemaNode "procedure-{proc2}") (VariableNode "$X"))\n    (EvaluationLink (PredicateNode "goal-{goal}") (VariableNode "$X"))))\n', 'attention_allocation': '\n;; ECAN Attention Allocation Pattern\n(AtomSpace\n  (set-sti! (ConceptNode "{concept}") {sti_value})\n  (set-lti! (ConceptNode "{memory_pattern}") {lti_value})\n  (set-av! (SchemaNode "{schema}") (av {av_sti} {av_lti})))\n\n;; Attention spreading pattern\n(cog-stimulate (ConceptNode "{target_concept}") {stimulation_amount})\n', 'inference_chain': '\n;; PLN Inference Chain\n(ImplicationLink (stv {tv_strength} {tv_confidence})\n  (InheritanceLink (ConceptNode "{concept_a}") (ConceptNode "{concept_b}"))\n  (InheritanceLink (ConceptNode "{concept_b}") (ConceptNode "{concept_c}")))\n\n(InheritanceLink (stv {derived_strength} {derived_confidence})\n  (ConceptNode "{concept_a}") (ConceptNode "{concept_c}"))\n', 'goal_hierarchy': '\n;; Goal Hierarchy Structure\n(ImplicationLink (stv 0.9 0.85)\n  (SatisfactionLink (GoalNode "{parent_goal}"))\n  (AndLink\n    (SatisfactionLink (GoalNode "{sub_goal_1}"))\n    (SatisfactionLink (GoalNode "{sub_goal_2}"))\n    (SatisfactionLink (GoalNode "{sub_goal_3}"))))\n\n;; Goal activation pattern  \n(EvaluationLink (stv 0.8 0.7)\n  (PredicateNode "goal-priority")\n  (ListLink (GoalNode "{parent_goal}") (NumberNode {priority})))\n', 'pattern_mining_result': '\n;; Pattern Mining Discovery\n(EvaluationLink (stv {support} {confidence})\n  (PredicateNode "frequent-pattern")\n  (ListLink\n    (ConceptNode "{pattern_element_1}")\n    (ConceptNode "{pattern_element_2}")\n    (ConceptNode "{pattern_element_3}")))\n\n;; Surprising pattern detection\n(EvaluationLink (stv {surprise_value} 0.9)\n  (PredicateNode "surprising-association")\n  (ListLink (ConceptNode "{element_a}") (ConceptNode "{element_b}")))\n'}
DIAGNOSTIC_PATTERN_TEMPLATES = {'bottleneck_analysis': '\n;; Cognitive Bottleneck Analysis\n;; Context: High STI concentration indicates attention bottleneck\n(EvaluationLink (stv 0.9 0.8)\n  (PredicateNode "attention-bottleneck")\n  (ListLink\n    (ConceptNode "sti-distribution")\n    (NumberNode {high_sti_count})\n    (NumberNode {total_atoms})))\n\n;; Recommendation: Adjust ECAN parameters\n(ImplicationLink (stv 0.85 0.9)\n  (EvaluationLink (PredicateNode "attention-bottleneck") (VariableNode "$X"))\n  (ExecutionLink (SchemaNode "adjust-ecan-decay") (NumberNode {decay_rate})))\n', 'goal_proliferation': '\n;; Goal Proliferation Detection\n(EvaluationLink (stv {severity} 0.9)\n  (PredicateNode "goal-proliferation")\n  (ListLink (NumberNode {active_goals}) (NumberNode {threshold})))\n\n;; Pruning recommendation\n(ImplicationLink (stv 0.8 0.85)\n  (EvaluationLink (PredicateNode "goal-proliferation") (VariableNode "$X"))\n  (ExecutionLink (SchemaNode "increase-goal-selection-threshold") \n                 (NumberNode {new_threshold})))\n', 'schematic_success_analysis': '\n;; Cognitive Schematic Success Rate Analysis\n(EvaluationLink (stv {success_rate} {confidence})\n  (PredicateNode "schematic-performance")\n  (ListLink\n    (ConceptNode "{schematic_type}")\n    (NumberNode {success_count})\n    (NumberNode {total_attempts})))\n\n;; Learning parameter adjustment\n(ImplicationLink (stv 0.9 0.8)\n  (EvaluationLink (PredicateNode "low-schematic-success") (VariableNode "$X"))\n  (ExecutionLink (SchemaNode "adjust-learning-parameters")\n                 (ListLink (NumberNode {new_learning_rate}) \n                          (NumberNode {new_exploration_factor}))))\n'}
def download_file(url, output_path):
    try:
        response = requests.get(url, stream=True)
        response.raise_for_status()
        with open(output_path, 'wb') as f:
            for chunk in response.iter_content(chunk_size=8192):
                f.write(chunk)
        print(f'✓ Downloaded {url} to {output_path}')
        return True
    except requests.exceptions.RequestException as e:
        print(f'✗ Error downloading {url}: {e}')
        return False
def read_file_content(file_path):
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            return f.read()
    except UnicodeDecodeError:
        try:
            with open(file_path, 'r', encoding='latin-1') as f:
                content = f.read()
                print(f'⚠ File {file_path} read with latin-1 encoding')
                return content
        except Exception as e:
            print(f'✗ Error reading file {file_path} with latin-1: {e}')
            return ''
    except FileNotFoundError:
        print(f'⚠ File not found {file_path}')
        return ''
    except Exception as e:
        print(f'✗ Error reading file {file_path}: {e}')
        return ''
def find_repository_path(repo_name, possible_locations=None):
    if possible_locations is None:
        script_dir = os.path.dirname(os.path.abspath(__file__))
        possible_locations = [os.path.abspath(os.path.join(script_dir, '..', '..', repo_name)), os.path.abspath(os.path.join(script_dir, '..', repo_name)), os.path.abspath(os.path.join(script_dir, '..', '..', '..', repo_name)), os.path.abspath(os.path.join(script_dir, repo_name))]
    for location in possible_locations:
        if os.path.exists(location) and os.path.isdir(location):
            print(f"✓ Found repository '{repo_name}' at: {location}")
            return location
    print(f"⚠ Could not find repository '{repo_name}' in any of the checked locations")
    return None
def get_file_marker(file_path):
    filename = os.path.basename(file_path)
    ext = os.path.splitext(filename)[1].lower()
    marker_template = FILE_TYPE_MARKERS.get(ext, DEFAULT_MARKER)
    return marker_template.format(filename=filename, filepath=file_path)
def get_token_stats(token_ids, enc):
    token_counter = Counter(token_ids)
    most_common = token_counter.most_common(20)
    most_common_tokens = [(enc.decode([token_id]), count) for token_id, count in most_common]
    unique_tokens = len(token_counter)
    vocab_coverage = unique_tokens / enc.n_vocab * 100
    token_lengths = [len(enc.decode([token_id])) for token_id in set(token_ids)]
    avg_token_length = sum(token_lengths) / len(token_lengths) if token_lengths else 0
    return {'total_tokens': len(token_ids), 'unique_tokens': unique_tokens, 'vocab_coverage_percent': vocab_coverage, 'avg_token_length': avg_token_length, 'most_common_tokens': most_common_tokens}
def get_corpus_stats(all_text_content):
    file_types = defaultdict(int)
    file_sizes = []
    total_chars = 0
    total_words = 0
    for content_item in all_text_content:
        if isinstance(content_item, tuple) and len(content_item) == 2:
            file_path, content = content_item
            ext = os.path.splitext(file_path)[1].lower()
            file_types[ext] += 1
            file_size = len(content)
            file_sizes.append((file_path, file_size))
            total_chars += file_size
            total_words += len(content.split())
    file_sizes.sort(key=lambda x: x[1], reverse=True)
    return {'total_files': len(file_sizes), 'total_chars': total_chars, 'total_words': total_words, 'file_types': dict(file_types), 'largest_files': file_sizes[:10], 'avg_file_size': total_chars / len(file_sizes) if file_sizes else 0}
def print_stats(corpus_stats, token_stats):
    print('\n' + '=' * 80)
    print(' ' * 30 + 'NANOCOG CORPUS STATISTICS')
    print('=' * 80)
    print('\n📊 CORPUS OVERVIEW:')
    print(f"  • Total files: {corpus_stats['total_files']}")
    print(f"  • Total characters: {corpus_stats['total_chars']:,}")
    print(f"  • Total words: {corpus_stats['total_words']:,}")
    print(f"  • Average file size: {corpus_stats['avg_file_size']:.1f} characters")
    print('\n📂 FILE TYPES:')
    for ext, count in corpus_stats['file_types'].items():
        print(f"  • {ext or 'no extension'}: {count} files")
    print('\n📄 LARGEST FILES:')
    for i, (file_path, size) in enumerate(corpus_stats['largest_files'][:5], 1):
        print(f'  {i}. {os.path.basename(file_path)}: {size:,} characters')
    print('\n🔤 TOKENIZATION:')
    print(f"  • Total tokens: {token_stats['total_tokens']:,}")
    print(f"  • Unique tokens: {token_stats['unique_tokens']:,}")
    print(f"  • Vocabulary coverage: {token_stats['vocab_coverage_percent']:.2f}%")
    print(f"  • Average token length: {token_stats['avg_token_length']:.2f} characters")
    print('\n📊 TRAIN/VAL SPLIT:')
    train_tokens = int(token_stats['total_tokens'] * TRAIN_RATIO)
    val_tokens = token_stats['total_tokens'] - train_tokens
    print(f'  • Training set: {train_tokens:,} tokens ({TRAIN_RATIO * 100:.0f}%)')
    print(f'  • Validation set: {val_tokens:,} tokens ({(1 - TRAIN_RATIO) * 100:.0f}%)')
    print('\n' + '=' * 80)
def collect_files(directory, file_pattern, description):
    files = glob.glob(os.path.join(directory, file_pattern), recursive=True)
    if files:
        print(f'✓ Found {len(files)} {description} files')
    else:
        print(f'⚠ No {description} files found matching pattern: {os.path.join(directory, file_pattern)}')
    return files
def process_file(file_path, all_text_content):
    print(f'  Processing: {file_path}')
    content = read_file_content(file_path)
    if content:
        marker = get_file_marker(file_path)
        all_text_content.append((file_path, marker + content))
        return True
    return False
def generate_hypergraph_samples():
    import random
    samples = []
    contexts = ['human_interaction', 'problem_solving', 'learning', 'exploration', 'communication', 'planning']
    conditions = ['present', 'active', 'satisfied', 'triggered', 'available', 'detected']
    procedures = ['analyze', 'respond', 'learn', 'explore', 'communicate', 'plan', 'execute', 'evaluate']
    goals = ['understand', 'achieve', 'learn', 'explore', 'help', 'optimize', 'create', 'solve']
    concepts = ['knowledge', 'experience', 'pattern', 'relationship', 'behavior', 'skill', 'memory', 'attention']
    for i in range(20):
        template_name = random.choice(list(COGNITIVE_SCHEMATIC_TEMPLATES.keys()))
        template = COGNITIVE_SCHEMATIC_TEMPLATES[template_name]
        if template_name == 'context_procedure_goal':
            content = template.format(context=random.choice(contexts), condition=random.choice(conditions), param=random.choice(concepts), proc1=random.choice(procedures), proc2=random.choice(procedures), goal=random.choice(goals))
        elif template_name == 'attention_allocation':
            content = template.format(concept=random.choice(concepts), sti_value=round(random.uniform(0.1, 0.9), 2), memory_pattern=f'{random.choice(concepts)}_pattern', lti_value=round(random.uniform(0.1, 0.8), 2), schema=f'{random.choice(procedures)}_schema', av_sti=round(random.uniform(0.1, 0.9), 2), av_lti=round(random.uniform(0.1, 0.8), 2), target_concept=random.choice(concepts), stimulation_amount=round(random.uniform(0.1, 0.5), 2))
        elif template_name == 'inference_chain':
            concept_a = random.choice(concepts)
            concept_b = f'{random.choice(concepts)}_type'
            concept_c = f'{random.choice(concepts)}_category'
            content = template.format(concept_a=concept_a, concept_b=concept_b, concept_c=concept_c, tv_strength=round(random.uniform(0.7, 0.95), 2), tv_confidence=round(random.uniform(0.8, 0.95), 2), derived_strength=round(random.uniform(0.6, 0.9), 2), derived_confidence=round(random.uniform(0.7, 0.9), 2))
        elif template_name == 'goal_hierarchy':
            content = template.format(parent_goal=f'{random.choice(goals)}_main', sub_goal_1=f'{random.choice(goals)}_sub1', sub_goal_2=f'{random.choice(goals)}_sub2', sub_goal_3=f'{random.choice(goals)}_sub3', priority=random.randint(1, 10))
        elif template_name == 'pattern_mining_result':
            content = template.format(support=round(random.uniform(0.1, 0.8), 2), confidence=round(random.uniform(0.7, 0.95), 2), pattern_element_1=random.choice(concepts), pattern_element_2=random.choice(concepts), pattern_element_3=random.choice(concepts), surprise_value=round(random.uniform(0.6, 0.9), 2), element_a=random.choice(concepts), element_b=random.choice(concepts))
        file_path = f'synthetic_cognitive_schematic_{template_name}_{i + 1}.scm'
        marker = get_file_marker(file_path)
        samples.append((file_path, marker + content))
    for i in range(15):
        template_name = random.choice(list(DIAGNOSTIC_PATTERN_TEMPLATES.keys()))
        template = DIAGNOSTIC_PATTERN_TEMPLATES[template_name]
        if template_name == 'bottleneck_analysis':
            total_atoms = random.randint(5000, 20000)
            high_sti_count = random.randint(100, 500)
            content = template.format(high_sti_count=high_sti_count, total_atoms=total_atoms, decay_rate=round(random.uniform(0.01, 0.1), 3))
        elif template_name == 'goal_proliferation':
            active_goals = random.randint(8, 25)
            threshold = 7
            severity = 0.9 if active_goals > 15 else 0.6
            content = template.format(severity=severity, active_goals=active_goals, threshold=threshold, new_threshold=threshold + 2)
        elif template_name == 'schematic_success_analysis':
            total_attempts = random.randint(50, 200)
            success_count = random.randint(20, total_attempts)
            success_rate = round(success_count / total_attempts, 2)
            content = template.format(success_rate=success_rate, confidence=round(random.uniform(0.8, 0.95), 2), schematic_type=f'{random.choice(procedures)}_schematic', success_count=success_count, total_attempts=total_attempts, new_learning_rate=round(random.uniform(0.001, 0.01), 4), new_exploration_factor=round(random.uniform(0.1, 0.3), 2))
        file_path = f'synthetic_diagnostic_pattern_{template_name}_{i + 1}.scm'
        marker = get_file_marker(file_path)
        samples.append((file_path, marker + content))
    curriculum_examples = ['\n;; Basic Atomese Construction\n(ConceptNode "basic_concept")\n(PredicateNode "simple_predicate") \n(ListLink (ConceptNode "element1") (ConceptNode "element2"))\n(EvaluationLink (PredicateNode "relation") \n                (ListLink (ConceptNode "subject") (ConceptNode "object")))\n', '\n;; Intermediate Cognitive Pattern\n(ImplicationLink (stv 0.8 0.9)\n  (EvaluationLink (PredicateNode "condition") (VariableNode "$X"))\n  (EvaluationLink (PredicateNode "consequence") (VariableNode "$X")))\n  \n(InheritanceLink (ConceptNode "specific") (ConceptNode "general"))\n', '\n;; Advanced Neural-Symbolic Synergy\n(define moses-fitness-function\n  (lambda (program)\n    (let ((predictions (execute-program program test-cases))\n          (ecan-relevance (get-attention-value program)))\n      (* (accuracy predictions) ecan-relevance))))\n\n(BindLink\n  (VariableList (VariableNode "$X") (VariableNode "$Y"))\n  (AndLink\n    (InheritanceLink (VariableNode "$X") (ConceptNode "learning_target"))\n    (EvaluationLink (PredicateNode "pln_inference") \n                   (ListLink (VariableNode "$X") (VariableNode "$Y"))))\n  (ExecutionLink (SchemaNode "moses_evolve") \n                 (ListLink (VariableNode "$X") (VariableNode "$Y"))))\n']
    for i, example in enumerate(curriculum_examples):
        file_path = f'synthetic_curriculum_example_{i + 1}.scm'
        marker = get_file_marker(file_path)
        samples.append((file_path, marker + example))
    return samples
def main():
    start_time = time.time()
    script_dir = os.path.dirname(os.path.abspath(__file__))
    nanocog_dir = script_dir
    output_dir = os.path.join(nanocog_dir, 'data')
    os.makedirs(output_dir, exist_ok=True)
    print('\n🔍 NanoCog Data Preparation')
    print('=' * 50)
    opencog_central_path = find_repository_path('opencog-central')
    cogprime_path = find_repository_path('cogprime')
    all_text_content = []
    cogprime_paper_local_path = os.path.join(output_dir, 'cogprime_paper.md')
    print('\n📄 Processing CogPrime Main Paper')
    if download_file(COGPRIME_PAPER_URL, cogprime_paper_local_path):
        paper_content = read_file_content(cogprime_paper_local_path)
        if paper_content:
            marker = get_file_marker(cogprime_paper_local_path)
            all_text_content.append((cogprime_paper_local_path, marker + paper_content))
    if opencog_central_path:
        print('\n📚 Processing opencog-central Documentation')
        opencog_docs_files = ['README.md', 'docs/CogPrime_Integrative_Architecture_AGI.md', 'docs/IMPLEMENTATION_GUIDE.md', 'docs/COGPRIME_STATUS_2024.md', 'docs/COGPRIME_ARCHITECTURE_DIAGRAM.md', 'examples/SIMPLE_COGPRIME_AGENT.md', 'profile/README.md']
        for doc_file in opencog_docs_files:
            file_path = os.path.join(opencog_central_path, doc_file)
            process_file(file_path, all_text_content)
        print('\n💻 Processing opencog-central Scheme Files')
        os.path.join(opencog_central_path, 'Scheme', '**', '*.scm')
        scheme_files = collect_files(opencog_central_path, 'Scheme/**/*.scm', 'Scheme')
        for scm_file in scheme_files:
            process_file(scm_file, all_text_content)
    if cogprime_path:
        print('\n📘 Processing Additional CogPrime Resources')
        cogprime_docs = collect_files(cogprime_path, '*.md', 'CogPrime markdown')
        for doc_file in cogprime_docs:
            process_file(doc_file, all_text_content)
        episodes_dir = os.path.join(cogprime_path, '50 Episodes in Relevance Realization')
        if os.path.exists(episodes_dir):
            episode_files = collect_files(episodes_dir, '*.md', 'Relevance Realization episodes')
            for episode_file in episode_files:
                process_file(episode_file, all_text_content)
        src_files = collect_files(cogprime_path, 'src/**/*.py', 'Python source')
        for src_file in src_files:
            process_file(src_file, all_text_content)
    if not all_text_content:
        print('\n❌ No content collected. Exiting. Please check data source paths and availability.')
        sys.exit(1)
    print('\n🧠 Injecting hypergraph-encoded cognitive patterns...')
    hypergraph_samples = generate_hypergraph_samples()
    all_text_content.extend(hypergraph_samples)
    print(f'   Added {len(hypergraph_samples)} hypergraph pattern samples')
    print('\n📊 Calculating corpus statistics...')
    corpus_stats = get_corpus_stats(all_text_content)
    print('\n🔄 Concatenating text data...')
    full_text_data = ''
    for _, content in all_text_content:
        full_text_data += content + DOCUMENT_SEPARATOR
    print('\n🔤 Tokenizing data with GPT-2 tokenizer...')
    enc = tiktoken.get_encoding('gpt2')
    token_ids = enc.encode_ordinary(full_text_data)
    token_stats = get_token_stats(token_ids, enc)
    print_stats(corpus_stats, token_stats)
    print('\n📂 Splitting data into training and validation sets...')
    n_tokens = len(token_ids)
    split_idx = int(n_tokens * TRAIN_RATIO)
    train_data_ids = token_ids[:split_idx]
    val_data_ids = token_ids[split_idx:]
    train_ids_np = np.array(train_data_ids, dtype=np.uint16)
    val_ids_np = np.array(val_data_ids, dtype=np.uint16)
    train_output_path = os.path.join(output_dir, 'train.bin')
    val_output_path = os.path.join(output_dir, 'val.bin')
    print(f'\n💾 Saving training data to {train_output_path}...')
    train_ids_np.tofile(train_output_path)
    print(f'💾 Saving validation data to {val_output_path}...')
    val_ids_np.tofile(val_output_path)
    metadata = {'date_created': datetime.now().isoformat(), 'corpus_stats': corpus_stats, 'token_stats': {k: v for k, v in token_stats.items() if k != 'most_common_tokens'}, 'train_tokens': len(train_data_ids), 'val_tokens': len(val_data_ids), 'tokenizer': 'gpt2', 'train_ratio': TRAIN_RATIO, 'sources': {'cogprime_paper': bool(cogprime_paper_local_path and os.path.exists(cogprime_paper_local_path)), 'opencog_central': bool(opencog_central_path), 'cogprime_repo': bool(cogprime_path)}}
    metadata_path = os.path.join(output_dir, 'metadata.json')
    with open(metadata_path, 'w', encoding='utf-8') as f:
        json.dump(metadata, f, indent=2)
    if os.path.exists(cogprime_paper_local_path):
        try:
            os.remove(cogprime_paper_local_path)
            print(f'\n🧹 Cleaned up temporary file: {cogprime_paper_local_path}')
        except OSError as e:
            print(f'\n⚠ Error deleting temporary file {cogprime_paper_local_path}: {e}')
    elapsed_time = time.time() - start_time
    print(f'\n✅ Data preparation complete in {elapsed_time:.2f} seconds!')
    print('📦 Output files:')
    print(f'   • {train_output_path} ({os.path.getsize(train_output_path) / 1024 / 1024:.2f} MB)')
    print(f'   • {val_output_path} ({os.path.getsize(val_output_path) / 1024 / 1024:.2f} MB)')
    print(f'   • {metadata_path}')
    print('\nYou can now train NanoCog using:')
    print('   python train.py config/train_cogprime.py --out_dir=out-nanocog')
    print('=' * 80 + '\n')
if __name__ == '__main__':
    main()