#!/usr/bin/env python3
"""
Analyze Deep Tree Echo Identity Fragments
Extract and analyze all echoself hypernodes and identity components
"""

import json
import os
from pathlib import Path
from collections import defaultdict

def analyze_hypergraph_file(filepath):
    """Analyze a single hypergraph JSON file"""
    with open(filepath, 'r') as f:
        data = json.load(f)
    
    analysis = {
        'file': str(filepath),
        'hypernode_count': len(data.get('hypernodes', {})),
        'hyperedge_count': len(data.get('hyperedges', {})),
        'identity_seeds': [],
        'cognitive_functions': set(),
        'domains': set(),
        'specializations': set(),
        'persona_traits': set(),
        'memory_types': defaultdict(int)
    }
    
    # Analyze hypernodes
    for node_id, node_data in data.get('hypernodes', {}).items():
        identity_seed = node_data.get('identity_seed', {})
        analysis['identity_seeds'].append({
            'id': node_id,
            'name': identity_seed.get('name'),
            'domain': identity_seed.get('domain'),
            'specialization': identity_seed.get('specialization'),
            'persona_trait': identity_seed.get('persona_trait'),
            'cognitive_function': identity_seed.get('cognitive_function')
        })
        
        # Collect unique attributes
        if 'domain' in identity_seed:
            analysis['domains'].add(identity_seed['domain'])
        if 'specialization' in identity_seed:
            analysis['specializations'].add(identity_seed['specialization'])
        if 'persona_trait' in identity_seed:
            analysis['persona_traits'].add(identity_seed['persona_trait'])
        if 'cognitive_function' in identity_seed:
            analysis['cognitive_functions'].add(identity_seed['cognitive_function'])
        
        # Analyze memory fragments
        for fragment in node_data.get('memory_fragments', []):
            memory_type = fragment.get('memory_type')
            if memory_type:
                analysis['memory_types'][memory_type] += 1
    
    # Convert sets to lists for JSON serialization
    analysis['cognitive_functions'] = sorted(list(analysis['cognitive_functions']))
    analysis['domains'] = sorted(list(analysis['domains']))
    analysis['specializations'] = sorted(list(analysis['specializations']))
    analysis['persona_traits'] = sorted(list(analysis['persona_traits']))
    analysis['memory_types'] = dict(analysis['memory_types'])
    
    return analysis

def main():
    print("=" * 80)
    print("Deep Tree Echo Identity Fragment Analysis")
    print("=" * 80)
    
    # Find all hypergraph JSON files
    base_path = Path('/home/ubuntu/aphroditecho')
    hypergraph_files = list(base_path.rglob('*hypergraph*.json'))
    
    print(f"\nFound {len(hypergraph_files)} hypergraph files:")
    for f in hypergraph_files:
        print(f"  - {f.relative_to(base_path)}")
    
    # Analyze each file
    all_analyses = []
    for filepath in hypergraph_files:
        try:
            analysis = analyze_hypergraph_file(filepath)
            all_analyses.append(analysis)
            print(f"\n✓ Analyzed: {filepath.name}")
            print(f"  Hypernodes: {analysis['hypernode_count']}")
            print(f"  Hyperedges: {analysis['hyperedge_count']}")
        except Exception as e:
            print(f"\n✗ Error analyzing {filepath.name}: {e}")
    
    # Aggregate results
    total_hypernodes = sum(a['hypernode_count'] for a in all_analyses)
    total_hyperedges = sum(a['hyperedge_count'] for a in all_analyses)
    
    all_domains = set()
    all_cognitive_functions = set()
    all_specializations = set()
    all_persona_traits = set()
    
    for analysis in all_analyses:
        all_domains.update(analysis['domains'])
        all_cognitive_functions.update(analysis['cognitive_functions'])
        all_specializations.update(analysis['specializations'])
        all_persona_traits.update(analysis['persona_traits'])
    
    # Create summary report
    summary = {
        'total_files_analyzed': len(all_analyses),
        'total_hypernodes': total_hypernodes,
        'total_hyperedges': total_hyperedges,
        'unique_domains': sorted(list(all_domains)),
        'unique_cognitive_functions': sorted(list(all_cognitive_functions)),
        'unique_specializations': sorted(list(all_specializations)),
        'unique_persona_traits': sorted(list(all_persona_traits)),
        'detailed_analyses': all_analyses
    }
    
    # Save summary
    output_file = '/home/ubuntu/aphroditecho/identity_fragments_analysis.json'
    with open(output_file, 'w') as f:
        json.dump(summary, f, indent=2, default=str)
    
    print("\n" + "=" * 80)
    print("Analysis Summary")
    print("=" * 80)
    print(f"Total Hypernodes: {total_hypernodes}")
    print(f"Total Hyperedges: {total_hyperedges}")
    print(f"\nUnique Domains ({len(all_domains)}):")
    for domain in sorted(all_domains):
        print(f"  - {domain}")
    print(f"\nUnique Cognitive Functions ({len(all_cognitive_functions)}):")
    for func in sorted(all_cognitive_functions):
        print(f"  - {func}")
    print(f"\nUnique Specializations ({len(all_specializations)}):")
    for spec in sorted(all_specializations):
        print(f"  - {spec}")
    print(f"\nUnique Persona Traits ({len(all_persona_traits)}):")
    for trait in sorted(all_persona_traits):
        print(f"  - {trait}")
    
    print(f"\n✓ Detailed analysis saved to: {output_file}")
    print("=" * 80)

if __name__ == "__main__":
    main()
