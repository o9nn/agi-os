#!/usr/bin/env python3
"""
Enhanced Batch Minification Script for Smol Protocol - Iteration 2
More aggressive optimizations: whitespace compression, line joining, etc.
"""

import os
import sys
import ast
import re
from pathlib import Path

EXCLUDED_DIRS = {'.git', 'node_modules', 'external', 'archive', '__pycache__', '.venv', 'venv'}
EXCLUDED_FILES = {'package-lock.json', 'yarn.lock'}


def aggressive_minify_python(content: str) -> str:
    """Aggressively minify Python - remove docstrings, comments, compress whitespace"""
    try:
        tree = ast.parse(content)
        
        # Remove docstrings
        for node in ast.walk(tree):
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef, ast.Module)):
                if (node.body and isinstance(node.body[0], ast.Expr) and 
                    isinstance(node.body[0].value, (ast.Str, ast.Constant))):
                    if isinstance(node.body[0].value, ast.Constant) and isinstance(node.body[0].value.value, str):
                        node.body = node.body[1:]
                    elif isinstance(node.body[0].value, ast.Str):
                        node.body = node.body[1:]
        
        minified = ast.unparse(tree)
        
        # Remove inline comments and compress
        lines = []
        for line in minified.split('\n'):
            # Remove inline comments
            if '#' in line:
                in_string = False
                quote_char = None
                new_line = []
                for i, c in enumerate(line):
                    if c in '"\'':
                        if not in_string:
                            in_string = True
                            quote_char = c
                        elif c == quote_char and (i == 0 or line[i-1] != '\\'):
                            in_string = False
                        new_line.append(c)
                    elif c == '#' and not in_string:
                        break
                    else:
                        new_line.append(c)
                line = ''.join(new_line).rstrip()
            
            # Remove trailing whitespace
            line = line.rstrip()
            
            if line:
                lines.append(line)
        
        return '\n'.join(lines)
    except:
        return content


def aggressive_minify_c(content: str) -> str:
    """Aggressively minify C/C++ - remove comments, compress whitespace"""
    # Remove multi-line comments
    content = re.sub(r'/\*.*?\*/', '', content, flags=re.DOTALL)
    # Remove single-line comments
    content = re.sub(r'//.*$', '', content, flags=re.MULTILINE)
    
    # Process line by line
    lines = []
    for line in content.split('\n'):
        # Strip whitespace
        line = line.strip()
        if not line:
            continue
        
        # Compress multiple spaces to single space (but preserve string literals)
        # Simple approach: just compress spaces outside of quotes
        new_line = []
        in_string = False
        prev_space = False
        for i, c in enumerate(line):
            if c == '"' and (i == 0 or line[i-1] != '\\'):
                in_string = not in_string
                new_line.append(c)
                prev_space = False
            elif c in ' \t' and not in_string:
                if not prev_space:
                    new_line.append(' ')
                    prev_space = True
            else:
                new_line.append(c)
                prev_space = False
        
        line = ''.join(new_line).strip()
        if line:
            lines.append(line)
    
    return '\n'.join(lines)


def aggressive_minify_scheme(content: str) -> str:
    """Aggressively minify Scheme/Lisp"""
    lines = []
    for line in content.split('\n'):
        # Remove comment lines
        if line.strip().startswith(';'):
            continue
        # Remove inline comments
        if ';' in line:
            in_string = False
            new_line = []
            for c in line:
                if c == '"':
                    in_string = not in_string
                if c == ';' and not in_string:
                    break
                new_line.append(c)
            line = ''.join(new_line)
        
        line = line.strip()
        if line:
            lines.append(line)
    
    return '\n'.join(lines)


def aggressive_minify_shell(content: str) -> str:
    """Aggressively minify shell scripts"""
    lines = []
    for i, line in enumerate(content.split('\n')):
        # Keep shebang
        if i == 0 and line.startswith('#!'):
            lines.append(line)
            continue
        # Remove comment lines
        if line.strip().startswith('#'):
            continue
        # Remove inline comments (simple approach)
        if ' #' in line:
            # Only remove if # is preceded by space (likely a comment)
            parts = line.split(' #')
            line = parts[0]
        
        line = line.strip()
        if line:
            lines.append(line)
    
    return '\n'.join(lines)


def aggressive_minify_go(content: str) -> str:
    """Aggressively minify Go"""
    # Remove multi-line comments
    content = re.sub(r'/\*.*?\*/', '', content, flags=re.DOTALL)
    # Remove single-line comments
    content = re.sub(r'//.*$', '', content, flags=re.MULTILINE)
    
    lines = []
    for line in content.split('\n'):
        line = line.strip()
        if line:
            lines.append(line)
    
    return '\n'.join(lines)


def aggressive_minify_js(content: str) -> str:
    """Aggressively minify JavaScript/TypeScript"""
    # Remove multi-line comments
    content = re.sub(r'/\*.*?\*/', '', content, flags=re.DOTALL)
    # Remove single-line comments (but not URLs)
    content = re.sub(r'(?<!:)//(?!/)[^\n]*', '', content)
    
    lines = []
    for line in content.split('\n'):
        line = line.strip()
        if line:
            lines.append(line)
    
    return '\n'.join(lines)


def aggressive_minify_ruby(content: str) -> str:
    """Aggressively minify Ruby"""
    lines = []
    for i, line in enumerate(content.split('\n')):
        # Keep shebang
        if i == 0 and line.startswith('#!'):
            lines.append(line)
            continue
        # Remove comment lines
        if line.strip().startswith('#'):
            continue
        
        line = line.strip()
        if line:
            lines.append(line)
    
    return '\n'.join(lines)


def get_minifier(ext: str):
    """Get the appropriate minifier for a file extension"""
    minifiers = {
        '.py': aggressive_minify_python,
        '.c': aggressive_minify_c,
        '.cpp': aggressive_minify_c,
        '.cc': aggressive_minify_c,
        '.h': aggressive_minify_c,
        '.hpp': aggressive_minify_c,
        '.scm': aggressive_minify_scheme,
        '.lisp': aggressive_minify_scheme,
        '.el': aggressive_minify_scheme,
        '.rkt': aggressive_minify_scheme,
        '.sh': aggressive_minify_shell,
        '.bash': aggressive_minify_shell,
        '.go': aggressive_minify_go,
        '.js': aggressive_minify_js,
        '.ts': aggressive_minify_js,
        '.jsx': aggressive_minify_js,
        '.tsx': aggressive_minify_js,
        '.rb': aggressive_minify_ruby,
        '.jl': aggressive_minify_c,
        '.b': aggressive_minify_c,
        '.zig': aggressive_minify_c,
    }
    return minifiers.get(ext)


def process_file(filepath: str) -> dict:
    """Process a single file and return results"""
    result = {
        'path': filepath,
        'original_size': 0,
        'optimized_size': 0,
        'saved': 0,
        'status': 'skipped',
        'error': None
    }
    
    try:
        path = Path(filepath)
        ext = path.suffix.lower()
        
        minifier = get_minifier(ext)
        if not minifier:
            result['status'] = 'no_minifier'
            return result
        
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            original = f.read()
        
        result['original_size'] = len(original.encode('utf-8'))
        
        if result['original_size'] < 50:
            result['status'] = 'too_small'
            return result
        
        minified = minifier(original)
        result['optimized_size'] = len(minified.encode('utf-8'))
        result['saved'] = result['original_size'] - result['optimized_size']
        
        if result['saved'] > 0:
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(minified)
            result['status'] = 'optimized'
        else:
            result['status'] = 'no_savings'
        
        return result
        
    except Exception as e:
        result['status'] = 'error'
        result['error'] = str(e)
        return result


def find_files(root_dir: str, extensions: set) -> list:
    """Find all files with given extensions"""
    files = []
    for dirpath, dirnames, filenames in os.walk(root_dir):
        dirnames[:] = [d for d in dirnames if d not in EXCLUDED_DIRS]
        
        for filename in filenames:
            if filename in EXCLUDED_FILES:
                continue
            ext = Path(filename).suffix.lower()
            if ext in extensions:
                files.append(os.path.join(dirpath, filename))
    
    return files


def main():
    if len(sys.argv) < 2:
        print("Usage: batch-minify-v2.py <directory> [--dry-run] [--ext=.py,.js,...]")
        sys.exit(1)
    
    root_dir = sys.argv[1]
    dry_run = '--dry-run' in sys.argv
    
    extensions = {'.py', '.c', '.cpp', '.h', '.js', '.ts', '.scm', '.go', '.sh', '.rb', '.jl', '.b'}
    for arg in sys.argv:
        if arg.startswith('--ext='):
            extensions = set(arg[6:].split(','))
    
    print(f"[Iteration 2] Scanning {root_dir} for files with extensions: {extensions}")
    files = find_files(root_dir, extensions)
    print(f"Found {len(files)} files to process")
    
    if dry_run:
        print("Dry run mode - no files will be modified")
        return
    
    total_original = 0
    total_optimized = 0
    optimized_count = 0
    by_ext = {}
    
    for i, filepath in enumerate(files):
        if i % 500 == 0:
            print(f"Processing {i}/{len(files)}...")
        
        result = process_file(filepath)
        
        if result['status'] == 'optimized':
            total_original += result['original_size']
            total_optimized += result['optimized_size']
            optimized_count += 1
            
            ext = Path(filepath).suffix
            if ext not in by_ext:
                by_ext[ext] = {'original': 0, 'optimized': 0, 'count': 0}
            by_ext[ext]['original'] += result['original_size']
            by_ext[ext]['optimized'] += result['optimized_size']
            by_ext[ext]['count'] += 1
    
    print("\n" + "="*60)
    print("ITERATION 2 OPTIMIZATION SUMMARY")
    print("="*60)
    print(f"Files processed: {len(files)}")
    print(f"Files optimized: {optimized_count}")
    print(f"Total original: {total_original:,} bytes")
    print(f"Total optimized: {total_optimized:,} bytes")
    print(f"Total saved: {total_original - total_optimized:,} bytes")
    if total_original > 0:
        print(f"Reduction: {((total_original - total_optimized) / total_original) * 100:.1f}%")
    
    print("\nBy Extension:")
    for ext, data in sorted(by_ext.items()):
        saved = data['original'] - data['optimized']
        pct = (saved / data['original']) * 100 if data['original'] > 0 else 0
        print(f"  {ext}: {data['original']:,} → {data['optimized']:,} ({saved:,} saved, {pct:.1f}%) [{data['count']} files]")


if __name__ == '__main__':
    main()
