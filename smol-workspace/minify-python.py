#!/usr/bin/env python3
"""
Python code minifier for Smol Protocol
Removes comments, docstrings, and unnecessary whitespace while preserving functionality
"""

import ast
import sys
import re

def minify_python(source: str) -> str:
    """Minify Python source code"""
    
    # Parse the AST
    tree = ast.parse(source)
    
    # Remove docstrings
    for node in ast.walk(tree):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef, ast.Module)):
            if (node.body and isinstance(node.body[0], ast.Expr) and 
                isinstance(node.body[0].value, (ast.Str, ast.Constant))):
                if isinstance(node.body[0].value, ast.Constant) and isinstance(node.body[0].value.value, str):
                    node.body = node.body[1:]
                elif isinstance(node.body[0].value, ast.Str):
                    node.body = node.body[1:]
    
    # Unparse back to source
    minified = ast.unparse(tree)
    
    # Additional minification
    lines = []
    for line in minified.split('\n'):
        # Remove inline comments
        if '#' in line:
            # Be careful not to remove # in strings
            in_string = False
            quote_char = None
            new_line = []
            i = 0
            while i < len(line):
                c = line[i]
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
                i += 1
            line = ''.join(new_line).rstrip()
        
        if line.strip():
            lines.append(line)
    
    return '\n'.join(lines)


def aggressive_minify(source: str) -> str:
    """More aggressive minification - shorten names"""
    
    # First do basic minification
    minified = minify_python(source)
    
    # Map of long names to short names
    name_map = {
        'OptimizationResult': 'OR',
        'OptimizationStatus': 'OS',
        'Transformation': 'T',
        'filepath': 'fp',
        'content': 'c',
        'original_size': 'os',
        'transformed': 'tr',
        'new_size': 'ns',
        'transforms': 'ts',
        'transform': 't',
        'version': 'v',
        'result': 'r',
        'status': 's',
        'functionality_preserved': 'fp',
        'size_reduced': 'sr',
    }
    
    # This is risky - only do it for local variables
    # For now, just return the basic minification
    return minified


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input.py> [output.py]", file=sys.stderr)
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else None
    
    with open(input_file, 'r') as f:
        source = f.read()
    
    original_size = len(source)
    minified = minify_python(source)
    new_size = len(minified)
    
    if output_file:
        with open(output_file, 'w') as f:
            f.write(minified)
        print(f"Minified: {original_size} → {new_size} bytes ({original_size - new_size} saved)")
    else:
        print(minified)
