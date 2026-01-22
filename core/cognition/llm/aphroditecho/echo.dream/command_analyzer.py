import re
import logging
from typing import Dict
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
class CommandAnalyzer:
    def __init__(self):
        self.patterns = {'create_recursive_function': ['create\\s+(?:a\\s+)?recursive\\s+function\\s+(?:called\\s+)?(\\w+)', 'define\\s+(?:a\\s+)?recursive\\s+function\\s+(?:named\\s+)?(\\w+)', 'make\\s+(?:a\\s+)?recursive\\s+function\\s+(?:for\\s+)?(.+?)\\s+(?:that|which)'], 'fibonacci': ['fibonacci\\s+(?:sequence|series)(?:\\s+for\\s+(\\d+))?', 'calculate\\s+fibonacci\\s+(?:to|up to|for)\\s+(\\d+)', 'generate\\s+(?:the\\s+)?fibonacci\\s+(?:numbers|sequence)(?:\\s+for\\s+(\\d+))?'], 'factorial': ['factorial\\s+(?:of\\s+)?(\\d+)', 'calculate\\s+(?:the\\s+)?factorial\\s+(?:of\\s+)?(\\d+)', "compute\\s+(\\d+)(?:'s|\\s+)factorial"], 'tree_recursion': ['(?:create|make|generate)\\s+(?:a\\s+)?(?:recursive\\s+)?tree(?:\\s+with\\s+depth\\s+(\\d+))?', 'tree\\s+recursion(?:\\s+with\\s+depth\\s+(\\d+))?', 'binary\\s+tree(?:\\s+(?:with|of)\\s+depth\\s+(\\d+))?'], 'backtracking': ['(?:generate|create|find)\\s+(?:all\\s+)?(\\w+)(?:\\s+using\\s+backtracking)?', 'backtracking\\s+for\\s+(\\w+)', 'solve\\s+(\\w+)(?:\\s+using\\s+backtracking)?'], 'merge_function': ['(?:merge|combine)\\s+functions\\s+(\\w+)\\s+and\\s+(\\w+)', 'create\\s+(?:a\\s+)?function\\s+that\\s+uses\\s+both\\s+(\\w+)\\s+and\\s+(\\w+)']}
    def analyze(self, command: str) -> Dict:
        command = command.lower().strip()
        logger.info(f'Analyzing command: {command}')
        result = {'command_type': None, 'params': {}, 'matches': []}
        for cmd_type, pattern_list in self.patterns.items():
            for pattern in pattern_list:
                match = re.search(pattern, command, re.IGNORECASE)
                if match:
                    result['command_type'] = cmd_type
                    result['matches'].append(match.groups())
                    if cmd_type == 'create_recursive_function':
                        result['params']['function_name'] = match.group(1)
                    elif cmd_type == 'fibonacci':
                        n = match.group(1) if match.group(1) else '10'
                        result['params']['n'] = int(n)
                    elif cmd_type == 'factorial':
                        result['params']['n'] = int(match.group(1))
                    elif cmd_type == 'tree_recursion':
                        depth = match.group(1) if match.group(1) else '3'
                        result['params']['depth'] = int(depth)
                    elif cmd_type == 'backtracking':
                        problem_type = match.group(1)
                        if problem_type in ['permutation', 'permutations']:
                            result['params']['problem_type'] = 'permutations'
                        elif problem_type in ['subset', 'subsets']:
                            result['params']['problem_type'] = 'subsets'
                        elif problem_type in ['combination', 'combinations']:
                            result['params']['problem_type'] = 'combinations'
                        else:
                            result['params']['problem_type'] = problem_type
                    elif cmd_type == 'merge_function':
                        result['params']['function1'] = match.group(1)
                        result['params']['function2'] = match.group(2)
                    return result
        if 'fibonacci' in command:
            result['command_type'] = 'fibonacci'
            result['params']['n'] = 10
        elif 'factorial' in command:
            result['command_type'] = 'factorial'
            result['params']['n'] = 5
        elif 'tree' in command:
            result['command_type'] = 'tree_recursion'
            result['params']['depth'] = 3
        elif 'backtrack' in command:
            result['command_type'] = 'backtracking'
            result['params']['problem_type'] = 'subsets'
        return result
    def generate_code(self, analysis: Dict) -> str:
        cmd_type = analysis.get('command_type')
        params = analysis.get('params', {})
        if not cmd_type:
            return '# Could not determine what type of code to generate'
        code = ''
        if cmd_type == 'fibonacci':
            n = params.get('n', 10)
            code = self._generate_fibonacci_code(n)
        elif cmd_type == 'factorial':
            n = params.get('n', 5)
            code = self._generate_factorial_code(n)
        elif cmd_type == 'tree_recursion':
            depth = params.get('depth', 3)
            code = self._generate_tree_recursion_code(depth)
        elif cmd_type == 'backtracking':
            problem_type = params.get('problem_type', 'subsets')
            code = self._generate_backtracking_code(problem_type)
        elif cmd_type == 'create_recursive_function':
            function_name = params.get('function_name', 'recursive_function')
            code = self._generate_custom_recursive_function(function_name)
        elif cmd_type == 'merge_function':
            function1 = params.get('function1', 'func1')
            function2 = params.get('function2', 'func2')
            code = self._generate_merged_functions(function1, function2)
        return code
    def _generate_fibonacci_code(self, n: int) -> str:
        return f'from functools import lru_cache\n\n@lru_cache(maxsize=None)\ndef fibonacci(n):\n    if n <= 1:\n        return n\n    return fibonacci(n-1) + fibonacci(n-2)\n\n# Calculate first {n} terms\nresult = [fibonacci(i) for i in range({n})]\nprint("Fibonacci sequence:", result)\n'
    def _generate_factorial_code(self, n: int) -> str:
        return f'def factorial(n):\n    if n == 0 or n == 1:\n        return 1\n    return n * factorial(n-1)\n\nresult = factorial({n})\nprint(f"Factorial of {n} is:", result)\n'
    def _generate_tree_recursion_code(self, depth: int) -> str:
        return f'def tree_recursion(depth, value):\n    if depth >= {depth}:\n        return [value]\n    \n    result = [value]\n    for i in range(2):  # Binary tree (2 branches)\n        child_value = value * 10 + (i + 1)\n        result.extend(tree_recursion(depth + 1, child_value))\n    return result\n\n# Generate tree with depth {depth}\nresult = tree_recursion(0, 1)\nprint("Tree structure:", result)\n'
    def _generate_backtracking_code(self, problem_type: str) -> str:
        if problem_type == 'permutations':
            return 'def generate_permutations(nums):\n    def backtrack(start):\n        if start == len(nums):\n            result.append(nums.copy())\n            return\n        \n        for i in range(start, len(nums)):\n            # Swap elements\n            nums[start], nums[i] = nums[i], nums[start]\n            # Recurse\n            backtrack(start + 1)\n            # Backtrack (undo the swap)\n            nums[start], nums[i] = nums[i], nums[start]\n    \n    result = []\n    backtrack(0)\n    return result\n\n# Example usage\nnums = [1, 2, 3]\npermutations = generate_permutations(nums)\nprint(f"All permutations of {nums}:", permutations)\n'
        elif problem_type == 'combinations':
            return 'def generate_combinations(nums, k):\n    def backtrack(start, combination):\n        if len(combination) == k:\n            result.append(combination.copy())\n            return\n        \n        for i in range(start, len(nums)):\n            # Add element\n            combination.append(nums[i])\n            # Recurse\n            backtrack(i + 1, combination)\n            # Backtrack\n            combination.pop()\n    \n    result = []\n    backtrack(0, [])\n    return result\n\n# Example usage\nnums = [1, 2, 3, 4]\nk = 2\ncombinations = generate_combinations(nums, k)\nprint(f"Combinations of {k} elements from {nums}:", combinations)\n'
        else:
            return 'def generate_subsets(nums):\n    def backtrack(start, current):\n        result.append(current.copy())\n        \n        for i in range(start, len(nums)):\n            # Add element\n            current.append(nums[i])\n            # Recurse\n            backtrack(i + 1, current)\n            # Backtrack\n            current.pop()\n    \n    result = []\n    backtrack(0, [])\n    return result\n\n# Example usage\nnums = [1, 2, 3]\nsubsets = generate_subsets(nums)\nprint(f"All subsets of {nums}:", subsets)\n'
    def _generate_custom_recursive_function(self, function_name: str) -> str:
        return f'def {function_name}(n, *args):\n    # Base case\n    if n <= 0:\n        return 1\n    \n    # Recursive case\n    return n * {function_name}(n - 1, *args)\n\n# Example usage\nresult = {function_name}(5)\nprint(f"Result of {function_name}(5):", result)\n'
    def _generate_merged_functions(self, function1: str, function2: str) -> str:
        return f'def {function1}(n):\n    # Example implementation\n    if n <= 1:\n        return n\n    return {function1}(n-1) + {function1}(n-2)\n\ndef {function2}(n):\n    # Example implementation\n    if n <= 1:\n        return 1\n    return n * {function2}(n-1)\n\ndef combined_{function1}_{function2}(n):\n    # Use both functions together\n    result1 = {function1}(n)\n    result2 = {function2}(n)\n    return result1 + result2\n\n# Example usage\nn = 5\nresult = combined_{function1}_{function2}(n)\nprint(f"Combined result for n={n}:", result)\n'
if __name__ == '__main__':
    analyzer = CommandAnalyzer()
    test_commands = ['Calculate fibonacci sequence for 10 terms', 'Find the factorial of 5', 'Create a recursive tree with depth 4', 'Generate all permutations using backtracking', 'Define a recursive function called calculate_sum', 'Merge functions fibonacci and factorial']
    for cmd in test_commands:
        print(f'\nCommand: {cmd}')
        analysis = analyzer.analyze(cmd)
        print(f'Analysis: {analysis}')
        code = analyzer.generate_code(analysis)
        print('Generated Code:')
        print(code)