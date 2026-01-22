import sys
import subprocess
import os
def main():
    try:
        result = subprocess.run([sys.executable, 'dtesn_compiler.py', 'generate-docs'], cwd=os.path.dirname(os.path.abspath(__file__)))
        return result.returncode == 0
    except Exception as e:
        print(f'Error running DTESN compiler: {e}')
        return False
if __name__ == '__main__':
    success = main()
    sys.exit(0 if success else 1)