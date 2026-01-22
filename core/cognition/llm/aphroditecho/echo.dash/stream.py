import sys
from pathlib import Path
import subprocess
def main():
    try:
        echo_dir = Path.home() / '.deep_tree_echo'
        if not echo_dir.exists():
            print('Deep Tree Echo directory not found. Is Deep Tree Echo running?')
            sys.exit(1)
        subprocess.run(['python3', 'monitor_interface.py'])
    except KeyboardInterrupt:
        print('\nStream monitor closed.')
    except Exception as e:
        print(f'Error: {str(e)}')
        sys.exit(1)
if __name__ == '__main__':
    main()