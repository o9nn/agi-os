import sys
import logging
from unified_launcher import UnifiedLauncher, create_config_from_args, create_argument_parser
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s', filename='gui_dashboard.log', filemode='a')
logger = logging.getLogger(__name__)
def main():
    parser = create_argument_parser('gui-standalone')
    args = parser.parse_args()
    config = create_config_from_args('gui-standalone', args)
    launcher = UnifiedLauncher()
    return launcher.launch_sync(config)
if __name__ == '__main__':
    sys.exit(main())