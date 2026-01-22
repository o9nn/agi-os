import logging
import sys
RESET = '\x1b[0m'
COLOR_MAP = {logging.DEBUG: '\x1b[33m', logging.INFO: '\x1b[32m', logging.WARNING: '\x1b[35m', logging.ERROR: '\x1b[31m', logging.CRITICAL: '\x1b[1;31m'}
class ColoredFormatter(logging.Formatter):
    def format(self, record):
        color = COLOR_MAP.get(record.levelno, RESET)
        record.levelname = f'{color}{record.levelname}{RESET}'
        msg = super().format(record)
        return msg
log_format = '%(asctime)s | [%(levelname)s] | %(message)s'
date_format = '%Y-%m-%d %H:%M:%S'
log = logging.getLogger('python_client')
log.setLevel(logging.INFO)
handler = logging.StreamHandler(sys.stdout)
handler.setFormatter(ColoredFormatter(fmt=log_format, datefmt=date_format))
log.addHandler(handler)