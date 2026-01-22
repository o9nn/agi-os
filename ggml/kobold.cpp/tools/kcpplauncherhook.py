import os
import sys
pyd_subdir = os.path.join(sys._MEIPASS, 'pyds')
print('Augmenting PYD directory...')
if os.path.isdir(pyd_subdir):
    sys.path.insert(0, pyd_subdir)