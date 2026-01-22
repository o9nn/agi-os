import time
import os
os.system('export GUILE_AUTO_COMPILE=0')
time.sleep(5)
os.chdir('~/opencog/opencog/build')
os.system('./opencog/server/cogserver &')
time.sleep(30)
path = '/home/doc/'
scm_files = [f for f in os.listdir(path) if f.endswith('.scm')]
os.system(" echo  'scm' | nc localhost 17001")
os.system(" echo '(clear)'  | nc localhost 17001 ")
for scm in scm_files:
    os.system(' echo  \'(load-from-path "' + path + scm + '")\' | nc localhost 17001 ')
os.system(" echo '(count-all)'  | nc localhost 17001 ")