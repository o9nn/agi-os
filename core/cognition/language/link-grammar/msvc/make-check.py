import os
import sys
import re
local_prop_file = 'Local.props'
scriptdir = '..\\bindings\\python-examples'
pyscript = 'tests.py'
def error(msg):
    if msg:
        print(msg)
    prog = os.path.basename(sys.argv[0])
    print('Usage: ', prog, '[python_flag] PYTHON_OUTDIR [script.py] [script_args]')
    print('        OUTDIR is in the format of "x64\\Debug\\Python3"')
    sys.exit(1)
local_prop = {}
def read_props(vsfile):
    vs_f = open(vsfile, 'r')
    macdef_re = re.compile('<(\\w+)>([^<]*)<')
    for line in vs_f:
        read_m = re.search(macdef_re, line)
        if read_m is None:
            continue
        if len(read_m.groups()) != 2:
            error('Bad line in "{}": {}'.format(vsfile, line))
        local_prop[read_m.group(1)] = read_m.group(2)
    if not local_prop:
        error('No properties found in {}.'.format(vsfile))
NODEFAULT = object()
prop_re = re.compile('\\$\\((\\w+)')
def get_prop(prop, default=NODEFAULT):
    prop_val = local_prop.get(prop, None)
    if prop_val is None:
        if default is NODEFAULT:
            error('Property "{}" not found in {}'.format(prop, local_prop_file))
        return default
    while True:
        prop_m = re.search(prop_re, prop_val)
        if prop_m is None:
            break
        prop_rep = prop_m.group(1)
        prop_repval = local_prop.get(prop_rep, None)
        if prop_repval is None:
            prop_repval = os.getenv(prop_rep)
            if prop_repval is None:
                error('Property "{}" not found in "{}" and also not in the environment'.format(prop_rep, local_prop_file))
        prop_val = str.replace(prop_val, '$(' + prop_rep + ')', prop_repval)
    return prop_val
rundir = os.path.dirname(sys.argv[0]) or '.'
if rundir == '':
    rundir = '.'
local_prop_file = rundir + '\\' + local_prop_file
read_props(local_prop_file)
if len(sys.argv) < 2:
    error('Missing argument')
pyargs = ''
if sys.argv[1] and sys.argv[1][0] == '-':
    pyargs = sys.argv.pop(1)
if len(sys.argv) < 2:
    error('Missing argument')
outdir = rundir + '\\' + sys.argv.pop(1)
if not os.path.isdir(outdir):
    error('Directory "{}" doesn\'t exist'.format(outdir))
m = re.search('(.*)\\\\(.*)$', outdir)
if not m or len(m.groups()) != 2:
    error('Invalid output directory "{}"'.format(outdir))
config = m.group(1)
pydir = m.group(2).upper()
pyexe = get_prop(pydir + '_EXE')
if len(sys.argv) == 2:
    if sys.argv[1] == '' or sys.argv[1][0] != '-':
        pyscript = sys.argv.pop(1)
if pyscript != '':
    if '\\' not in pyscript:
        pyscript = rundir + '\\' + scriptdir + '\\' + pyscript
args = ''
if len(sys.argv) >= 2:
    args = ' '.join(sys.argv[2:])
path = os.environ['PATH']
dllpath = get_prop('LG_DLLPATH')
os.environ['PATH'] = '{};{};{}'.format(config, dllpath, path)
os.environ['PYTHONPATH'] = rundir + '\\' + '..\\bindings\\python;{}'.format(outdir)
print('PYTHONPATH=' + os.environ['PYTHONPATH'])
cmd = ' '.join((pyexe, pyargs, pyscript, args))
print('Issuing command:', cmd)
os.system(cmd)