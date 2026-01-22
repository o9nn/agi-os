from distutils.core import setup
from distutils.extension import Extension
from distutils.sysconfig import get_python_inc
from Cython.Distutils import build_ext
import os
opencog_library_dir = '/usr/local/lib/opencog'
def read(fname):
    return open(os.path.join(os.path.dirname(__file__), fname)).read()
incdir = os.path.join(get_python_inc(plat_specific=1), 'Numerical')
ext = Extension('cogserver', define_macros=[('MAJOR_VERSION', '0'), ('MINOR_VERSION', '1')], sources=['cogserver.pyx'], language='c++', include_dirs=['.', '../..', '/usr/local/include', '/opt/local/include'], libraries=['stdc++', 'atomspace', 'util'], library_dirs=['/opt/local/lib', opencog_library_dir], runtime_library_dirs=[opencog_library_dir])
setup(name='pyopencog', description='Python Cogserver', author='Joel Pitt', author_email='joel@opencog.org', url='http://wiki.opencog.org/w/Python', long_description=read('README'), version='0.1', classifiers=['Development Status :: 5 - Release', 'Topic :: Scientific/Engineering :: Artificial Intelligence', 'License :: OSI Approved :: GNU Affero General Public License v3'], cmdclass={'build_ext': build_ext}, ext_modules=[ext])