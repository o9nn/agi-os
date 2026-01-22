from subprocess import Popen, PIPE
from string import Template
import os
import sys
import logging
import argparse
import re
from collections import namedtuple
QTLIB_NAME_REGEX = '^(?:@executable_path)?/.*/(Qt[a-zA-Z]*).framework/(?:Versions/\\d/)?\\1$'
QTLIB_NORMALIZED = '$prefix/Frameworks/$qtlib.framework/Versions/$qtversion/$qtlib'
QTPLUGIN_NAME_REGEX = '^(?:@executable_path)?/.*/[pP]lug[iI]ns/(.*)/(.*).dylib$'
QTPLUGIN_NORMALIZED = '$prefix/PlugIns/$plugintype/$pluginname.dylib'
LOADERPATH_REGEX = '^@[a-z_]+path/(.*)'
LOADERPATH_NORMALIZED = '$prefix/Frameworks/$loaderpathlib'
class GlobalConfig(object):
    logger = None
    qtpath = None
    exepath = None
def run_and_get_output(popen_args):
    process_output = namedtuple('ProcessOutput', ['stdout', 'stderr', 'retcode'])
    try:
        GlobalConfig.logger.debug('run_and_get_output({0})'.format(repr(popen_args)))
        proc = Popen(popen_args, stdin=PIPE, stdout=PIPE, stderr=PIPE)
        stdout, stderr = proc.communicate(b'')
        proc_out = process_output(stdout, stderr, proc.returncode)
        GlobalConfig.logger.debug('\tprocess_output: {0}'.format(proc_out))
        return proc_out
    except Exception as exc:
        GlobalConfig.logger.error('\texception: {0}'.format(exc))
        return process_output('', exc.message, -1)
def get_dependencies(filename):
    GlobalConfig.logger.debug('get_dependencies({0})'.format(filename))
    popen_args = ['otool', '-L', filename]
    proc_out = run_and_get_output(popen_args)
    deps = []
    if proc_out.retcode == 0:
        deps = [s.strip().split(b' ')[0].decode('utf-8') for s in proc_out.stdout.splitlines()[1:] if s]
        deps = [s for s in deps if os.path.basename(filename) not in s]
    return deps
def is_qt_plugin(filename):
    qtlib_name_rgx = re.compile(QTPLUGIN_NAME_REGEX)
    return qtlib_name_rgx.match(filename) is not None
def is_qt_lib(filename):
    qtlib_name_rgx = re.compile(QTLIB_NAME_REGEX)
    return qtlib_name_rgx.match(filename) is not None
def is_loader_path_lib(filename):
    qtlib_name_rgx = re.compile(LOADERPATH_REGEX)
    return qtlib_name_rgx.match(filename) is not None
def normalize_qtplugin_name(filename):
    GlobalConfig.logger.debug('normalize_plugin_name({0})'.format(filename))
    qtplugin_name_rgx = re.compile(QTPLUGIN_NAME_REGEX)
    rgxret = qtplugin_name_rgx.match(filename)
    if not rgxret:
        msg = "couldn't normalize a non-qt plugin filename: {0}".format(filename)
        GlobalConfig.logger.critical(msg)
        raise Exception(msg)
    qtplugintype = rgxret.groups()[0]
    qtpluginname = rgxret.groups()[1]
    templ = Template(QTPLUGIN_NORMALIZED)
    abspath = os.path.normpath(templ.safe_substitute(prefix=os.path.dirname(GlobalConfig.exepath) + '/..', plugintype=qtplugintype, pluginname=qtpluginname))
    rpath = templ.safe_substitute(prefix='@executable_path/..', plugintype=qtplugintype, pluginname=qtpluginname)
    GlobalConfig.logger.debug('\treturns({0})'.format((qtpluginname, abspath, rpath)))
    return (qtpluginname, abspath, rpath)
def normalize_qtlib_name(filename):
    GlobalConfig.logger.debug('normalize_qtlib_name({0})'.format(filename))
    qtlib_name_rgx = re.compile(QTLIB_NAME_REGEX)
    rgxret = qtlib_name_rgx.match(filename)
    if not rgxret:
        msg = "couldn't normalize a non-qt lib filename: {0}".format(filename)
        GlobalConfig.logger.critical(msg)
        raise Exception(msg)
    qtlib = rgxret.groups()[0]
    qtversion = 5
    templ = Template(QTLIB_NORMALIZED)
    abspath = os.path.normpath(templ.safe_substitute(prefix=os.path.dirname(GlobalConfig.exepath) + '/..', qtlib=qtlib, qtversion=qtversion))
    rpath = templ.safe_substitute(prefix='@executable_path/..', qtlib=qtlib, qtversion=qtversion)
    GlobalConfig.logger.debug('\treturns({0})'.format((qtlib, abspath, rpath)))
    return (qtlib, abspath, rpath)
def normalize_loaderpath_name(filename):
    GlobalConfig.logger.debug('normalize_loaderpath_name({0})'.format(filename))
    loaderpath_name_rgx = re.compile(LOADERPATH_REGEX)
    rgxret = loaderpath_name_rgx.match(filename)
    if not rgxret:
        msg = "couldn't normalize a loaderpath lib filename: {0}".format(filename)
        GlobalConfig.logger.critical(msg)
        raise Exception(msg)
    loaderpathlib = rgxret.groups()[0]
    templ = Template(LOADERPATH_NORMALIZED)
    abspath = os.path.normpath(templ.safe_substitute(prefix=os.path.dirname(GlobalConfig.exepath) + '/..', loaderpathlib=loaderpathlib))
    rpath = templ.safe_substitute(prefix='@executable_path/..', loaderpathlib=loaderpathlib)
    GlobalConfig.logger.debug('\treturns({0})'.format((loaderpathlib, abspath, rpath)))
    return (loaderpathlib, abspath, rpath)
def fix_dependency(binary, dep):
    if is_qt_lib(dep):
        qtname, dep_abspath, dep_rpath = normalize_qtlib_name(dep)
        qtnamesrc = os.path.join(GlobalConfig.qtpath, 'lib', '{0}.framework'.format(qtname), qtname)
    elif is_qt_plugin(dep):
        qtname, dep_abspath, dep_rpath = normalize_qtplugin_name(dep)
        qtnamesrc = os.path.join(GlobalConfig.qtpath, 'lib', '{0}.framework'.format(qtname), qtname)
    elif is_loader_path_lib(dep):
        qtname, dep_abspath, dep_rpath = normalize_loaderpath_name(dep)
        qtnamesrc = os.path.join(GlobalConfig.qtpath + '/lib', qtname)
    else:
        return True
    if not os.path.exists(qtnamesrc):
        return True
    dep_ok = True
    if dep != dep_rpath:
        GlobalConfig.logger.info("changing rpath '{0}' in binary {1}".format(dep, binary))
        popen_args = ['install_name_tool', '-change', dep, dep_rpath, binary]
        proc_out = run_and_get_output(popen_args)
        if proc_out.retcode != 0:
            GlobalConfig.logger.error(proc_out.stderr)
            dep_ok = False
        else:
            popen_args = ['install_name_tool', '-id', dep_rpath, binary]
            proc_out = run_and_get_output(popen_args)
            if proc_out.retcode != 0:
                GlobalConfig.logger.error(proc_out.stderr)
                dep_ok = False
    if dep_ok and (not os.path.exists(dep_abspath)):
        GlobalConfig.logger.info("ensuring directory '{0}' exists: {0}".format(os.path.dirname(dep_abspath)))
        popen_args = ['mkdir', '-p', os.path.dirname(dep_abspath)]
        proc_out = run_and_get_output(popen_args)
        if proc_out.retcode != 0:
            GlobalConfig.logger.info(proc_out.stderr)
            dep_ok = False
        else:
            GlobalConfig.logger.info('copying missing dependency in bundle: {0}'.format(qtname))
            popen_args = ['cp', qtnamesrc, dep_abspath]
            proc_out = run_and_get_output(popen_args)
            if proc_out.retcode != 0:
                GlobalConfig.logger.info(proc_out.stderr)
                dep_ok = False
            else:
                GlobalConfig.logger.info('ensuring 755 perm to {0}'.format(dep_abspath))
                popen_args = ['chmod', '755', dep_abspath]
                proc_out = run_and_get_output(popen_args)
                if proc_out.retcode != 0:
                    GlobalConfig.logger.info(proc_out.stderr)
                    dep_ok = False
    else:
        GlobalConfig.logger.debug('{0} is at correct location in bundle'.format(qtname))
    if dep_ok:
        return fix_binary(dep_abspath)
    return False
def fix_binary(binary):
    GlobalConfig.logger.debug('fix_binary({0})'.format(binary))
    for dep in get_dependencies(binary):
        if not fix_dependency(binary, dep):
            GlobalConfig.logger.error("quitting early: couldn't fix dependency {0} of {1}".format(dep, binary))
            return False
    return True
def fix_main_binaries():
    bundlepath = os.path.sep.join(GlobalConfig.exepath.split(os.path.sep)[0:-3])
    GlobalConfig.logger.info("fixing executable '{0}'".format(GlobalConfig.exepath))
    if fix_binary(GlobalConfig.exepath):
        GlobalConfig.logger.info('fixing plugins')
        for root, dummy, files in os.walk(bundlepath):
            for name in [f for f in files if os.path.splitext(f)[1] == '.dylib']:
                GlobalConfig.logger.info('fixing plugin {0}'.format(name))
                if not fix_binary(os.path.join(root, name)):
                    return False
    return True
def main():
    descr = 'finish the job started by macdeployqt!\n - find dependencies/rpaths with otool\n - copy missed dependencies with cp and mkdir\n - fix missed rpaths        with install_name_tool\n\n exit codes:\n - 0 : success\n - 1 : error\n '
    parser = argparse.ArgumentParser(description=descr, formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument('exepath', help='path to the binary depending on Qt')
    parser.add_argument('qtpath', help='path of Qt libraries used to build the Qt application')
    parser.add_argument('-q', '--quiet', action='store_true', default=False, help='do not create log on standard output')
    parser.add_argument('-nl', '--no-log-file', action='store_true', default=False, help="do not create log file './macdeployqtfix.log'")
    parser.add_argument('-v', '--verbose', action='store_true', default=False, help='produce more log messages(debug log)')
    args = parser.parse_args()
    GlobalConfig.qtpath = os.path.normpath(args.qtpath)
    GlobalConfig.exepath = args.exepath
    GlobalConfig.logger = logging.getLogger()
    formatter = logging.Formatter('%(levelname)s | %(message)s')
    if not args.quiet:
        chdlr = logging.StreamHandler(sys.stdout)
        chdlr.setFormatter(formatter)
        GlobalConfig.logger.addHandler(chdlr)
    if not args.no_log_file:
        fhdlr = logging.FileHandler('./macdeployqtfix.log', mode='w')
        fhdlr.setFormatter(formatter)
        GlobalConfig.logger.addHandler(fhdlr)
    if args.no_log_file and args.quiet:
        GlobalConfig.logger.addHandler(logging.NullHandler())
    else:
        GlobalConfig.logger.setLevel(logging.DEBUG if args.verbose else logging.INFO)
    if fix_main_binaries():
        GlobalConfig.logger.info('macdeployqtfix terminated with success')
        ret = 0
    else:
        GlobalConfig.logger.error('macdeployqtfix terminated with error')
        ret = 1
    sys.exit(ret)
if __name__ == '__main__':
    main()