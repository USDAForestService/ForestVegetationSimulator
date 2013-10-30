"""
SCons tool for compiling a Python extension with F2PY

This more or less mimics the approach used by distutils, but in the SCons 
build environment.
"""

##FIXME:  This is a bit overly complex.  There may be a simpler approach using
##        distutils directly

import os
import sys
import re
import shutil
import subprocess

import numpy
import numpy.f2py as f2py
import distutils.sysconfig as dist
import SCons.Builder

#suffixes of files that F2PY will generate as needed
CMODULE = 'module.c'
FWRAPPER = '-f2pywrappers.f'
F90WRAPPER = '-f2pywrappers2.f90'
FORTRANOBJECT = 'fortranobject.c'

def gen_signature(target, source, env):
    """
    Return a F2PY module signature file for the given source files
    """
    cmdargs = ['-h' , str(target[0]), '--overwrite-signature']
    cmdargs.extend([str(src) for src in source])
    result = f2py.run_main(cmdargs)
    return None

def emit_f2py_targets(target, source, env):
    """
    Return a list of all the F2PY files for SCons to track
    """
    #include all possible wrapper files regardless of whether they are used
    ext_name = os.path.splitext(os.path.basename(str(target[0])))[0]
    targets = []
    targets.append('%s%s' % (ext_name, CMODULE))
    targets.append('%s%s' % (ext_name, FWRAPPER))
    targets.append('%s%s' % (ext_name, F90WRAPPER))
    targets.append(FORTRANOBJECT)

    return (targets, source)

##FIXME: Borrowed from NumSCons, need credits/license, replaced
def f2py_cmd_exec(cmd):
    """
    Executes a f2py command.

    Execute f2py in a new process instead of using f2py.main to
    avoid race issues when using multiple jobs with scons.

    Args
    ----
    cmd - An list/tuple of F2PY command line arguments
    """
    f2py_cmd = [sys.executable, '-c',
                ('"from numpy.f2py.f2py2e import run_main;'
                 , 'run_main({})"'.format(repr(cmd))
                 )]
    p = subprocess.Popen(f2py_cmd, stdout=subprocess.PIPE)
    for i in p.stdout.readlines():
        print i.rstrip('\n')
    return p.wait()

def gen_wrapper(target, source, env):
    """
    Generate the wrapper source files
    """
    pyext_name = env['F2PY_MODULE_NAME']
    #pyext_name = os.path.splitext(os.path.split(str(target[0]))[-1])[0]

    ##TODO: subroutine/function names should be configuration option
    skip_names = env['F2PY_SKIP_NAMES']
    only_names = env['F2PY_ONLY_NAMES']

    #f2py.run_main expects a list of command line arguments as
    #with the command line tool

    cmdargs = ['--include-paths', ';'.join(env['CPPPATH'])
            , '--lower', '-m', pyext_name, ]
    cmdargs.extend(['--build-dir', env['BUILD_DIR']])
    if skip_names:
        cmdargs.extend(['skip:'] + [n for n in skip_names])

    if only_names:
        cmdargs.extend(['only:'] + [n for n in only_names])

    cmdargs.extend([':'] + [s.abspath for s in source])

#    print cmdargs

    #execute the F2PY command
    f2py_cmd_exec(cmdargs)

    # ensure the wrapper files are available even if they are empty
    fwrapper = os.path.join(env['BUILD_DIR'], '{}{}'.format(pyext_name, FWRAPPER))
    if not os.path.exists(fwrapper):
        open(fwrapper, 'w').close()

    f90wrapper = os.path.join(env['BUILD_DIR'], '%s%s' % (pyext_name, F90WRAPPER))
    if not os.path.exists(f90wrapper):
        open(f90wrapper, 'w').close()

    # copy the fortanobject file locally
    fn = os.path.split(env['F2PY_OBJECTFILE'])[-1]
    fp = os.path.join(env['BUILD_DIR'], fn)
    shutil.copy(env['F2PY_OBJECTFILE'], fp)

    return None

_signature_builder = SCons.Builder.Builder(
        action=gen_signature
        , suffix='$F2PY_SIGSUFFIX'
        )

_wrapper_builder = SCons.Builder.Builder(
        action=gen_wrapper
        , emitter=emit_f2py_targets
        )

# _pyext_builder = SCons.Builder.Builder(
        # action=build_pyext
        # ,suffix='$F2PY_LIBSUFFIX'
        # )

def gen_f2py_signature(env, module_name, wrap_sources, *args, **kargs):
    sigfile = module_name + env.subst('$F2PY_SIGSUFFIX')
    sigfile = _signature_builder.__call__(env, sigfile, wrap_sources, **kargs)

    return sigfile

def gen_f2py_wrapper(env, module_name, wrap_sources, link_libs=[], *args, **kargs):
    for lib in link_libs:
        env['F2PY_LINKLIBS'].append(lib)

    env['F2PY_MODULE_NAME'] = module_name

    # wrappers = gen_wrapper(module_name,wrap_sources,env)
    wrappers = _wrapper_builder.__call__(env, module_name, wrap_sources, **kargs)
    return wrappers

def gen_f2py_pylib(env, lib_name, wrappers
                   , link_objs=[], link_libs=[]
                   , *args, **kargs):
    """
    Generate the python extension as a SCons shared library
    """
    ##TODO: is there a way to extract compiler and linker flags from distutils
    wrapper_objs = env.SharedObject(
            source=wrappers
            , CPPPATH=env['CPPPATH'] + env['F2PY_INCLUDE']
            , CCFLAGS=env['CCFLAGS']
            )

    if not hasattr(link_libs, '__iter__'):
        link_libs = [link_libs, ]
    else:
        link_libs = list(link_libs)

    pyext_lib = env.SharedLibrary(
        target=lib_name
        , SHLIBSUFFIX=env['F2PY_LIBSUFFIX']
        , SHLIBPREFIX='py'
        , source=wrapper_objs + link_objs
        , LIBS=env['F2PY_PYTHONLIB'] + link_libs

        , LIBPATH=env['F2PY_LIBDIR'] + [env['BUILD_DIR'], ]
        , CPPPATH=env['CPPPATH'] + env['F2PY_INCLUDE'] + [env['BUILD_DIR'], ]
        , chdir=env['ROOT_DIR']
        )

    env.Depends(pyext_lib, wrapper_objs)

#    open(env['BUILD_DIR'] + '/dumpenv.txt', 'w').write(env.Dump())

    return pyext_lib

def generate(env):
    vars = dist.get_config_vars()
    libdir = os.path.join(vars['BINDIR'], 'libs')
    env.SetDefault(
        F2PY_SIGSUFFIX='.pyf'
        , F2PY_LIBSUFFIX=vars['SO']
        , F2PY_INCLUDE=[
                vars['INCLUDEPY']
                , numpy.get_include()
                , os.path.join(os.path.split(f2py.__file__)[0], 'src')
                ]
        , F2PY_OBJECTFILE=os.path.join(os.path.split(f2py.__file__)[0], 'src', FORTRANOBJECT)
        , F2PY_LIBDIR=[libdir, ]
        , F2PY_PYTHONLIB=['python' + vars['VERSION'], ]
        , F2PY_LINKLIBS=[]
        , F2PY_ONLY_NAMES=[]
        , F2PY_SKIP_NAMES=[]
        , F2PY_MODULE_NAME=''
        )

    env.AddMethod(gen_f2py_signature, "F2PYSignature")
    env.AddMethod(gen_f2py_wrapper, "F2PYWrapper")
    env.AddMethod(gen_f2py_pylib, "F2PYLibrary")

def exists():
    try:
        ##TODO: test for numpy, distutils, etc.
        return 1
    except:
        return 0
