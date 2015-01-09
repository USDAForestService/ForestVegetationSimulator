"""
Python script to generate F2PY wrappers for a set of Fortran sources.

This script originates as a template that is populated by the CMake command
    `configure_file`.

@author: Tod Haren tod.haren@gmail.com
"""

import os
import shutil

from numpy.f2py import f2py2e

# These are the file names F2PY may generate depending on the source
wrappers = {
    'cmodule':'${pymod_name}module.c'
    , 'fwrapper':'${pymod_name}-f2pywrappers.f'
    , 'f90wrapper':'${pymod_name}-f2pywrappers2.f90'
    , 'fortranobject':'fortranobject.c'
    }

build_dir = '${CMAKE_CURRENT_BINARY_DIR}/f2py'
try:
    os.makedirs(build_dir)
except:
    pass

pyf_fn = '{}/${pymod_sig}'.format(build_dir)

# Generate the PYF signature file from source files
# f2py.run_main takes a list of arguments identical to the command line options
args = ['-h', pyf_fn
        , '-m', '${pymod_name}'
        , '--overwrite-signature'
        , '--build-dir', build_dir
        , '--include-paths']

# TODO: This may not be true for OSX
if os.name == 'nt':
    args.append('${include_dirs}')
else:
    # F2PY expects a colon separated list, not semicolon
    args = args.append(':'.join('${include_dirs}'.split(';')))

pymod_skip = '${pymod_skip}'.split(';')
if pymod_skip[0] != '':
    args.append('skip:')
    args.extend(pymod_skip)
    args.append(':')

args.extend('${pymod_source}'.split(';'))

r = f2py2e.run_main(args)

# Generate the wrapper code from the signature file
args = ['--build-dir', build_dir, pyf_fn]
r = f2py2e.run_main(args)

# Copy the fortranobject files to the build_dir
fortran_files = [
        [p for p in r['${pymod_name}']['csrc'] if p.endswith('fortranobject.c')][0]
        , [p for p in r['${pymod_name}']['h'] if p.endswith('fortranobject.h')][0]
        ]

for fortran_file in fortran_files:
    fn = os.path.split(fortran_file)[-1]
    dest = os.path.join(build_dir, fn)

    try:
        shutil.copy2(fortran_file, dest)
    except:
        IOError('Unable to copy {}, this is likely due to a bad Numpy installation.'.format(fn))
        raise

gen_files = [f for f in os.listdir(build_dir) if os.path.splitext(f)[-1] in ('.c', '.f', '.f90')]

# Ensure all expected files are present, even if empty
for fn in wrappers.values():
    if not fn in gen_files:
        open(os.path.join(build_dir, fn), 'w')

# Cmake captures the return values from stdout
print ';'.join(os.path.join(build_dir, fn).replace('\\','/') for fn in gen_files)
