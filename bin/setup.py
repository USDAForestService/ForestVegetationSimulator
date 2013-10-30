"""
A Python distutils setup script for building a F2PY wrapper of a FVS library.
"""

import os
from numpy.distutils.core import setup, Extension

#TODO: these need to be config arguments to setup.py
variant = 'pnc'
build_dir = './pnc/debug/bin'  #location of the FVS library file
source_root = '..'  #using the build copy of the sources in this case

lib_name = 'libFVS_{}'.format(variant)

#Paths to find the .F77 and other 'include "xxx"' source files
include_dirs = ['{}/pn/common'.format(source_root)
                , '{}/common'.format(source_root)
                , '{}/fire/base/common'.format(source_root)
                ]

fvs_wrapper = Extension('pyfvs{}'.format(variant)
        , sources=[
                '{}/base/src/fvs.f'.format(source_root)
                , '{}/base/src/apisubs.f'.format(source_root)
                , '{}/base/src/cmdline.f'.format(source_root)
                , '{}/base/src/filopn.f'.format(source_root)
                ]
        , include_dirs=include_dirs
        , library_dirs=[build_dir, ]
        , libraries=[lib_name, ]
        , f2py_options=['--include-paths', ';'.join(include_dirs)]
        )

setup(name='PyFVS {}'.format(variant.upper())
        , version='0.0'
        , description='Python wrapper for the FVS shared library'
        , author='Tod Haren'
        , author_email='tod.haren@gmail.com'
        , url='https://code.google.com/p/open-fvs/'
        , ext_modules=[fvs_wrapper]
        )
