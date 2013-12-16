This readme describes the procedure to build one or more FVS variants using
cmake and the included CMakeLists.txt file.  Currently this only applies to 
the PyFVS branch, but may soon be included in others.

http://www.cmake.org/

CMake typically does not build (ie. execute a compiler) software.  CMake 
takes care of the complicated task of setting up a build environment.  It is
more-or-less platform, language, and compiler agnostic.  It can generate 
projects for "make", both GNU and MinGW, Eclipse, Visual Studio, and more. 
Open-FVS uses CMake to automate the build configuration process.  This allows
users on different platforms to collaborate without too much effort devoted to
platform specific build configurations.

CMake has it's own mini language that is used to process a source tree and 
construct a build environment.  The primary set of instructions for 
initializing a project comes from the CMakeLists.txt.  This file is typically
located in the root folder of a source code folder tree.  Subfolders within
the tree can contain additional CMakeLists.txt files to control subprojects,
etc.  Additional instructions can be imported from a variety of other CMake 
files as well.  Ultimately, CMake is capable of handling a variety of 
complicated project structures and makes cross platform compiling less dauting.

Building Open-FVS

The following examples assume the user will be using mingw32-make as the 
build tool.  However, the process will be similar for Visual Studio users.
Linux users need only to replace references to MinGW and mingw32-make 
with GNU and make respectively if they are using the standard GNU/GCC 
toolchain.  All examples assume the "bin" folder of a the Open-FVS project 
folder is the root of the build tree.

The included CMakeLists.txt file is configured for a hierarchical build. The
Libraries common to all variants are built only once in the top level folder. 
Each configured variant is then built in it's own subfolder. There are a number
of options for modifying the Open-FVS build configuration, and all CMake 
variables can be modified at configuration time from the command line.  
Compiled binaries will be located within a folder called 'bin' in the project
build folder.

Example 1, A debug build:
    mkdir bin\debug
    cd bin\debug
    cmake .. -G"MinGW Makefiles" -DDEBUG=1 -DFVS_VARIANTS="pnc;wcc"
    mingw32-make -j2 all
    mingw32-make install

Description:
Assuming you are in the root of the Open-FVS source tree (for the branch 
you are working on) create a folder to generate the build project in and
move to the new folder.  Call on CMake to generate a MinGW Makefile build
project using the CMakeLists.txt file located one level up. Include DEBUG
information in all compiled binaries.  Note that -DDEBUG=1 is a simply a 
shortcut to -DCMAKE_BUILD_TYPE=DEBUG.  Configure the build for the PNC and 
WCC FVS variants.  These variants will be added as sub-projects within
the top level project.  Finally, call mingw32-make to build the default 
target for the toplevel project, and use two processor cores. The default
target is "all" and will generate binaries for both variants. By default
the generated binaries are placed in a folder called bin within the 
toplevel build folder, eg. debug/bin. Finish off by installing the compiled 
binaries to the system default location.  

Nothing prevents you from maintaining parallel build environments. For instance
you could create a separate folder for a release build, or individual folders
for each variant.  However, it is not necessary to maintain seperate build 
environments for each variant.  The project will be structured so that each 
variant is compiled in its own out-of-source subfolder.

Example 2, Configure all variants for a release build, build one at a time:
    mkdir bin\release
    cd bin\release
    cmake .. -G"MinGW Makefiles" -DRELEASE=1
    mingw32-make -j2 soc
    mingw32-make -j2 ncc
    
This creates subprojects for each variant in the FVS*_sourceList.txt file set.
Release optimization flags are set on the compiler calls.  Finally, only the 
SOC and NCC variants are built. 

Example 3, configure and build each variant separately:
    mkdir pnc_only
    cd pnc_only
    cmake .. -G"MinGW Makefiles" -DFVS_VARIANTS=pnc
    mingw32-make -j2
    cd ..
    mkdir wcc_only
    cd wcc_only
    cmake .. -G"MinGW Makefiles" -DFVS_VARIANTS=wcc
    mingw32-make -j2

Starting in the "bin" folder of the Open-FVS source tree this will build each 
variant in complete isolation.  This may be useful for debugging, but should be
discouraged for general use.

Configuration options specified within CMakeLists.txt:
    
    -DFVS_VARIANTS - Configure only selected FVS variants to build.  Use a 
            semicolon seperated list to select which variants will be 
            configured, eg. pnc;wcc;soc;ncc.  The default is "all" which will
            scan the FVS*_sourceLists.txt files for available variants.
    
    -DWITH_PYMOD=ON - Configure each selected variant so a linked Python 
            module will be built.  The resulting module file will be named 
            according to the variant, prefixed with 'py', and with a platform
            specific file extension, '.pyd' on Windows and '.so' on others.
    
    -DDEBUG=ON - Enable debugging compiler flags.
    -DRELEASE=ON - Enable release optimization compiler flats

Useful CMake variables:

    -DCMAKE_INSTALL_PREFIX - Root folder for installing the compiled binaries.
    -DCMAKE_BUILD_TYPE - Enable alternative build modes.  See the CMak docs.
    