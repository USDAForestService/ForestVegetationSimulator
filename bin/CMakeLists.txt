cmake_minimum_required (VERSION 2.8)

if (CMAKE_GENERATOR MATCHES "Visual Studio 10")
  set (CMAKE_C_FLAGS "/DWIN32 /D_WINDOWS /W3 /Zm100" CACHE STRING "VS10 mod A" FORCE)
  set (CMAKE_CXX_FLAGS "/DWIN32 /D_WINDOWS /W3 /Zm100" CACHE STRING "VS10 mod B" FORCE)
  message(STATUS "Building FVS with VS10/Intel")
elseif (CMAKE_GENERATOR MATCHES "MinGW Makefiles")
  message(STATUS "Building FVS with MinGW")
elseif (CMAKE_GENERATOR MATCHES "Unix Makefiles")
  message(STATUS "Building FVS with Unix")
else ()
  message(STATUS "Unknown FVS toolchain; exiting")
  return()
endif()

#Process the optional  command line argument semicolon seperated list
#  eg. cmake <...> -DFVS_VARIANTS=pnc;wcc;iec
if (FVS_VARIANTS)
    set(tobuild "")
    foreach(var in ${FVS_VARIANTS})
        file(GLOB fn FVS${var}_sourceList.txt)
        list(APPEND tobuild ${fn})
    endforeach()
else()
    #Assumes all variant build folders are to be prepared
    file(GLOB tobuild FVS*_sourceList.txt)
endif()
    
list(SORT tobuild)
message(STATUS "tobuild = ${tobuild}")

set(dirsToProcess)
foreach (sourceList ${tobuild})

  get_filename_component (slfn ${sourceList} NAME CACHE)
  string(REPLACE "_sourceList.txt" "" prgName ${slfn})
  string(REPLACE "FVS" "" varName ${prgName})

  file(STRINGS ${sourceList}  sourceListStrings  NEWLINE_CONSUME)
  string(REPLACE "../" "../../" newsourceList ${sourceListStrings})

  file(STRINGS CMakeLists.txt cmakelistin NEWLINE_CONSUME)

  string(REGEX REPLACE ".*##DO NOT REMOVE THIS TAG##" ""
  cmakelistout ${cmakelistin})

  file(MAKE_DIRECTORY ${prgName}_CmakeDir)
  file(REMOVE ${prgName}_CmakeDir/CMakeLists.txt)
  file(REMOVE ${prgName}_CmakeDir/CMakeCache.txt)
  file(REMOVE ${prgName}_CmakeDir/${prgName}_sourceList.txt)
  file(REMOVE_RECURSE ${prgName}_CmakeDir/CMakeFiles)
  file(WRITE ${prgName}_CmakeDir/CMakeLists.txt ${cmakelistout})
  file(WRITE ${prgName}_CmakeDir/${prgName}_sourceList.txt ${newsourceList})
  list(APPEND dirsToProcess ${prgName}_CmakeDir)

  unset(prgName CACHE)
  unset(slfn CACHE)

endforeach(sourceList)

if (PYEXT)
    set(pyext_flag "-DPYEXT=1")
else()
    set(pyext_flag "")
endif()

foreach(toProcess ${dirsToProcess})
  execute_process(COMMAND cmake -G ${CMAKE_GENERATOR} ${pyext_flag} CMakeLists.txt WORKING_DIRECTORY ${toProcess})
endforeach(toProcess)

return()

##This text below this tag becomes the CMakeLists.txt file in the subdirectory
##DO NOT REMOVE THIS TAG##

cmake_minimum_required(VERSION 2.8)

project(FVS C CXX Fortran)

file(GLOB fn FVS*_sourceList.txt)
get_filename_component (sourceList ${fn} NAME CACHE)

##FIXME: The variant name is parsed from the source list filename
##          but it is available from the initial configuration loop.
string(REPLACE "_sourceList.txt" "" variant_exe ${sourceList})
string(REPLACE "FVS" "" variant ${variant_exe})
set(variant_lib "FVS_${variant}")
message(STATUS "sourceList = ${sourceList}")
message(STATUS "variant_exe= ${variant_exe}")
message(STATUS "variant_lib= ${variant_lib}")

set(include_dirs)
set(sql_source)
set(ffe_source)
set(fvs_source)
set(main_source)
set(pyext_source)

add_definitions(-DANSI -DWINDOWS -D_WINDLL -DCMPgcc)
link_libraries(odbc32)

##FIXME: need to use static gfortran and qmath libraries
set(CMAKE_LINKER_FLAGS "${CMAKE_LINKER_FLAGS} -static")  

file(STRINGS ${sourceList} file_list)
foreach (fn ${file_list})
    get_filename_component(filename ${fn} NAME CACHE)
    get_filename_component(pathname ${fn} PATH CACHE)
    get_filename_component(extname ${fn} EXT CACHE)
    
    if(PYEXT)
        #source files to be parsed by F2PY
        if (${filename} STREQUAL "apisubs.f" OR
            ${filename} STREQUAL "cmdline.f" OR
            ${filename} STREQUAL "filopn.f" OR
            ${filename} STREQUAL "fvs.f")
            list(APPEND pyext_source ${fn})
        endif()
    endif()
    
    if (${extname} STREQUAL ".h")
        list(APPEND include_dirs ${pathname})
    endif()
    
    if (${extname} STREQUAL ".F77")
        list(APPEND include_dirs ${pathname})
    endif()
    
    if (${extname} STREQUAL ".f")
        if (${filename} STREQUAL "main.f")
            list(APPEND main_source ${fn})
        else()
            list (APPEND fvs_source ${fn})
        endif()
    elseif(${extname} STREQUAL ".c" OR ${extname} STREQUAL ".cpp")
        if (filename STREQUAL mkdbsTypeDefs.c)
            add_executable(mkdbsTypeDefs ${fn})
            
        elseif(${filename} STREQUAL fvsSQL.c)
            list (APPEND sql_source ${fn})
        
        else()
            list (APPEND ffe_source ${fn})
        
        endif()
    endif()

    unset(filename CACHE)
    unset(pathname CACHE)
    unset(extname CACHE)
    
endforeach(fn)

list(REMOVE_DUPLICATES include_dirs)
list(APPEND include_dirs ${CMAKE_CURRENT_BINARY_DIR})

include_directories(BEFORE ${include_dirs})

set(dbstypedefs_f77 "${CMAKE_CURRENT_BINARY_DIR}/DBSTYPEDEFS.F77")

#Launch the executable that generates the DBSTYPEDEFS.F77 include file.
add_custom_command(
        OUTPUT ${dbstypedefs_f77}
        COMMAND mkdbsTypeDefs
        DEPENDS mkdbsTypeDefs)

#Dummy target to associate the DBSTYPEDEFS.F77 file with the FVS library
add_custom_target(
        gen_dbstypedefs
        DEPENDS ${dbstypedefs_f77})

add_library(FVSsql SHARED ${sql_source})
add_library(FVSfofem SHARED ${ffe_source})
add_library(${variant_lib} SHARED ${fvs_source})

#Require the DBSTYPEDEFS.F77 file for the FVS library
add_dependencies(${variant_lib} gen_dbstypedefs)

add_executable (${variant_exe} ${main_source})
target_link_libraries(${variant_exe} ${variant_lib})
target_link_libraries(${variant_lib} FVSfofem FVSsql)

set_target_properties(${variant_exe} PROPERTIES LINKER_LANGUAGE Fortran)

if(PYEXT)
    ##TODO: This works well, but the safer approach would be to use distutils
    ##      with a template setup.py script and avoid all the F2PY command 
    ##      line manipulation.
    find_package(PythonInterp)
    find_package(PythonLibs)
    
    set(pymod "pyfvs${variant}")
    set(pymod_sig "pyfvs${variant}.pyf")
    
    ##TODO: Autodiscover the platform specific Python library extension 
    if(WIN32)
        set(pymod_lib "pyfvspnc.pyd")
    elseif(APPLE)
        ##FIXME: I'm not sure if this is the extension used on OSX
        set(pymod_lib "pyfvspnc.dylib")
    else()
        set(pymod_lib "pyfvspnc.so")
    endif()
    
    if (PYTHON_EXECUTABLE)
        message(STATUS "Found Python: ${PYTHON_EXECUTABLE}")
    else()
        message(SEND_ERROR "Python could not be located. Make sure it is in the system PATH variable.")
    endif()

    ##FIXME: f2py.py is only relative to python_home on windows with a default install
    get_filename_component(python_home ${PYTHON_EXECUTABLE} PATH)
    find_file(F2PY "f2py.py" ${python_home})
    if (F2PY)
        message(STATUS "Found F2PY: ${F2PY}")
    else()
        message(SEND_ERROR "The F2PY script could not be located.")
    endif()
    
    #TODO: Refer to the compiler in F2PY terms
    if (CMAKE_GENERATOR MATCHES "Visual Studio 10")
        set(f2py_compiler "msvc")
        set(f2py_fcompiler "intelv")
    elseif (CMAKE_GENERATOR MATCHES "MinGW Makefiles")
        set(f2py_compiler "mingw32")
        set(f2py_fcompiler "gnu95")
    elseif (CMAKE_GENERATOR MATCHES "Unix Makefiles")
        set(f2py_compiler "unix")
        set(f2py_fcompiler "gnu95")
    else()
        message(SEND_ERROR "F2PY compilers are not specified for this generator.")
    endif()

    #use a custom command to keep track of the output files
    add_custom_command(
            OUTPUT ${pymod_sig} ${pymod_lib}
            COMMAND ${python} ${F2PY} -h ${pymod_sig} -m ${pymod} --overwrite-signature --include-paths "${include_dirs}" ${pyext_source}
            COMMAND ${python} ${F2PY} -c ${pymod_sig} -L${CMAKE_CURRENT_BINARY_DIR} -l${variant_lib} --compiler=${f2py_compiler} --fcompiler=${f2py_fcompiler} --build-dir ${CMAKE_CURRENT_BINARY_DIR}/f2py
            )
    add_custom_target(f2py_out ALL DEPENDS ${pymod_sig} ${pymod_lib})
    add_dependencies(f2py_out ${variant_lib} ${pyext_source})
    
    set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES ${CMAKE_CURRENT_BINARY_DIR}/f2py)
    
    install (FILES ${pymod_lib} DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/../bin)

endif()

install (TARGETS ${variant_exe} ${variant_lib} FVSfofem FVSsql DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/../bin)