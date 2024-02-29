## $Id$

## Towards using cmake! NCrookston July 24 2012

## command-line:
# cmake -G"Visual Studio 10 Win64" .

cmake_minimum_required (VERSION 2.8)

if (CMAKE_GENERATOR MATCHES "Visual Studio 10")
  set (CMAKE_C_FLAGS   "/D_WINDOWS /W3 /Zm100" CACHE STRING "VS10 mod A" FORCE)
  set (CMAKE_CXX_FLAGS "/D_WINDOWS /W3 /Zm100" CACHE STRING "VS10 mod B" FORCE)
  message(STATUS "Building FVS with VS10/Intel")
elseif (CMAKE_GENERATOR MATCHES "MinGW Makefiles")
  message(STATUS "Building FVS with MinGW")
elseif (CMAKE_GENERATOR MATCHES "Unix Makefiles")
  message(STATUS "Building FVS with Unix")
else ()
  message(STATUS "Unknown FVS toolchain; exiting")
  return()
endif()

## example 1: Single-variant GLOB: file(GLOB tobuild FVSiec_sourceList.txt)
## example 2: All-variant GLOB: file(GLOB tobuild FVS*_sourceList.txt)

#file(GLOB tobuild FVSbc_sourceList.txt)
#file(GLOB tobuild FVSon_sourceList.txt)
file(GLOB tobuild FVS*_sourceList.txt)

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

foreach(toProcess ${dirsToProcess})
  execute_process(COMMAND cmake -G ${CMAKE_GENERATOR} CMakeLists.txt WORKING_DIRECTORY ${toProcess})
endforeach(toProcess)

return()

##The text below this tag becomes the CMakeLists.txt file in the subdirectory
##DO NOT REMOVE THIS TAG##

##If edits are made, make them in the CMakeList.txt file in the parent directory.
cmake_minimum_required (VERSION 2.8)

if (CMAKE_GENERATOR MATCHES "Visual Studio 10")
  set (CMAKE_C_FLAGS   "/D_WINDOWS /W3 /Zm100" CACHE STRING "VS10 mod A" FORCE)
  set (CMAKE_CXX_FLAGS "/D_WINDOWS /W3 /Zm100" CACHE STRING "VS10 mod B" FORCE)
  message(STATUS "Building FVS variant with VS10/Intel")
elseif (CMAKE_GENERATOR MATCHES "MinGW Makefiles")
  message(STATUS "Building FVS variant with MinGW")
elseif (CMAKE_GENERATOR MATCHES "Unix Makefiles")
  message(STATUS "Building FVS variant with Unix")
else ()
  message(STATUS "Unknown toolchain; exiting")
  return()
endif()

project (FVS C CXX Fortran)

set (CMAKE_Fortran_Format FIXED)
set (CMAKE_VERBOSE_MAKEFILE ON)

file(GLOB sourceList FVS*_sourceList.txt)
get_filename_component (slfn ${sourceList} NAME CACHE)
string(REPLACE "_sourceList.txt" "" prgName ${slfn})
string(REPLACE "FVS" "" varName ${prgName})
message(STATUS "slfn = ${slfn} prgName= ${prgName}")

if (CMAKE_GENERATOR MATCHES "Visual Studio 10")
  add_definitions(-DANSI -DWINDOWS -D_WINDLL)
  elseif (CMAKE_GENERATOR MATCHES "MinGW Makefiles")
  add_definitions(-DANSI -DWINDOWS -D_WINDLL -DCMPgcc)
  elseif (CMAKE_GENERATOR MATCHES "Unix Makefiles")
  add_definitions(-DANSI -DCMPgcc) 
else()
endif()

set (OUTPUT_NAME ${prgName})
set (includeDirs)
set (CsourceSQL)
set (CsourceFFE)
#set (CsourceCFIM)
set (FsourceFVS)
set (FsourceMAIN)

file(STRINGS ${prgName}_sourceList.txt fileList)

foreach (fn ${fileList})
  get_filename_component (fname   ${fn} NAME CACHE)
  get_filename_component (pname   ${fn} PATH CACHE)
  get_filename_component (extname ${fn} EXT  CACHE)

  if (${extname} STREQUAL ".h" OR ${extname} STREQUAL ".F77")
    list (APPEND includeDirs  ${pname})
  endif()

  if (${extname} STREQUAL ".f" OR ${extname} STREQUAL ".F" OR ${extname} STREQUAL ".for" )
    if (${fname} STREQUAL "main.f")
      list (APPEND FsourceMAIN ${fn})
    else()
      list (APPEND FsourceFVS ${fn})
    endif()
  elseif(${extname} STREQUAL ".c" OR ${extname} STREQUAL ".cpp")
    if(fname STREQUAL "sqlite3.c" OR fname STREQUAL "fvsqlite3.c" OR fname STREQUAL "apisubsc.c")
      list (APPEND CsourceSQL ${fn})
    else()
      list (APPEND CsourceFFE ${fn})
    endif()
  endif()
  unset (fname   CACHE)
  unset (pname   CACHE)
  unset (extname CACHE)
endforeach(fn)

list (REMOVE_DUPLICATES includeDirs)
list (APPEND includeDirs ${CMAKE_CURRENT_BINARY_DIR})

include_directories(BEFORE ${includeDirs})

message(STATUS "includeDirs= ${includeDirs}")
message(STATUS "CsourceSQL= ${CsourceSQL}")
message(STATUS "CsourceFFE= ${CsourceFFE}")
message(STATUS "FsourceFVS= ${FsourceFVS}")
message(STATUS "FsourceMAIN= ${FsourceMAIN}")

message(STATUS "FVSvariant = FVS${varName}")

add_library(FVSsql SHARED ${CsourceSQL})
add_library(FVSfofem SHARED ${CsourceFFE})
#add_library(FVScfim SHARED ${CsourceCFIM})  ### delete this line permanently
add_library(FVS_${varName} SHARED ${FsourceFVS})
add_executable (${prgName} ${FsourceMAIN})

target_link_libraries(${prgName} FVS_${varName} FVSsql FVSfofem )
target_link_libraries(FVS_${varName} FVSsql FVSfofem )

set_target_properties(${prgName} PROPERTIES LINKER_LANGUAGE Fortran)

# set up function alias for DLL containing CPP code

if (CMAKE_GENERATOR MATCHES "MinGW Makefiles")
  set_target_properties(FVSfofem PROPERTIES LINK_FLAGS -Wl,--add-stdcall-alias)
  # set_target_properties(FVScfim PROPERTIES LINK_FLAGS -Wl,--add-stdcall-alias)
endif()
