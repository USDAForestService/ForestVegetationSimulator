## $Id$

## Towards using cmake! NCrookston July 24 2012

cmake_minimum_required (VERSION 2.6) 

file(GLOB tobuild FVS*_sourceList.txt)
list(SORT tobuild)
message(STATUS "tobuild = ${tobuild}")

set(dirsToProcess)

foreach (sourceList ${tobuild})
 
  get_filename_component (slfn ${sourceList} NAME CACHE)
  string(REPLACE "_sourceList.txt" "" prgName ${slfn})
  message(STATUS "sourceList=" ${sourceList} "top slfn = ${slfn} prgName= ${prgName}")

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
  execute_process(COMMAND cmake CMakeLists.txt WORKING_DIRECTORY ${toProcess})
endforeach(toProcess)

return()

##This text below this tag becomes the CMakeLists.txt file in the subdirectory 
##DO NOT REMOVE THIS TAG##

##See the CMakeList.txt file in the parent directory.
cmake_minimum_required (VERSION 2.6) 
project (FVS C Fortran)

set (CMAKE_Fortran_Format FIXED)
set (CMAKE_VERBOSE_MAKEFILE ON)

file(GLOB sourceList FVS*_sourceList.txt)
get_filename_component (slfn ${sourceList} NAME CACHE)
string(REPLACE "_sourceList.txt" "" prgName ${slfn})
message(STATUS "slfn = ${slfn} prgName= ${prgName}")

add_definitions(-DANSI)
if(${CMAKE_HOST_SYSTEM_NAME} MATCHES "Windows")
  link_libraries(odbc32)
  add_definitions(-DWINDOWS)
else()  
  link_libraries(odbc)
  add_definitions(-DCMPgcc)
endif()

  
set (OUTPUT_NAME ${prgName})
set (includeDirs)
set (sourceFiles)
  
file(STRINGS ${prgName}_sourceList.txt fileList)

foreach (fn ${fileList})  
  get_filename_component (fname   ${fn} NAME CACHE)
  get_filename_component (pname   ${fn} PATH CACHE)
  get_filename_component (extname ${fn} EXT  CACHE)  
  if (${extname} STREQUAL ".F77" OR ${extname} MATCHES ".h")
    list (APPEND includeDirs  ${pname})
  endif()
  if (${extname} STREQUAL ".f")
    list (APPEND sourceFiles ${fn})
  elseif(${extname} STREQUAL ".c")   
    if (fname STREQUAL mkdbsTypeDefs.c)
      add_executable(mkdbsTypeDefs ${fn})
      get_target_property(MKDBSTYPEDEFS_EXE mkdbsTypeDefs LOCATION)
      add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/DBSTYPEDEFS.F77 
                         COMMAND ${MKDBSTYPEDEFS_EXE} 
                         DEPENDS ${MKDBSTYPEDEFS_EXE})
      add_custom_target(dbstypedefs mkdbsTypeDefs)
    else()                                       
      list (APPEND sourceFiles ${fn})
    endif()
  endif()
  unset (fname   CACHE)
  unset (pname   CACHE)
  unset (extname CACHE)     
endforeach(fn)

list (REMOVE_DUPLICATES includeDirs)
list (APPEND includeDirs ${CMAKE_CURRENT_BINARY_DIR})
message(STATUS "prgName= ${prgName} includeDirs = ${includeDirs}")
include_directories(BEFORE ${includeDirs})
get_directory_property(includeSoFar INCLUDE_DIRECTORIES)
message(STATUS "includeSoFar= ${includeSoFar}")

add_executable (${prgName} ${sourceFiles})
add_dependencies(${prgName} dbstypedefs)

set_target_properties(${prgName} PROPERTIES LINKER_LANGUAGE Fortran)

