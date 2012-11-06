## $Id$

## Towards using cmake! NCrookston July 24 2012

cmake_minimum_required (VERSION 2.6) 

# set Visual Studio flags
set (CMAKE_C_FLAGS "/DWIN32 /D_WINDOWS /W3 /Zm100" CACHE STRING "VS10 mod" FORCE)

file(GLOB tobuild FVSwsc*_sourceList.txt)
#file(GLOB tobuild FVS*_sourceList.txt)

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
  execute_process(COMMAND cmake CMakeLists.txt WORKING_DIRECTORY ${toProcess})
endforeach(toProcess)

return()

##This text below this tag becomes the CMakeLists.txt file in the subdirectory 
##DO NOT REMOVE THIS TAG##

##If edits are made, make them in the CMakeList.txt file in the parent directory.
cmake_minimum_required (VERSION 2.6) 

# set Visual Studio flags
set (CMAKE_C_FLAGS "/DWIN32 /D_WINDOWS /W3 /Zm100" CACHE STRING "VS10 mod" FORCE)

project (FVS C Fortran)

set (CMAKE_Fortran_Format FIXED)
set (CMAKE_VERBOSE_MAKEFILE ON)

file(GLOB sourceList FVS*_sourceList.txt)
get_filename_component (slfn ${sourceList} NAME CACHE)
string(REPLACE "_sourceList.txt" "" prgName ${slfn})
string(REPLACE "FVS" "" varName ${prgName})
message(STATUS "slfn = ${slfn} prgName= ${prgName}")

add_definitions(-DANSI)
if(${CMAKE_HOST_SYSTEM_NAME} MATCHES "Windows")
  link_libraries(odbc32)
  add_definitions(-DWINDOWS -D_WINDLL)
else()  
  link_libraries(odbc)
  add_definitions(-DCMPgcc)
endif()

  
set (OUTPUT_NAME ${prgName})
set (includeDirs)
set (CsourceSQL)
set (CsourceFFE)
set (FsourceFVS)
set (FsourceMAIN)

  
file(STRINGS ${prgName}_sourceList.txt fileList)

foreach (fn ${fileList})  
  get_filename_component (fname   ${fn} NAME CACHE)
  get_filename_component (pname   ${fn} PATH CACHE)
  get_filename_component (extname ${fn} EXT  CACHE)  

  if (${extname} STREQUAL ".h") 
    list (APPEND includeDirs  ${pname})
  endif()
  if (${extname} STREQUAL ".F77")
    list (APPEND includeDirs  ${pname})
  endif()

  if (${extname} STREQUAL ".f")
    if (${fname} STREQUAL "main.f")
      list (APPEND FsourceMAIN ${fn})
    else()
      list (APPEND FsourceFVS ${fn})
    endif()
  elseif(${extname} STREQUAL ".c")   
    if (fname STREQUAL mkdbsTypeDefs.c)
      add_executable(mkdbsTypeDefs ${fn})
      get_target_property(MKDBSTYPEDEFS_EXE mkdbsTypeDefs LOCATION)
      add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/DBSTYPEDEFS.F77 
                         COMMAND ${MKDBSTYPEDEFS_EXE} 
                         DEPENDS ${MKDBSTYPEDEFS_EXE})
      add_custom_target(dbstypedefs mkdbsTypeDefs)
    elseif(fname STREQUAL fvsSQL.c)                                       
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

#message(STATUS "includeDirs= ${includeDirs}")
#message(STATUS "CsourceSQL= ${CsourceSQL}")
#message(STATUS "CsourceFFE= ${CsourceFFE}")
#message(STATUS "FsourceFVS= ${FsourceFVS}")
#message(STATUS "FsourceMAIN= ${FsourceMAIN}")

message(STATUS "FVSvariant = FVS${varName}")

add_library(FVSsql SHARED ${CsourceSQL})
add_library(FVSfofem SHARED ${CsourceFFE})
add_library(FVS_${varName} SHARED ${FsourceFVS}) 
add_executable (${prgName} ${FsourceMAIN})
target_link_libraries(${prgName} FVS_${varName} FVSsql FVSfofem)
target_link_libraries(FVS_${varName} FVSfofem FVSsql)
add_dependencies(FVS_${varName} dbstypedefs)

set_target_properties(${prgName} PROPERTIES LINKER_LANGUAGE Fortran)
