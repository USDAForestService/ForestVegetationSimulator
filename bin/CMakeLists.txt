## $Id$
## Revised for VS2026 and Intel oneAPI (ifx)
## April 2026 - Don Robinson (ESSA), Google Gemini
## June 2026 - David Diaz (Vibrant Planet), updated to inject version and build metadata

cmake_minimum_required(VERSION 4.3)

message(STATUS "CMake version: ${CMAKE_VERSION}")

# --- 1. TOOLCHAIN SETUP ---
if (MSVC OR CMAKE_GENERATOR MATCHES "Visual Studio")
    message(STATUS "Building FVS for Windows (Visual Studio/Intel)")
    set(IS_WINDOWS_BUILD TRUE)
elseif (CMAKE_GENERATOR MATCHES "MinGW Makefiles" OR CMAKE_GENERATOR MATCHES "Unix Makefiles")
    message(STATUS "Building FVS for GCC environment")
    set(IS_GCC_BUILD TRUE)
endif()

# --- 2. VARIANT PROCESSING LOGIC ---
# Define which variant to build (e.g., FVSie, FVSpn)
file(GLOB tobuild FVSbc_sourceList.txt)
#file(GLOB tobuild FVSon_sourceList.txt)
#file(GLOB tobuild FVSie_sourceList.txt)
list(SORT tobuild)

set(dirsToProcess)

foreach (sourceList ${tobuild})
    get_filename_component(slfn ${sourceList} NAME)
    string(REPLACE "_sourceList.txt" "" prgName ${slfn})

    # Read and adjust source paths
    file(STRINGS ${sourceList} sourceListStrings NEWLINE_CONSUME)
    string(REPLACE "../" "../../" newsourceList "${sourceListStrings}")

    # Extract the sub-project CMakeLists logic (text after the tag)
    file(READ "${CMAKE_CURRENT_LIST_FILE}" full_content)
    string(REGEX REPLACE ".*##DO NOT REMOVE THIS TAG##" "" sub_cmake_logic "${full_content}")

    # Prep the subdirectory

## default
    set(targetDir "${prgName}_CmakeDir")

    file(MAKE_DIRECTORY ${targetDir})
    file(REMOVE "${targetDir}/CMakeCache.txt") 
    
    # Write the actual files into the sub-directory
    file(WRITE "${targetDir}/CMakeLists.txt" "${sub_cmake_logic}")
    file(WRITE "${targetDir}/${prgName}_sourceList.txt" "${newsourceList}")
    list(APPEND dirsToProcess ${targetDir})
endforeach()

# Execute sub-builds
foreach(toProcess ${dirsToProcess})
    execute_process(COMMAND cmake -G "${CMAKE_GENERATOR}" . WORKING_DIRECTORY ${toProcess})
endforeach()

# This 'return' prevents the parent script from executing the code below
return()

##DO NOT REMOVE THIS TAG##
cmake_minimum_required(VERSION 4.3)

# Set Intel LLVM (ifx) as the preferred Fortran compiler for VS2026
if(CMAKE_GENERATOR MATCHES "Visual Studio 18 2026")
    set(CMAKE_GENERATOR_TOOLSET "fortran=ifx" CACHE STRING "" FORCE)
endif()

project(FVS LANGUAGES C CXX Fortran)

# --- 3. VERSION AND BUILD METADATA ---
# Derived from git at cmake configure time. All fields fall back to
# 'unknown' if git is unavailable or the directory is not a git repo.
#
# A -dirty suffix indicates uncommitted changes in the working tree.
# A -dirty binary should not be used in production or deliverable contexts.
get_filename_component(FVS_ROOT "${CMAKE_CURRENT_LIST_DIR}/../.." ABSOLUTE)
find_package(Git)

if(Git_FOUND)
    execute_process(
        COMMAND ${GIT_EXECUTABLE} -C ${FVS_ROOT} remote get-url origin
        OUTPUT_VARIABLE FVS_GIT_REMOTE
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
        RESULT_VARIABLE _fvs_git_rc)
    if(NOT _fvs_git_rc EQUAL 0 OR FVS_GIT_REMOTE STREQUAL "")
        set(FVS_GIT_REMOTE unknown)
    endif()

    string(REGEX REPLACE ".*/([^/]+)/[^/]*(\\.git)?$" "\\1" FVS_GIT_ORG "${FVS_GIT_REMOTE}")
    if(FVS_GIT_ORG STREQUAL "" OR FVS_GIT_ORG STREQUAL FVS_GIT_REMOTE)
        set(FVS_GIT_ORG unknown)
    endif()

    execute_process(
        COMMAND ${GIT_EXECUTABLE} -C ${FVS_ROOT} describe --tags --exact-match
        OUTPUT_VARIABLE FVS_GIT_VERSION
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
        RESULT_VARIABLE _fvs_git_rc)
    if(NOT _fvs_git_rc EQUAL 0 OR FVS_GIT_VERSION STREQUAL "")
        execute_process(
            COMMAND ${GIT_EXECUTABLE} -C ${FVS_ROOT} describe --tags
            OUTPUT_VARIABLE FVS_GIT_VERSION
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET
            RESULT_VARIABLE _fvs_git_rc)
    endif()
    if(NOT _fvs_git_rc EQUAL 0 OR FVS_GIT_VERSION STREQUAL "")
        set(FVS_GIT_VERSION unknown)
    endif()

    execute_process(
        COMMAND ${GIT_EXECUTABLE} -C ${FVS_ROOT} rev-parse --short HEAD
        OUTPUT_VARIABLE FVS_GIT_HASH
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
        RESULT_VARIABLE _fvs_git_rc)
    if(NOT _fvs_git_rc EQUAL 0 OR FVS_GIT_HASH STREQUAL "")
        set(FVS_GIT_HASH unknown)
    endif()

    execute_process(
        COMMAND ${GIT_EXECUTABLE} -C ${FVS_ROOT} log -1 --format=%cd --date=format:%Y%m%d
        OUTPUT_VARIABLE FVS_GIT_DATE
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
        RESULT_VARIABLE _fvs_git_rc)
    if(NOT _fvs_git_rc EQUAL 0 OR FVS_GIT_DATE STREQUAL "")
        set(FVS_GIT_DATE unknown)
    endif()

    execute_process(
        COMMAND ${GIT_EXECUTABLE} -C ${FVS_ROOT} rev-parse --abbrev-ref HEAD
        OUTPUT_VARIABLE FVS_GIT_BRANCH
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
        RESULT_VARIABLE _fvs_git_rc)
    if(NOT _fvs_git_rc EQUAL 0 OR FVS_GIT_BRANCH STREQUAL "")
        set(FVS_GIT_BRANCH unknown)
    endif()

    execute_process(
        COMMAND ${GIT_EXECUTABLE} -C ${FVS_ROOT} status --porcelain
        OUTPUT_VARIABLE _fvs_git_dirty
        ERROR_QUIET)
    if(NOT _fvs_git_dirty STREQUAL "")
        set(FVS_GIT_VERSION "${FVS_GIT_VERSION}-dirty")
        set(FVS_GIT_HASH "${FVS_GIT_HASH}-dirty")
        message(WARNING "WARNING: working tree has uncommitted changes.")
        message(WARNING "WARNING: Version will be marked -dirty.")
        message(WARNING "WARNING: Do not use this build in production or deliverable contexts.")
    endif()
else()
    set(FVS_GIT_REMOTE unknown)
    set(FVS_GIT_ORG unknown)
    set(FVS_GIT_VERSION unknown)
    set(FVS_GIT_HASH unknown)
    set(FVS_GIT_DATE unknown)
    set(FVS_GIT_BRANCH unknown)
endif()

message(STATUS "FVS_GIT_REMOTE:  ${FVS_GIT_REMOTE}")
message(STATUS "FVS_GIT_ORG:     ${FVS_GIT_ORG}")
message(STATUS "FVS_GIT_VERSION: ${FVS_GIT_VERSION}")
message(STATUS "FVS_GIT_HASH:    ${FVS_GIT_HASH}")
message(STATUS "FVS_GIT_DATE:    ${FVS_GIT_DATE}")
message(STATUS "FVS_GIT_BRANCH:  ${FVS_GIT_BRANCH}")

# --- 4. COMPILER CONFIGURATION ---
set(CMAKE_Fortran_Format FIXED)
set(CMAKE_VERBOSE_MAKEFILE ON)

if(MSVC)
    add_compile_definitions(ANSI WINDOWS _WINDLL _CRT_SECURE_NO_WARNINGS)
else()
    add_compile_definitions(ANSI CMPgcc)
endif()

# --- 5. SOURCE CATEGORIZATION ---
file(GLOB sl_file FVS*_sourceList.txt)
get_filename_component(slfn ${sl_file} NAME)
string(REPLACE "_sourceList.txt" "" prgName ${slfn})
string(REPLACE "FVS" "" varName ${prgName})

file(STRINGS ${prgName}_sourceList.txt fileList)

set(FortranHeaders "")
set(FsourceFVS "")
set(FsourceMAIN "")
set(CsourceSQL "")
set(CsourceFFE "")
set(includeDirs "")

foreach(fn ${fileList})
    string(STRIP "${fn}" fn)
    get_filename_component(pname ${fn} PATH)
    get_filename_component(fname ${fn} NAME)
    string(TOLOWER "${fn}" fn_low)

    # Split Headers: Fortran vs C
    if(fn_low MATCHES "\\.(f77|inc)$")
        list(APPEND FortranHeaders "${fn}")
        set(is_header TRUE)
#    elseif(fn_low MATCHES "\\.h$")
#        list(APPEND CHeaders "${fn}")
#        set(is_header TRUE)
    else()
        set(is_header FALSE)
    endif()

    if(is_header)
        if(pname MATCHES "/${varName}$")
            list(INSERT includeDirs 0 "${pname}")
        else()
            list(APPEND includeDirs "${pname}")
        endif()
    # Fortran Source
    elseif(fn_low MATCHES "\\.(f|for)$")
        if(fname STREQUAL "main.f")
            list(APPEND FsourceMAIN "${fn}")
        else()
            list(APPEND FsourceFVS "${fn}")
        endif()
    # C/C++ Source
    elseif(fn_low MATCHES "\\.(c|cpp)$")
        if(fname MATCHES "(sqlite3|fvsqlite3|apisubsc)\\.c")
            list(APPEND CsourceSQL "${fn}")
        else()
            list(APPEND CsourceFFE "${fn}")
        endif()
    endif()
endforeach()

if(includeDirs)
    list(REMOVE_DUPLICATES includeDirs)
    list(REVERSE includeDirs)
endif()

# --- 6. PROPERTY ASSIGNMENT  ---

foreach(src ${FsourceFVS} ${FsourceMAIN})
    get_filename_component(src_name ${src} NAME)
    if(src_name STREQUAL "version.f" OR src_name STREQUAL "main.f")
        set_source_files_properties(${src} PROPERTIES Fortran_FORMAT FREE)
    endif()
endforeach()

# Apply Fortran-specific indexing to .f77 and .inc
if(FortranHeaders)
    set_source_files_properties(${FortranHeaders} PROPERTIES 
        HEADER_FILE_ONLY TRUE
        LANGUAGE Fortran
    )
    source_group("Include Files" FILES ${FortranHeaders})
endif()

# Standard C indexing for .h files
#if(CHeaders)
#    set_source_files_properties(${CHeaders} PROPERTIES
#			 HEADER_FILE_ONLY TRUE
#		)
#    source_group("C Headers" FILES ${CHeaders})
#endif()

## Combine lists for target definition
#set(AllHeaders ${FortranHeaders} ${CHeaders})

# --- 7. TARGET DEFINITIONS ---
include_directories(BEFORE ${includeDirs} "${CMAKE_CURRENT_BINARY_DIR}")

add_library(FVSsql SHARED ${CsourceSQL})
add_library(FVSfofem SHARED ${CsourceFFE})
add_library(FVS_${varName} SHARED ${FsourceFVS} ${FortranHeaders})
add_executable(${prgName} ${FsourceMAIN} ${FortranHeaders})

# --- 8. TARGET PROPERTIES & LINKING ---
set_property(DIRECTORY PROPERTY VS_STARTUP_PROJECT ${prgName})

set_target_properties(FVS_${varName} PROPERTIES 
    Fortran_PREPROCESS ON
    WINDOWS_EXPORT_ALL_SYMBOLS TRUE
    LINKER_LANGUAGE Fortran
)
target_compile_definitions(FVS_${varName} PRIVATE
    "FVS_GIT_ORG=\"${FVS_GIT_ORG}\""
    "FVS_GIT_VERSION=\"${FVS_GIT_VERSION}\""
    "FVS_GIT_HASH=\"${FVS_GIT_HASH}\""
    "FVS_GIT_DATE=\"${FVS_GIT_DATE}\""
    "FVS_GIT_BRANCH=\"${FVS_GIT_BRANCH}\""
    "FVS_GIT_REMOTE=\"${FVS_GIT_REMOTE}\""
)
set_target_properties(${prgName} PROPERTIES 
    Fortran_PREPROCESS ON
    LINKER_LANGUAGE Fortran
)

if(MSVC OR CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    target_compile_options(FVS_${varName} PRIVATE "/MP")
    target_compile_options(${prgName} PRIVATE "/MP")
endif()

target_link_libraries(FVS_${varName} PRIVATE FVSsql FVSfofem)
target_link_libraries(${prgName} PRIVATE FVS_${varName})