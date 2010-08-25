project(listaller)

cmake_minimum_required(VERSION 2.8.0 FATAL_ERROR)

message("The CMake build system of Listaller is not yet complete.")
message("Please use it only for testing purposes.")
message("To compile Listaller, please checkout the ./configure script!")
message("To use the CMake scripts, comment out this error.")
message(FATAL_ERROR "UNSTABLE BUILD SYSTEM")

if(COMMAND cmake_policy)
	cmake_policy(SET CMP0003 NEW)
endif(COMMAND cmake_policy)

set(VERSION_SUFFIX "-dev") #UNSET THIS VARIABLE AT RELEASE TIME

#detect vcs revision (if present)
if(VERSION_SUFFIX MATCHES "-dev")
	if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git)
		find_package(Git)
		if(GIT_EXECUTABLE)
		  execute_process(WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
				    COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
				    OUTPUT_VARIABLE project_revision RESULT_VARIABLE check_fail)
		  if(check_fail)
		   message(STATUS "Could not fetch current Git revision: ${check_fail}")
		  else()
		   message(STATUS "Builing revision: ${project_revision}")
		   set(VERSION_SUFFIX "-dev:${project_revision}")
		  endif(check_fail)
		endif(GIT_EXECUTABLE)
	endif()
endif()

set(LISTALLER_VERSION_MAJOR  "0")
set(LISTALLER_VERSION_MINOR  "5")
set(LISTALLER_VERSION_PATCH  "0")
set(LISTALLER_VERSION_SUFFIX "${VERSION_SUFFIX}")

set(CPACK_PACKAGE_VERSION_MAJOR "${LISTALLER_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${LISTALLER_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${LISTALLER_VERSION_PATCH}${VERSION_SUFFIX}")
set(LISTALLER_VERSION "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")

#forbid in-tree building & force "build" directory name
if(${CMAKE_SOURCE_DIR} MATCHES ${CMAKE_BINARY_DIR})
      message(STATUS "Please do an out-of-tree build:")
      message(STATUS "rm -f CMakeCache.txt && mkdir build && cd build; cmake .. && make")
      message(FATAL_ERROR "In-tree-build detected!")     
endif(${CMAKE_SOURCE_DIR} MATCHES ${CMAKE_BINARY_DIR})
if(NOT ${CMAKE_SOURCE_DIR}/build MATCHES ${CMAKE_BINARY_DIR})
      message("Please build only in 'build' subfolder!")
      message(FATAL_ERROR "Not in /build!")
endif()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/data/cmake_modules/)

find_package(PkgConfig REQUIRED)
find_package(UnixCommands REQUIRED)
find_package(Gettext REQUIRED)
find_library(QT4PAS_LIB NAMES Qt4Pas PATH /usr/lib /usr/local/lib /usr/lib64)
if(QT4PAS_LIB)
    message(STATUS "Found Qt4Pas: ${QT4PAS_LIB}")
else(QT4PAS_LIB)
    message(FATAL_ERROR "Could not find Qt4Pas")
endif(QT4PAS_LIB)

message(STATUS "Compiling Listaller version ${LISTALLER_VERSION}")
 
find_package(FPC REQUIRED)

make_directory("${CMAKE_BINARY_DIR}/${PASCAL_TARGET_ARCH}-${PASCAL_TARGET_OS}")

add_subdirectory(lib)
add_subdirectory(src)
