# CMakeLists for the Listaller Project
project(listaller)

cmake_minimum_required(VERSION 2.8.0 FATAL_ERROR)

if(COMMAND cmake_policy)
	cmake_policy(SET CMP0003 NEW)
	cmake_policy(SET CMP0002 OLD)
endif(COMMAND cmake_policy)

#UNSET THIS VARIABLE AT RELEASE TIME
set(VERSION_SUFFIX "-dev")

# Detect Git revision (if present)
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
		   message(STATUS "Found Git revision: ${project_revision}")
		   set(VERSION_SUFFIX "${VERSIOn_SUFFIX}:${project_revision}")
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

# Forbid in-tree building & force "build" directory name
if(${CMAKE_SOURCE_DIR} MATCHES ${CMAKE_BINARY_DIR})
      message(STATUS "Please do an out-of-tree build:")
      message(STATUS "rm -f CMakeCache.txt && mkdir build && cd build; cmake .. && make")
      message(FATAL_ERROR "In-tree-build detected!")     
endif(${CMAKE_SOURCE_DIR} MATCHES ${CMAKE_BINARY_DIR})
if(NOT ${CMAKE_SOURCE_DIR}/build MATCHES ${CMAKE_BINARY_DIR})
      message("Please build only in 'build' subfolder!")
      message(FATAL_ERROR "Not in /build!")
endif()

########################################
OPTION(QT "Build Listaller's Qt4 modules" OFF)
OPTION(GTK "Build Listaller's GTK2 modules" OFF)
OPTION(FRONTENDS "Compile Listaller's frontends" OFF)
OPTION(CREATOR "Build the liCreator application" OFF)
########################################

IF(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  SET(CMAKE_INSTALL_PREFIX
    "/usr" CACHE PATH "Listaller default install prefix" FORCE)
ENDIF(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)

set(XDG_APPS_INSTALL_DIR share/applications)

add_definitions(-DCMAKE_INSTALL_PREFIX="\\\"${CMAKE_INSTALL_PREFIX}\\\"")
add_definitions(-DCMAKE_INSTALL_LIBDIR="\\\"${CMAKE_INSTALL_LIBDIR}\\\"")

set(CMAKE_INSTALL_LIBDIR lib CACHE PATH "Output directory for libraries")
set(LISTALLER_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR}/listaller)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/data/cmake/)

message(STATUS "Compiling Listaller version ${LISTALLER_VERSION}")

configure_file("${CMAKE_MODULE_PATH}/cmake_uninstall.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
  IMMEDIATE @ONLY)
  
add_custom_target(uninstall
  "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
)

add_custom_target(writedoc
  "bash" "${CMAKE_SOURCE_DIR}/src/writedoc"
)

find_package(PkgConfig REQUIRED)
find_package(UnixCommands REQUIRED)

include(${CMAKE_SOURCE_DIR}/data/cmake/listaller_build_gui.cmake)

make_directory("${CMAKE_BINARY_DIR}/${PASCAL_TARGET_ARCH}-${PASCAL_TARGET_OS}")

add_subdirectory(lib)
add_subdirectory(src)
add_subdirectory(data)
add_subdirectory(graphics)
add_subdirectory(po)
