# CMakeLists for the Listaller Project
project(listaller)

cmake_minimum_required(VERSION 2.8.6 FATAL_ERROR)

#UNSET THIS VARIABLE AT RELEASE TIME
set(VERSION_SUFFIX "-dev")

set(CMAKE_BUILD_TYPE "Debug")

# Detect Git revision (if present)
if(VERSION_SUFFIX MATCHES "-dev")
	if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git)
		find_package(Git)
		if(GIT_EXECUTABLE)
		  execute_process(WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
				    COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
				    OUTPUT_VARIABLE project_revision RESULT_VARIABLE check_fail)
		  string(REGEX REPLACE "\n" "" project_revision ${project_revision})
		  if(check_fail)
		   message(STATUS "Could not fetch current Git revision: ${check_fail}")
		  else()
		   message(STATUS "Found Git revision: ${project_revision}")
		   set(VERSION_SUFFIX "${VERSION_SUFFIX}:${project_revision}")
		  endif(check_fail)
		endif(GIT_EXECUTABLE)
	endif()
endif()

set(LISTALLER_VERSION_MAJOR  "0")
set(LISTALLER_VERSION_MINOR  "5")
set(LISTALLER_VERSION_PATCH  "8")
set(LISTALLER_VERSION_SUFFIX "${VERSION_SUFFIX}")
set(LISTALLER_VERSION "${LISTALLER_VERSION_MAJOR}.${LISTALLER_VERSION_MINOR}.${LISTALLER_VERSION_PATCH}")

set(LISTALLER_API_LEVEL "0")

# Forbid in-tree building
if(${CMAKE_SOURCE_DIR} MATCHES ${CMAKE_BINARY_DIR})
      message(STATUS "Please do an out-of-tree build:")
      message(STATUS "rm -f CMakeCache.txt && mkdir build && cd build; cmake .. && make")
      message(FATAL_ERROR "In-tree-build detected!")
endif(${CMAKE_SOURCE_DIR} MATCHES ${CMAKE_BINARY_DIR})

#
# Options
#
option(QT "Build Listaller's Qt4 modules" OFF)
option(TOOLS "Build Listaller's non-gui tools" ON)
option(LIBUILD "Build and install the LIBuild facilities" OFF)
option(PATCHELF "Build patchelf utility" OFF)
option(VISUAL_LDD "Build the Visual-LDD utility" OFF)
option(L10N "Enable localization" ON)
option(API_DOCS "Add target to build API documentation" OFF)
option(TESTS "Enable Listaller's Unit Tests" OFF)
option(TEST_DEPMANAGER "Test Listaller's dependency solver" OFF)
option(TEST_SIGNING "Enable Listaller signing tests" OFF)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX
    "/usr" CACHE PATH "Listaller default install prefix" FORCE)
endif(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)

set(XDG_APPS_INSTALL_DIR share/applications)

add_definitions(-DCMAKE_INSTALL_PREFIX="\"${CMAKE_INSTALL_PREFIX}\"")
add_definitions(-DCMAKE_INSTALL_LIBDIR="\"${CMAKE_INSTALL_LIBDIR}\"")

set(CMAKE_INSTALL_LIBDIR "lib/${CMAKE_LIBRARY_ARCHITECTURE}" CACHE PATH "Output directory for libraries")
set(LISTALLER_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR}/listaller)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/data/cmake/)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/data/cmake/vala)

message(STATUS "Compiling Listaller version ${LISTALLER_VERSION}")

configure_file("${CMAKE_SOURCE_DIR}/data/cmake/cmake_uninstall.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
  IMMEDIATE @ONLY)

add_custom_target(uninstall
  "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
)

add_custom_target(distclean
  "make clean"
)

#
# l10n
#
find_package (Gettext REQUIRED)

#
# Default paths
#
set(BIN_INSTALL_DIR bin CACHE PATH "Installation directory for executables")
set(LIB_INSTALL_DIR lib/${CMAKE_LIBRARY_ARCHITECTURE} CACHE PATH "Installation directory for libraries")
set(INC_INSTALL_DIR include CACHE PATH "Installation directory for headers")
set(DATA_INSTALL_DIR share/listaller CACHE PATH "Installation directory for data files")
set(SYSCONF_INSTALL_DIR /etc CACHE PATH "Installation directory for system configuration files")

#
# Configure files
#
set (PREFIXDIR "${CMAKE_INSTALL_PREFIX}")
set (DATADIR "${CMAKE_INSTALL_PREFIX}/share")
set (PKGDATADIR "${DATA_INSTALL_DIR}")
set (LIBDIR "${CMAKE_INSTALL_PREFIX}/${LIB_INSTALL_DIR}")
set (PKGLIBDIR "${LIBDIR}/listaller")
set (GETTEXT_PACKAGE "listaller")
set (LOCALE_DIR "${DATADIR}/locale")
set (VERSION "${LISTALLER_VERSION}")
set (BUILDDIR "${CMAKE_BINARY_DIR}")

configure_file(config.h.in ${CMAKE_BINARY_DIR}/config.h)
# set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES ${CMAKE_SOURCE_DIR}/src/config.h)

#
# Enable/disable testing
#
if(TESTS)
	enable_testing()
endif(TESTS)

#
# Custom C flags
#
set (MAINTAINER_CFLAGS "-Werror -Wall -Wcast-align -Wno-uninitialized -Wempty-body -Wformat-security -Winit-self")
option (DISABLE_MAINTAINER_CFLAGS "Disable maintainer CFlags" ON)
if (DISABLE_MAINTAINER_CFLAGS)
	set (MAINTAINER_CFLAGS "")
endif (DISABLE_MAINTAINER_CFLAGS)
add_definitions(${MAINTAINER_CFLAGS})

# Required to include config.h before all other units
add_definitions ("--include config.h")

# Required if GPGme was compiled with _FILE_OFFSET_BITS=64
add_definitions("-D_FILE_OFFSET_BITS=64")

#
# Validate Vala version
#
find_package(PkgConfig REQUIRED)
find_package(Vala REQUIRED)
include(${VALA_USE_FILE})
ensure_vala_version("0.18" MINIMUM)

add_subdirectory(src)
add_subdirectory(tools)
add_subdirectory(data)
add_subdirectory(contrib)
add_subdirectory(docs)
if(TESTS)
	add_subdirectory(tests)
endif(TESTS)
if (GETTEXT_FOUND AND L10N)
  add_subdirectory(po)
endif (GETTEXT_FOUND AND L10N)
