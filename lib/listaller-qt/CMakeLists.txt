
cmake_minimum_required(VERSION 2.8.0)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/../../data/cmake_modules/)

find_package(Qt4 REQUIRED)
find_package(PkgConfig REQUIRED)

include(${QT_USE_FILE})

add_definitions(-DCMAKE_INSTALL_PREFIX="\\\"${CMAKE_INSTALL_PREFIX}\\\"")
add_definitions(-DCMAKE_INSTALL_LIBDIR="\\\"${CMAKE_INSTALL_LIBDIR}\\\"")

set(listaller-qt_SRCS 
    limanager.cpp
    lisetup.cpp
)

set(listaller-qt_HEADERS
   listaller-qt
   li-types.h
   limanager.h
   lisetup.h
)

qt4_automoc(${listaller-qt_SRCS})

add_definitions(${QT_DEFINITIONS})
add_definitions(-DQT_SHARED)

include_directories(${CMAKE_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
    ${qt4_include_dir}
    "${CMAKE_CURRENT_SOURCE_DIR}/../linst-cbind")

add_library(listaller-qt SHARED ${listaller-qt_SRCS})

if(NOT LIBS_FULL_VERSION)
 message(FATAL_ERROR "Library version not defined. (This should never happen. Do you use the top CMakeLists?)")
endif()

set_target_properties(listaller-qt PROPERTIES VERSION ${LIBS_FULL_VERSION} SOVERSION ${LIBS_VERSION})
set(CMAKE_INSTALL_LIBDIR lib CACHE PATH "Output directory for libraries")

target_link_libraries(listaller-qt
    ${QT_LIBRARIES}
    ${LISTALLER_LIB}
)

set(LIB_INSTALL_DIR "\${exec_prefix}/${CMAKE_INSTALL_LIBDIR}"
  CACHE PATH "Directory where libraries will be installed"
  FORCE)
set(RESOURCE_PREFIX "")

configure_file("listaller-qt.pc.in" "listaller-qt.pc" @ONLY)

install(TARGETS listaller-qt DESTINATION ${CMAKE_INSTALL_LIBDIR})
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/listaller-qt.pc DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)
install(FILES ${listaller-qt_HEADERS} DESTINATION include/listaller-qt COMPONENT Headers)
