project(installer-qt)

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

set_target_properties(listaller-qt PROPERTIES VERSION 0.4.0 SOVERSION 0.4)
set(CMAKE_INSTALL_LIBDIR lib CACHE PATH "Output directory for libraries")

target_link_libraries(listaller-qt
    ${CMAKE_CURRENT_BINARY_DIR}/liblistaller.so
)

set(LIB_INSTALL_DIR "\${exec_prefix}/${CMAKE_INSTALL_LIBDIR}"
  CACHE PATH "Directory where libraries will be installed"
  FORCE)
set(RESOURCE_PREFIX "")

configure_file("listaller-qt.pc.in" "listaller-qt.pc" @ONLY)

install(TARGETS listaller-qt DESTINATION ${CMAKE_INSTALL_LIBDIR})
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/listaller-qt.pc DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)
install(FILES ${listaller-qt_HEADERS} DESTINATION include/listaller-qt COMPONENT Headers)
