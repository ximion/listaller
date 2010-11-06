
cmake_minimum_required(VERSION 2.8.0)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/../../data/cmake_modules/)

find_package(Qt4 REQUIRED)
include(${QT_USE_FILE})
find_package(PkgConfig REQUIRED)

set(listaller-qt_SRCS
    limanager.cpp
    lisetup.cpp
)

set(listaller-qt_HEADERS
   limanager.h
   lisetup.h
)

set(listaller-qt_INST_HEADERS Listaller-Qt
   limanager.h
   lisetup.h
)

qt4_wrap_cpp(listaller-qt_MOC_SRCS ${listaller-qt_HEADERS})

ADD_DEFINITIONS(${QT_DEFINITIONS})
ADD_DEFINITIONS(-DQT_PLUGIN)
ADD_DEFINITIONS(-DQT_NO_DEBUG)
ADD_DEFINITIONS(-DQT_SHARED)

link_directories(${CMAKE_BINARY_DIR})
include_directories(${CMAKE_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
    ${qt4_include_dir}
    "${CMAKE_CURRENT_SOURCE_DIR}/../linst-cbind")

add_library(listaller-qt SHARED ${listaller-qt_SRCS} ${listaller-qt_MOC_SRCS})

if(NOT LIBS_FULL_VERSION)
 message(FATAL_ERROR "Library version not defined. (This should never happen. Do you use the top CMakeLists?)")
endif()

set_target_properties(listaller-qt PROPERTIES VERSION ${LIBS_FULL_VERSION} SOVERSION ${LIBS_SOVERSION})

target_link_libraries(listaller-qt
    ${KDE4_KDEUI_LIBS}
    ${KDE4_KIO_LIBS}
    ${LISTALLER_LIB}
)

set(LIB_INSTALL_DIR "\${exec_prefix}/${CMAKE_INSTALL_LIBDIR}"
  CACHE PATH "Directory where libraries will be installed"
  FORCE)
set(RESOURCE_PREFIX "")

configure_file("listaller-qt.pc.in" "listaller-qt.pc" @ONLY)

install(TARGETS listaller-qt DESTINATION ${CMAKE_INSTALL_LIBDIR})
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/listaller-qt.pc DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig COMPONENT headers)
install(FILES ${listaller-qt_INST_HEADERS} DESTINATION include/listaller-qt COMPONENT headers)
