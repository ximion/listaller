project(installer-qt)

cmake_minimum_required(VERSION 2.6.0)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/../../data/cmake_modules/)

find_package(Qt4 REQUIRED)
find_package(GLIB2 REQUIRED)
find_package(PkgConfig)

include(${QT_USE_FILE})

set(installer-qt_SRCS
    lisetup.cpp
)

qt4_automoc(${installer-qt_SRCS})

add_definitions(${QT_DEFINITIONS})
add_definitions(-DQT_SHARED)

include_directories(${CMAKE_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
    ${GLIB2_INCLUDE_DIR}
    ${qt4_include_dir}
    "${CMAKE_CURRENT_SOURCE_DIR}/../linst-cbind")

ADD_LIBRARY(installer-qt SHARED ${installer-qt_SRCS})

target_link_libraries(installer-qt
    ${GLIB2_LIBRARIES}
    ${Listaller_LIBRARIES}
)

install(TARGETS installer-qt  DESTINATION lib )
