project(installer-qt)

cmake_minimum_required(VERSION 2.6.0)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/../../data/cmake_modules/)

find_package(Qt4 REQUIRED)
find_package(PkgConfig)

include(${QT_USE_FILE})

set(listaller-qt_SRCS limanager.cpp
    lisetup.cpp
)

qt4_automoc(${listaller-qt_SRCS})

add_definitions(${QT_DEFINITIONS})
add_definitions(-DQT_SHARED)

include_directories(${CMAKE_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
    ${qt4_include_dir}
    "${CMAKE_CURRENT_SOURCE_DIR}/../linst-cbind")

ADD_LIBRARY(listaller-qt SHARED ${listaller-qt_SRCS})

target_link_libraries(listaller-qt
    ${Listaller_LIBRARIES}
)

install(TARGETS listaller-qt  DESTINATION lib )
