
find_package(Lazbuild REQUIRED)

if(LCL_VERSION LESS "929")
 message(FATAL_ERROR "LCL should be at least version 0.9.29!")
endif()

include(${CMAKE_SOURCE_DIR}/data/cmake_modules/listaller_build_gui.cmake)

add_subdirectory(helper)
add_subdirectory(cmdtools)

if(USE_QT OR USE_GTK)
  file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/qt4)
  file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/gtk2)
  
  add_subdirectory(frontends)
  if(BUILD_CREATOR)
    add_subdirectory(licreator)
  endif(BUILD_CREATOR)
endif(USE_QT OR USE_GTK)
