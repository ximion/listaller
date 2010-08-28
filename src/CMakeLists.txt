
find_package(Lazbuild REQUIRED)

if(LCL_VERSION LESS "929")
 message(FATAL_ERROR "LCL should be at least version 0.9.29!")
endif()

add_subdirectory(helper)
add_subdirectory(cmdtools)

if(QT OR GTK)
  file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/qt4)
  file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/gtk2)
  
  if(FRONTENDS)
    add_subdirectory(frontends)
  endif(FRONTENDS)
  
  if(CREATOR)
    add_subdirectory(licreator)
  endif(CREATOR)
  
endif(QT OR GTK)
