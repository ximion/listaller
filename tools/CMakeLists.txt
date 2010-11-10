
add_subdirectory(litools)
add_subdirectory(runapp)

if(VLDD)
  add_subdirectory(visual-ldd)
endif(VLDD)
  
if(BUILDTOOLS)
  add_subdirectory(apbuild)
endif(BUILDTOOLS)

if(QT OR GTK)
  
  if(CREATOR)
    add_subdirectory(licreator)
  endif(CREATOR)
  
endif(QT OR GTK)
