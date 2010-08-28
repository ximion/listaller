# CMakeLists for Listaller Creator

set(licreator_project licreator)

set(licreator_base_sources
	${licreator_project}.lpr
	editor.pas
	fwiz.pas
	prjwizard.pas
)

set(LICREATOR_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR}/licreator)

#########> Build GTK+ <#########
if(USE_GTK)

  li_set_build_project(${licreator_project} gtk)
  add_custom_target(liCreatorGTK2 ALL DEPENDS ${licreator_gtk_OUT})
  
  set(CURRENT_WS GTK+)
  set(licreator_current_exec "${CMAKE_INSTALL_PREFIX}/${LICREATOR_INSTALL_DIR}/${licreator_project}-gtk")
  configure_file("licreator.desktop.in"
    "${CMAKE_CURRENT_BINARY_DIR}/licreator-gnome.desktop"
    @ONLY
  )
  
  install(PROGRAMS ${licreator_gtk_OUT} DESTINATION ${LICREATOR_INSTALL_DIR})
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/licreator-gnome.desktop DESTINATION ${XDG_APPS_INSTALL_DIR})
  
endif(USE_GTK)

#########> Build Qt <#########
if(USE_QT)
  find_package(Qt4Pas REQUIRED)

  li_set_build_project(${licreator_project} qt)
  add_custom_target(liCreatorQt4 ALL DEPENDS ${licreator_qt_OUT})
  
  set(CURRENT_WS Qt)
  set(licreator_current_exec "${CMAKE_INSTALL_PREFIX}/${LICREATOR_INSTALL_DIR}/${licreator_project}-qt")
  configure_file("licreator.desktop.in"
    "${CMAKE_CURRENT_BINARY_DIR}/licreator-kde.desktop"
    @ONLY
  )
  
  install(PROGRAMS ${licreator_qt_OUT} DESTINATION ${LICREATOR_INSTALL_DIR})
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/licreator-kde.desktop DESTINATION ${XDG_APPS_INSTALL_DIR})
  
endif(USE_QT)

install(FILES ${PROJECT_SOURCE_DIR}/graphics/listaller_creator.png DESTINATION share/pixmaps)
