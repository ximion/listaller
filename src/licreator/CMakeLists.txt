#CMakeLists for Listaller Creator

set(licreator_project licreator.lpr)

set(licreator_base_sources
	${licreator_project}
	editor.pas
	fwiz.pas
	prjwizard.pas
)

#Build GTK+
if(USE_GTK)
  li_set_build_project(${licreator_project} licreator gtk)
  add_custom_target(liCreatorGTK2 ALL DEPENDS ${licreator_name_gtk})
  
  install(FILES ${licreator_gtk_OUT} DESTINATION lib/licreator)
endif(USE_GTK)

#Build Qt
if(USE_QT)
  li_set_build_project(${licreator_project} licreator qt)
  add_custom_target(liCreatorQt4 ALL DEPENDS ${licreator_name_qt})
  
  install(FILES ${licreator_qt_OUT} DESTINATION lib/licreator)
endif(USE_QT)

install(FILES ${PROJECT_SOURCE_DIR}/graphics/listaller_creator.png DESTINATION share/pixmaps)
