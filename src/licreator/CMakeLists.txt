#CMakeLists for Listaller Creator

set(licreator_project licreator.lpr)

set(licreator_base_sources
	${licreator_project}
	editor.pas
	fwiz.pas
	prjwizard.pas
)

set(licreator_name ${CMAKE_BINARY_DIR}/licreator)
set(licreator_name_gtk ${CMAKE_BINARY_DIR}/gtk2/licreator-gtk)
set(licreator_name_qt ${CMAKE_BINARY_DIR}/qt4/licreator-qt)
set(licreator_build_args_gtk "-B" "--ws=gtk2")
set(licreator_build_args_qt "-B" "--ws=qt")

#Build GTK+
add_custom_command(OUTPUT ${licreator_name_gtk}
	COMMAND ${LAZBUILD_EXE}
	ARGS ${licreator_build_args_gtk} ${licreator_project}
	
	COMMAND ${CMAKE_COMMAND}
	ARGS -E rename ${licreator_name} ${licreator_name_gtk}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
              
	MAIN_DEPENDENCY ${licreator_project}
	DEPENDS ${licreator_base_sources}
)
if(USE_GTK)
  add_custom_target(liCreatorGTK2 ALL DEPENDS ${licreator_name_gtk})
  
  install(FILES ${licreator_name_gtk} DESTINATION lib/licreator)
endif(USE_GTK)

#Build Qt
add_custom_command(OUTPUT ${licreator_name_qt}
	COMMAND ${LAZBUILD_EXE}
	ARGS ${licreator_build_args_qt} ${licreator_project}
	
	COMMAND ${CMAKE_COMMAND}
	ARGS -E rename ${licreator_name} ${licreator_name_qt}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
              
	MAIN_DEPENDENCY ${licreator_project}
	DEPENDS ${licreator_base_sources}
)
if(USE_QT)
  add_custom_target(liCreatorQt4 ALL DEPENDS ${licreator_name_qt})
  
  install(FILES ${licreator_name_qt} DESTINATION lib/licreator)
endif(USE_QT)

install(FILES ${PROJECT_SOURCE_DIR}/graphics/listaller_creator.png DESTINATION share/pixmaps)
