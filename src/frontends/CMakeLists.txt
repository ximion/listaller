#CMakeLists for Listaller Frontends

############# ListallGO #############
set(listallgo_project listallgo.lpr)

set(listallgo_base_sources
	${listallgo_project}
	igobase.pas
	dgfrm.pas
)
set(listallgo_name ${CMAKE_BINARY_DIR}/listallgo)
set(listallgo_name_gtk ${CMAKE_BINARY_DIR}/gtk2/listallgo)
set(listallgo_name_qt ${CMAKE_BINARY_DIR}/qt4/listallgo)

############# ListallMgr #############
set(listallmgr_project listallmgr.lpr)

set(listallmgr_base_sources
	${listallmgr_project}
	manager.pas
	applist.pas
	aboutbox.pas
	
)
set(listallmgr_name ${CMAKE_BINARY_DIR}/listallmgr)
set(listallmgr_name_gtk ${CMAKE_BINARY_DIR}/gtk2/listallmgr)
set(listallmgr_name_qt ${CMAKE_BINARY_DIR}/qt4/listallmgr)

############# liUpdate #############
set(liupdate_project liupdate.lpr)

set(liupdate_base_sources
	${liupdate_project}
	
)
set(liupdate_name ${CMAKE_BINARY_DIR}/liupdate)
set(liupdate_name_gtk ${CMAKE_BINARY_DIR}/gtk2/liupdate)
set(liupdate_name_qt ${CMAKE_BINARY_DIR}/qt4/liupdate)

###

set(frontends_build_args_gtk "-B" "--ws=gtk2")
set(frontends_build_args_qt "-B" "--ws=qt")

##########> Build GTK+ <##########
## Listallgo
add_custom_command(OUTPUT ${listallgo_name_gtk}
	COMMAND ${LAZBUILD_EXE}
	ARGS ${frontends_build_args_gtk} ${listallgo_project}
	
	COMMAND ${CMAKE_COMMAND}
	ARGS -E rename ${listallgo_name} ${listallgo_name_gtk}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
              
	MAIN_DEPENDENCY ${listallgo_project}
	DEPENDS ${listallgo_base_sources}
)
## Listallmgr
add_custom_command(OUTPUT ${listallmgr_name_gtk}
	COMMAND ${LAZBUILD_EXE}
	ARGS ${frontends_build_args_gtk} ${listallmgr_project}
	
	COMMAND ${CMAKE_COMMAND}
	ARGS -E rename ${listallmgr_name} ${listallmgr_name_gtk}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
              
	MAIN_DEPENDENCY ${listallmgr_project}
	DEPENDS ${listallmgr_base_sources}
)
## LiUpdate
add_custom_command(OUTPUT ${liupdate_name_gtk}
	COMMAND ${LAZBUILD_EXE}
	ARGS ${frontends_build_args_gtk} ${liupdate_project}
	
	COMMAND ${CMAKE_COMMAND}
	ARGS -E rename ${liupdate_name} ${liupdate_name_gtk}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
              
	MAIN_DEPENDENCY ${liupdate_project}
	DEPENDS ${liupdate_base_sources}
)

if(USE_GTK)
  add_custom_target(frontendsGTK2 ALL DEPENDS ${listallgo_name_gtk}
					      ${listallmgr_name_gtk}
					      ${liupdate_name_gtk}
  )
  
  install(FILES ${listallgo_name_gtk} DESTINATION ${LISTALLER_INSTALL_DIR}/gtk2)
  install(FILES ${listallmgr_name_gtk} DESTINATION ${LISTALLER_INSTALL_DIR}/gtk2)
  install(FILES ${liupdate_name_gtk} DESTINATION ${LISTALLER_INSTALL_DIR}/gtk2)
endif(USE_GTK)

##########> Build Qt <##########
## Listallgo
add_custom_command(OUTPUT ${listallgo_name_qt}
	COMMAND ${LAZBUILD_EXE}
	ARGS ${frontends_build_args_qt} ${listallgo_project}
	
	COMMAND ${CMAKE_COMMAND}
	ARGS -E rename ${listallgo_name} ${listallgo_name_qt}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
              
	MAIN_DEPENDENCY ${listallgo_project}
	DEPENDS ${listallgo_base_sources}
)
## Listallmgr
add_custom_command(OUTPUT ${listallmgr_name_qt}
	COMMAND ${LAZBUILD_EXE}
	ARGS ${frontends_build_args_qt} ${listallmgr_project}
	
	COMMAND ${CMAKE_COMMAND}
	ARGS -E rename ${listallmgr_name} ${listallmgr_name_qt}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
              
	MAIN_DEPENDENCY ${listallmgr_project}
	DEPENDS ${listallmgr_base_sources}
)
## LiUpdate
add_custom_command(OUTPUT ${liupdate_name_qt}
	COMMAND ${LAZBUILD_EXE}
	ARGS ${frontends_build_args_qt} ${liupdate_project}
	
	COMMAND ${CMAKE_COMMAND}
	ARGS -E rename ${liupdate_name} ${liupdate_name_qt}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
              
	MAIN_DEPENDENCY ${liupdate_project}
	DEPENDS ${liupdate_base_sources}
)

if(USE_QT)
  add_custom_target(frontendsQt4 ALL DEPENDS ${listallgo_name_qt}
					      ${listallmgr_name_qt}
					      ${liupdate_name_qt}
  )
  
  install(FILES ${listallgo_name_qt} DESTINATION ${LISTALLER_INSTALL_DIR}/qt4)
  install(FILES ${listallmgr_name_qt} DESTINATION ${LISTALLER_INSTALL_DIR}/qt4)
  install(FILES ${liupdate_name_qt} DESTINATION ${LISTALLER_INSTALL_DIR}/qt4)
endif(USE_QT)
