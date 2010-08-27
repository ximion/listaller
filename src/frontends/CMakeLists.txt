#CMakeLists for Listaller Frontends

############# ListallGO #############
set(listallgo_project listallgo.lpr)

set(listallgo_base_sources
	${listallgo_project}
	igobase.pas
	dgfrm.pas
)

############# ListallMgr #############
set(listallmgr_project listallmgr.lpr)

set(listallmgr_base_sources
	${listallmgr_project}
	manager.pas
	applist.pas
	aboutbox.pas
	
)

############# liUpdate #############
set(liupdate_project liupdate.lpr)

set(liupdate_base_sources
	${liupdate_project}
	
)

###

##########> Build GTK+ <##########
if(USE_GTK)
## Listallgo
li_set_build_project(${listallgo_project} listallgo gtk)

## Listallmgr
li_set_build_project(${listallmgr_project} listallmgr gtk)

## LiUpdate
li_set_build_project(${liupdate_project} liupdate gtk)

message(${listallgo_gtk_OUT})

add_custom_target(frontendsGTK2 ALL DEPENDS ${listallgo_gtk_OUT}
					    ${listallmgr_gtk_OUT}
					    ${liupdate_gtk_OUT}
)
  
  install(FILES ${listallgo_gtk_OUT} DESTINATION ${LISTALLER_INSTALL_DIR}/gtk2)
  install(FILES ${listallmgr_gtk_OUT} DESTINATION ${LISTALLER_INSTALL_DIR}/gtk2)
  install(FILES ${liupdate_gtk_OUT} DESTINATION ${LISTALLER_INSTALL_DIR}/gtk2)

endif(USE_GTK)

##########> Build Qt <##########
if(USE_QT)
## Listallgo
li_set_build_project(${listallgo_project} listallgo qt)

## Listallmgr
li_set_build_project(${listallmgr_project} listallmgr qt)

## LiUpdate
li_set_build_project(${liupdate_project} liupdate qt)

add_custom_target(frontendsQt4 ALL DEPENDS ${listallgo_qt_OUT}
					   ${listallmgr_qt_OUT}
					   ${liupdate_qt_OUT}
)
  
  install(FILES ${listallgo_qt_OUT} DESTINATION ${LISTALLER_INSTALL_DIR}/qt4)
  install(FILES ${listallmgr_qt_OUT} DESTINATION ${LISTALLER_INSTALL_DIR}/qt4)
  install(FILES ${liupdate_qt_OUT} DESTINATION ${LISTALLER_INSTALL_DIR}/qt4)
 
endif(USE_QT)
