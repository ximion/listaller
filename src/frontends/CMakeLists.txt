# CMakeLists for Listaller Frontends

############# ListallGO #############
set(listallgo_project listallgo)

set(listallgo_base_sources
	${listallgo_project}.lpr
	igobase.pas
	dgfrm.pas
)

############# ListallMgr #############
set(listallmgr_project listallmgr)

set(listallmgr_base_sources
	${listallmgr_project}.lpr
	manager.pas
	applist.pas
	aboutbox.pas
	
)

############# liUpdate #############
set(liupdate_project liupdate)

set(liupdate_base_sources
	${liupdate_project}.lpr
)

###

##########> Build GTK+ <##########
if(GTK)
## Listallgo
li_set_build_project(${listallgo_project} gtk)

## Listallmgr
li_set_build_project(${listallmgr_project} gtk)

## LiUpdate
li_set_build_project(${liupdate_project} gtk)

set(show_desktop GNOME)
set(notshow_desktop KDE)
set(mgr_current_exec "${listallmgr_project}-gtk")
configure_file("listaller-manager.desktop.in"
  "${CMAKE_CURRENT_BINARY_DIR}/listaller-manager-gnome.desktop"
  @ONLY
)

add_custom_command(OUTPUT "${CMAKE_BINARY_DIR}/${listallmgr_project}-gtk"
	  COMMAND ln
	  ARGS -f -s ${CMAKE_INSTALL_PREFIX}/${LISTALLER_INSTALL_DIR}/gtk2/${listallmgr_project}
		     ${listallmgr_project}-gtk
	  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}      
	  MAIN_DEPENDENCY ${listallmgr_gtk_OUT}
)

add_custom_target(frontendsGTK2 ALL DEPENDS ${listallgo_gtk_OUT}
					    ${listallmgr_gtk_OUT}
					    ${liupdate_gtk_OUT}
					    "${CMAKE_BINARY_DIR}/${listallmgr_project}-gtk"
)
  
  install(PROGRAMS ${listallgo_gtk_OUT}
		   ${listallmgr_gtk_OUT}
		   ${liupdate_gtk_OUT}
		   DESTINATION ${LISTALLER_INSTALL_DIR}/gtk2
		   COMPONENT frontends)
  install(PROGRAMS ${CMAKE_BINARY_DIR}/${listallmgr_project}-gtk
	  DESTINATION bin
	  COMPONENT frontends)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/listaller-manager-gnome.desktop
	  DESTINATION ${XDG_APPS_INSTALL_DIR}
	  COMPONENT frontends)

endif(GTK)

##########> Build Qt <##########
if(QT)
find_package(Qt4Pas REQUIRED)

## Listallgo
li_set_build_project(${listallgo_project} qt)

## Listallmgr
li_set_build_project(${listallmgr_project} qt)

## LiUpdate
li_set_build_project(${liupdate_project} qt)

set(show_desktop KDE)
set(notshow_desktop GNOME)
set(mgr_current_exec "${listallmgr_project}-qt")
configure_file("listaller-manager.desktop.in"
  "${CMAKE_CURRENT_BINARY_DIR}/listaller-manager-kde.desktop"
  @ONLY
)

add_custom_command(OUTPUT "${CMAKE_BINARY_DIR}/${listallmgr_project}-qt"
	  COMMAND ln
	  ARGS -f -s ${CMAKE_INSTALL_PREFIX}/${LISTALLER_INSTALL_DIR}/qt4/${listallmgr_project}
		     ${listallmgr_project}-qt
	  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}      
	  MAIN_DEPENDENCY ${listallmgr_qt_OUT}
)

add_custom_target(frontendsQt4 ALL DEPENDS ${listallgo_qt_OUT}
					   ${listallmgr_qt_OUT}
					   ${liupdate_qt_OUT}
					   "${CMAKE_BINARY_DIR}/${listallmgr_project}-qt"
)
  
  install(PROGRAMS ${listallgo_qt_OUT}
		   ${listallmgr_qt_OUT}
		   ${liupdate_qt_OUT}
		   DESTINATION ${LISTALLER_INSTALL_DIR}/qt4
		   COMPONENT frontends)
  install(PROGRAMS ${CMAKE_BINARY_DIR}/${listallmgr_project}-qt
	  DESTINATION bin
	  COMPONENT frontends)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/listaller-manager-kde.desktop
	  DESTINATION ${XDG_APPS_INSTALL_DIR}
	  COMPONENT frontends)
 
endif(QT)
