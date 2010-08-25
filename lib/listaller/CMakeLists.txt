#CMakeLists for libListaller build

find_package(FPC)
find_package(GLIB2 REQUIRED)
find_package(SQLite3 REQUIRED)
pkg_check_modules(DBus REQUIRED dbus-1)
pkg_check_modules(PolKitGObject REQUIRED polkit-gobject-1>=0.90)
pkg_check_modules(PackageKitGLib2 REQUIRED packagekit-glib2>=0.6)

set(llistaller_project liblistaller.lpr)

set(llistaller_base_sources
	${llistaller_project}
	dderesolve.pas
	depmanage.pas
	ipkinstall.pas
	lidbusproc.pas
	limanageapp.pas
	liupdateapp.pas
	softwaredb.pas
)

set(listaller_libname "${LISTALLER_LIB}.${LIBS_FULL_VERSION}")

set(llistaller_includes "-Fu${PROJECT_SOURCE_DIR}/src/"
			"-Fu${PROJECT_SOURCE_DIR}/src/bind/"
			"-Fu${PROJECT_SOURCE_DIR}/src/synapse/"
			"-Fu/usr/share/fpcsrc/2.4.0/packages/dbus/src/"
			"-Fu${CMAKE_CURRENT_SOURCE_DIR}"
			"-Fu.")

set(pascal_compiler_flags_cmn "-fPIC" "-dNoGUI")

set(llistaller_build_args ${PASCAL_COMPILER_FLAGS}
	${pascal_compiler_flags_cmn}
	"-Fi${CMAKE_CURRENT_BINARY_DIR}"
	"-FU${CMAKE_CURRENT_BINARY_DIR}"
	"-o${CMAKE_BINARY_DIR}/${listaller_libname}" ${llistaller_includes}
	"${CMAKE_CURRENT_SOURCE_DIR}/${llistaller_project}")

add_custom_command(OUTPUT "${PROJECT_BINARY_DIR}/${listaller_libname}"
	COMMAND "${PASCAL_COMPILER}"
	ARGS ${llistaller_build_args}
	MAIN_DEPENDENCY ${llistaller_project}
	DEPENDS ${llistaller_base_sources}
)
add_custom_command(OUTPUT "${PROJECT_BINARY_DIR}/${LISTALLER_LIB}.${LIBS_VERSION}"
	      COMMAND ln
	      ARGS -f -s ${listaller_libname} ${LISTALLER_LIB}.${LIBS_VERSION}
              MAIN_DEPENDENCY ${llistaller_project}
              WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)
add_custom_command(OUTPUT "${PROJECT_BINARY_DIR}/${LISTALLER_LIB}"
	      COMMAND ln
	      ARGS -f -s ${LISTALLER_LIB}.${LIBS_VERSION} ${LISTALLER_LIB}
              MAIN_DEPENDENCY ${llistaller_project}
              WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)



add_custom_target(listallerlib ALL DEPENDS "${PROJECT_BINARY_DIR}/${listaller_libname}"
					   "${PROJECT_BINARY_DIR}/${LISTALLER_LIB}.${LIBS_VERSION}"
					   "${PROJECT_BINARY_DIR}/${LISTALLER_LIB}"
)

install(FILES ${PROJECT_BINARY_DIR}/${listaller_libname}
	      ${PROJECT_BINARY_DIR}/${LISTALLER_LIB}.${LIBS_VERSION}
	      ${PROJECT_BINARY_DIR}/${LISTALLER_LIB}
	      DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
