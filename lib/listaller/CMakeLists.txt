#CMakeLists for libListaller build

set(pascal_compiler_flags_cmn "-vm5024" "-fPIC" "-dNoGUI")

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

set(llistaller_name "${CMAKE_CURRENT_BINARY_DIR}/liblistaller.so.${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}")

set(llistaller_includes "-Fu${PROJECT_SOURCE_DIR}/src/"
			"-Fu${PROJECT_SOURCE_DIR}/src/bind/"
			"-Fu${PROJECT_SOURCE_DIR}/src/synapse/"
			"-Fu/usr/share/fpcsrc/2.4.0/packages/dbus/src/"
			"-Fu.")
			
set(llistaller_build_args ${PASCAL_COMPILER_FLAGS} "-o${llistaller_name}" ${llistaller_includes}
	"${CMAKE_CURRENT_SOURCE_DIR}/${llistaller_project}")

add_custom_command(OUTPUT ${llistaller_name}
	COMMAND "${PASCAL_COMPILER}"
	ARGS ${llistaller_build_args}
	MAIN_DEPENDENCY ${llistaller_project}
	DEPENDS ${llistaller_base_sources}
)

add_custom_target(liblistaller ALL DEPENDS ${llistaller_name})
