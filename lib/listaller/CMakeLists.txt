# CMakeLists for libListaller build

find_package(FPC)
find_package(GLIB2 REQUIRED)
find_package(SQLite3 REQUIRED)
find_package(DBus REQUIRED)
find_package(PolKit REQUIRED)
pkg_check_modules(PackageKitGLib2 REQUIRED packagekit-glib2>=0.6)

set(llistaller_project liblistaller.lpr)

set(lisrc ${CMAKE_SOURCE_DIR}/src)
set(llistaller_base_sources ${lisrc}/liversion.inc
	${llistaller_project}
	${lisrc}/liversion.inc
	${lisrc}/dderesolve.pas
	${lisrc}/depmanage.pas
	${lisrc}/ipkinstall.pas
	${lisrc}/lidbusproc.pas
	${lisrc}/limanageapp.pas
	${lisrc}/liupdateapp.pas
	${lisrc}/appinstalldb.pas
	${lisrc}/listallerdb.pas
	${lisrc}/softwaredb.pas
)

set(listaller_libname "${LISTALLER_LIB}.${LIBS_SOVERSION}")

set(llistaller_includes "-Fu${lisrc}"
			"-Fu${lisrc}/bind/"
			"-Fu${lisrc}/synapse/"
			"-Fu${lisrc}/backends/"
			"-Fu/usr/share/fpcsrc/2.4.0/packages/dbus/src/"
			"-Fu${CMAKE_CURRENT_SOURCE_DIR}"
			"-Fu.")

set(pascal_compiler_flags_cmn "-fPIC" "-dNoGUI" "-CX" "-XX")

# Disable Cg flag, if we build for i386
if (${PASCAL_TARGET_ARCH} MATCHES "[iI]386+")
 set(pascal_compiler_flags_cmn ${pascal_compiler_flags_cmn} "-Cg-")
endif()

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

set(llistaller_lib_parts ${PROJECT_BINARY_DIR}/${listaller_libname}
	      ${PROJECT_BINARY_DIR}/${LISTALLER_LIB}.${LIBS_VERSION}
	      ${PROJECT_BINARY_DIR}/${LISTALLER_LIB})

install(FILES ${llistaller_lib_parts} DESTINATION ${CMAKE_INSTALL_LIBDIR})
