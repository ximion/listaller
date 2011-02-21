# CMakeLists for libIPK build

find_package(FPC)
find_package(GLIB2 REQUIRED)
find_package(SQLite3 REQUIRED)
find_package(DBus REQUIRED)
find_package(PolKit REQUIRED)
pkg_check_modules(PackageKitGLib2 REQUIRED packagekit-glib2>=0.6)

set(llistaller_project libipk.lpr)

set(llistaller_base_sources ${lisrc}/liversion.inc
	${llistaller_project}
	${lisrc}/liversion.inc
	${lisrc}/appinstalldb.pas
	${lisrc}/callbackprocess.pas
	${lisrc}/dderesolve.pas
	${lisrc}/depmanage.pas
	${lisrc}/distri.pas
	${lisrc}/gpgsign.pas
	${lisrc}/hashfunc.pas
	${lisrc}/iconloader.pas
	${lisrc}/ipkcdef10.pas
	${lisrc}/ipkinstall.pas
	${lisrc}/ipkpackage11.pas
	${lisrc}/liapp.pas
	${lisrc}/lifileutil.inc
	${lisrc}/lifileutil.pas
	${lisrc}/lihash.pas
	${lisrc}/limanageapp.pas
	${lisrc}/listallerdb.pas
	${lisrc}/listatusobj.pas
	${lisrc}/litranslator.pas
	${lisrc}/litypes.pas
	${lisrc}/litypes.pas.bak
	${lisrc}/liupdateapp.pas
	${lisrc}/liutils.pas
	${lisrc}/liutils.pas.bak
	${lisrc}/mtpcpu.pas
	${lisrc}/mtprocs.pas
	${lisrc}/nglclstrconsts.pas
	${lisrc}/ngmasks.pas
	${lisrc}/regexpr.pas
	${lisrc}/simdbus.pas
	${lisrc}/softwaredb.pas
	${lisrc}/strlocale.pas
	${lisrc}/tararchive.pas
	${lisrc}/bind/liappitem.pas
	${lisrc}/bind/packagekit.pas
	${lisrc}/bind/pkbitfield.inc
	${lisrc}/bind/pkclient.inc
	${lisrc}/bind/pkdesktop.inc
	${lisrc}/bind/pkenum.inc
	${lisrc}/bind/pkerror.inc
	${lisrc}/bind/pkpackagesack.inc
	${lisrc}/bind/pkprogress.inc
	${lisrc}/bind/pkresults.inc
	${lisrc}/bind/polkit.pas
)

set(listaller_libname "${LIPK_LIB}.${LIBS_SOVERSION}")

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
add_custom_command(OUTPUT "${PROJECT_BINARY_DIR}/${LIPK_LIB}.${LIBS_VERSION}"
	      COMMAND ln
	      ARGS -f -s ${listaller_libname} ${LIPK_LIB}.${LIBS_VERSION}
              MAIN_DEPENDENCY ${llistaller_project}
              WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)
add_custom_command(OUTPUT "${PROJECT_BINARY_DIR}/${LIPK_LIB}"
	      COMMAND ln
	      ARGS -f -s ${LIPK_LIB}.${LIBS_VERSION} ${LIPK_LIB}
              MAIN_DEPENDENCY ${llistaller_project}
              WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)



add_custom_target(listallerlib ALL DEPENDS "${PROJECT_BINARY_DIR}/${listaller_libname}"
					   "${PROJECT_BINARY_DIR}/${LIPK_LIB}.${LIBS_VERSION}"
					   "${PROJECT_BINARY_DIR}/${LIPK_LIB}"
)

set(llistaller_lib_parts ${PROJECT_BINARY_DIR}/${listaller_libname}
	      ${PROJECT_BINARY_DIR}/${LIPK_LIB}.${LIBS_VERSION}
	      ${PROJECT_BINARY_DIR}/${LIPK_LIB})

install(FILES ${llistaller_lib_parts} DESTINATION ${CMAKE_INSTALL_LIBDIR})
