# CMakeLists for LI-XBuildTools

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

find_package(GLIB2 REQUIRED)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)

ensure_vala_version("0.10" MINIMUM)



set(BUILDLIST_SOURCES buildlist.vala
)

link_libraries(${GLIB2_LIBRARIES}
	       ${GEE_LIBRARIES}
	       ${GIO2_LIBRARIES}
)

include_directories(${CMAKE_BINARY_DIR}
		    ${CMAKE_CURRENT_SOURCE_DIR}
		    ${GLIB2_INCLUDE_DIR}
		    ${GEE_INCLUDE_DIR}
		    ${GIO2_INCLUDE_DIR}
)

add_definitions(${GEE_CFLAGS} ${GEE_CFLAGS_OTHER})

vala_add_executable(buildlist ${BUILDLIST_SOURCES}
				PACKAGES posix glib-2.0 gio-2.0 gee-1.0
)

install(TARGETS buildlist DESTINATION include/lixbuild COMPONENT buildtools)
install(PROGRAMS ligcc
		 lig++
		 relaytool
	DESTINATION bin
	COMPONENT buildtools
)
install(FILES LixBuild/GCC.pm
	      LixBuild/Utils.pm
	DESTINATION share/listaller/LixBuild
	COMPONENT buildtools
)
install(FILES ctype.h DESTINATION include/lixbuild COMPONENT headers)
install(FILES relaytool.m4 DESTINATION share/aclocal COMPONENT headers)
