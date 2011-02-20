# CMakeLists for LibListaller

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

find_package(GLIB2 REQUIRED)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)

ensure_vala_version("0.10" MINIMUM)

set(LLISTALLER_SOURCES ${lisrc}/manager/manager.vala
	${lisrc}/manager/database.vala
)

link_libraries(${GLIB2_LIBRARIES}
	       ${GEE_LIBRARIES}
	       ${GIO2_LIBRARIES}
)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}
		    ${GLIB2_INCLUDE_DIR}
		    ${GEE_INCLUDE_DIR}
		    ${GIO2_INCLUDE_DIR}
)

vala_add_library("listaller" SHARED ${LLISTALLER_SOURCES}
			     BASE_DIR ${lisrc}
			     PACKAGES gee-1.0 sqlite3
			     GENERATE_VAPI listaller.vapi
			     GENERATE_HEADER listaller.h
)
