# CMakeLists for LibListaller

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

find_package(GLIB2 REQUIRED)
find_package(Gettext REQUIRED)
find_package(SQLite3 REQUIRED)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)

ensure_vala_version("0.10" MINIMUM)

set(LLISTALLER_SOURCES ${lisrc}/manager/config.vala
	${lisrc}/manager/manager.vala
	${lisrc}/manager/database.vala
)

link_libraries(${GLIB2_LIBRARIES}
	       ${GEE_LIBRARIES}
	       ${GIO2_LIBRARIES}
	       ${SQLITE_LIBRARIES}
)

include_directories(${CMAKE_BINARY_DIR}
		    ${CMAKE_CURRENT_SOURCE_DIR}
		    ${GLIB2_INCLUDE_DIR}
		    ${GEE_INCLUDE_DIR}
		    ${GIO2_INCLUDE_DIR}
		    ${SQLITE_INCLUDE_DIR}
)

list(APPEND VALA_INCLUDE_DIRS ${CMAKE_BINARY_DIR})

#PackageKitGlib-1.0
vala_add_library("listaller" SHARED ${LLISTALLER_SOURCES}
			     BASE_DIR ${lisrc}
			     VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapis
			     PACKAGES pkgconfig gee-1.0 sqlite3 gio-2.0
			     GENERATE_VAPI listaller.vapi
			     GENERATE_HEADER listaller.h
)

set(LIUNITTEST_SOURCES ${LLISTALLER_SOURCES}
		       ${lisrc}/manager/unit-tests.vala
)

# Testcode
vala_add_executable ("li-unit-tests" ${LIUNITTEST_SOURCES}
				     BASE_DIR ${lisrc}
				     PACKAGES pkgconfig gee-1.0 sqlite3 gio-2.0
				     VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapis
)
