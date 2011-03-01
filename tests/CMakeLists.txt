# CMakeLists for Listaller Unit Tests

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

find_package(GLIB2 REQUIRED)
find_package(Gettext REQUIRED)
find_package(SQLite3 REQUIRED)
find_package(LibArchive REQUIRED)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)

set(LIUNITTEST_SOURCES ${LISTALLER_BASE_SOURCES}
			${lisrc}/listaller/manager.vala
)

link_libraries(${GLIB2_LIBRARIES}
	       ${GEE_LIBRARIES}
	       ${GIO2_LIBRARIES}
	       ${SQLITE_LIBRARIES}
	       ${LibArchive_LIBRARIES}
)

include_directories(${CMAKE_BINARY_DIR}
		    ${CMAKE_CURRENT_SOURCE_DIR}
		    ${GLIB2_INCLUDE_DIR}
		    ${GEE_INCLUDE_DIR}
		    ${GIO2_INCLUDE_DIR}
		    ${SQLITE_INCLUDE_DIR}
		    ${LibArchive_INCLUDE_DIR}
)

# Database tests
vala_add_executable ("li-test-database" ${LIUNITTEST_SOURCES} tests-database.vala
				     BASE_DIR ${lisrc}
				     PACKAGES pkgconfig gee-1.0 gio-2.0 sqlite3 libarchive gpgme
				     VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapis
)

# IPK layout tests
vala_add_executable ("li-test-ipk" ${LIUNITTEST_SOURCES} tests-ipk.vala
				     BASE_DIR ${lisrc}
				     PACKAGES pkgconfig gee-1.0 gio-2.0 sqlite3 libarchive gpgme
				     VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapis
)

add_test(database-test li-test-database)
add_test(ipk-test li-test-ipk ${CMAKE_CURRENT_SOURCE_DIR})
