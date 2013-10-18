# CMakeLists for Listaller Unit Tests

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

find_package(GLIB2 REQUIRED)
find_package(Gettext REQUIRED)
find_package(GPGMe REQUIRED)
find_package(GI REQUIRED)
find_library(M_LIB m)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)
pkg_check_modules(REDLAND REQUIRED redland>=0.4)
pkg_check_modules(PACKAGEKIT REQUIRED packagekit-glib2)
#find_gir(PKGIR "packagekit-glib2" REQUIRED)

set(LIUNITTEST_SOURCES testcommon.vala
)

link_libraries(${M_LIB}
		${GLIB2_LIBRARIES}
		${GEE_LIBRARIES}
		${GIO2_LIBRARIES}
		${LIBXML2_LIBRARIES}
		${GPGME_VANILLA_LIBRARIES}
		${PACKAGEKIT_LIBRARIES}
		${REDLAND_LIBRARIES}
		listaller-glib
)

include_directories(${CMAKE_BINARY_DIR}
		${CMAKE_BINARY_DIR}/src
		${CMAKE_CURRENT_SOURCE_DIR}
		${GLIB2_INCLUDE_DIR}
		${GEE_INCLUDE_DIRS}
		${GIO2_INCLUDE_DIR}
		${LIBXML2_INCLUDE_DIR}
		${GPGME_INCLUDES}
		${PACKAGEKIT_INCLUDE_DIRS}
		${REDLAND_INCLUDE_DIRS}
)

add_definitions ("-DI_KNOW_THE_PACKAGEKIT_GLIB2_API_IS_SUBJECT_TO_CHANGE")
add_definitions ("-DI_KNOW_THE_PACKAGEKIT_PLUGIN_API_IS_SUBJECT_TO_CHANGE")

# Basic tests
vala_add_executable (li-test-basics ${LIUNITTEST_SOURCES} tests-basics.vala
				BASE_DIR ${lisrc}
				PACKAGES ${LISTALLER_BASE_VALA_DEPS} packagekit-glib2 listaller_internal
				VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
				COMPILE_FLAGS ${GLOBAL_VALAC_FLAGS}
)
add_test(basic-test li-test-basics ${CMAKE_CURRENT_SOURCE_DIR})

# Database tests
vala_add_executable (li-test-database ${LIUNITTEST_SOURCES} tests-database.vala
				BASE_DIR ${lisrc}
				PACKAGES ${LISTALLER_BASE_VALA_DEPS} packagekit-glib2 listaller_internal
				VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
				COMPILE_FLAGS ${GLOBAL_VALAC_FLAGS}
)
add_test(database-test li-test-database)

# IPK layout tests
vala_add_executable (li-test-ipk ${LIUNITTEST_SOURCES} tests-ipk.vala
				BASE_DIR ${lisrc}
				PACKAGES ${LISTALLER_BASE_VALA_DEPS} packagekit-glib2 listaller_internal
				VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
				COMPILE_FLAGS ${GLOBAL_VALAC_FLAGS}
)
add_test(ipk-test li-test-ipk ${CMAKE_CURRENT_SOURCE_DIR})

# NOTE: lipkgen is part of LiTools, but it is essential for other tests to complete, so we don't exclude
# it here and instead depend on it. People who want to run tests need to be aware that this is intended
# behaviour.
# Lipkgen tests
vala_add_executable (li-test-ipkbuild ${LIUNITTEST_SOURCES} tests-ipkbuild.vala
				BASE_DIR ${lisrc}
				PACKAGES ${LISTALLER_BASE_VALA_DEPS} packagekit-glib2 listaller_internal
				VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
				COMPILE_FLAGS ${GLOBAL_VALAC_FLAGS}
)
add_test(ipkbuild-test li-test-ipkbuild ${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_BINARY_DIR}/tools/appcompile/appcompile
	${CMAKE_BINARY_DIR}/tools/lipkgen/lipkgen
)
add_dependencies(li-test-ipkbuild lipkgen appcompile)

# IPK installer tests
vala_add_executable (li-test-installer ${LIUNITTEST_SOURCES} tests-installer.vala
				BASE_DIR ${lisrc}
				PACKAGES ${LISTALLER_BASE_VALA_DEPS} packagekit-glib2 listaller_internal
				VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
				COMPILE_FLAGS ${GLOBAL_VALAC_FLAGS}
)
add_test(installer-test li-test-installer ${CMAKE_CURRENT_SOURCE_DIR})

# IPK repo tests
vala_add_executable (li-test-repo ${LIUNITTEST_SOURCES} tests-repository.vala
				BASE_DIR ${lisrc}
				PACKAGES ${LISTALLER_BASE_VALA_DEPS} packagekit-glib2 listaller_internal
				VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
				COMPILE_FLAGS ${GLOBAL_VALAC_FLAGS}
)
add_test(repo-test li-test-repo ${CMAKE_CURRENT_SOURCE_DIR})

# Updater tests
vala_add_executable (li-test-updater ${LIUNITTEST_SOURCES} tests-updater.vala
				BASE_DIR ${lisrc}
				PACKAGES ${LISTALLER_BASE_VALA_DEPS} packagekit-glib2 listaller_internal
				VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
				COMPILE_FLAGS ${GLOBAL_VALAC_FLAGS}
)
add_test(updater-test li-test-updater ${CMAKE_CURRENT_SOURCE_DIR})

# Dependency solver tests
vala_add_executable (li-test-depmanager ${LIUNITTEST_SOURCES} tests-depmanager.vala
				BASE_DIR ${lisrc}
				PACKAGES ${LISTALLER_BASE_VALA_DEPS} packagekit-glib2 listaller_internal
				VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
				COMPILE_FLAGS ${GLOBAL_VALAC_FLAGS}
)
if(TEST_DEPMANAGER)
	add_test(depmanager-test li-test-depmanager ${CMAKE_CURRENT_SOURCE_DIR})
endif(TEST_DEPMANAGER)

# Tests of GPG signing
vala_add_executable (li-test-signing ${LIUNITTEST_SOURCES} tests-signing.vala
							../tools/lipkgen/lpkgutils.vala
							../tools/lipkgen/gpgsign.vala
				BASE_DIR ${lisrc}
				PACKAGES ${LISTALLER_BASE_VALA_DEPS} packagekit-glib2 listaller_internal
				VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
				COMPILE_FLAGS ${GLOBAL_VALAC_FLAGS}
)
if(TEST_SIGNING)
	add_test(signatures-test li-test-signing ${CMAKE_CURRENT_SOURCE_DIR})
endif(TEST_SIGNING)

# GPG Keydatabase tests
vala_add_executable (li-test-keydb ${LIUNITTEST_SOURCES} tests-keydb.vala
				BASE_DIR ${lisrc}
				PACKAGES ${LISTALLER_BASE_VALA_DEPS} packagekit-glib2 listaller_internal
				VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi ${CMAKE_BINARY_DIR}/src
				COMPILE_FLAGS ${GLOBAL_VALAC_FLAGS}
)
add_test(keydb-test li-test-keydb ${CMAKE_CURRENT_SOURCE_DIR})
