# CMakeLists for Listaller Unit Tests

find_package(Vala REQUIRED)
include("${VALA_USE_FILE}")

find_package(GLIB2 REQUIRED)
find_package(Gettext REQUIRED)
find_package(SQLite3 REQUIRED)
find_package(LibArchive REQUIRED)
find_package(GPGme REQUIRED)
find_package(GI REQUIRED)
pkg_check_modules(GEE REQUIRED gee-1.0)
pkg_check_modules(GIO2 REQUIRED gio-2.0)
pkg_check_modules(PACKAGEKIT REQUIRED packagekit-glib2)
find_gir(PKGIR "PackageKitGlib-1.0" REQUIRED)

set(LIUNITTEST_SOURCES ${LISTALLER_BASE_SOURCES}
			${lisrc}/listaller/depsolver.vala
			${lisrc}/listaller/depprovider_pk.vala
			${lisrc}/listaller/manager.vala
			${lisrc}/listaller/installer.vala
)

link_libraries(${GLIB2_LIBRARIES}
		${GEE_LIBRARIES}
		${GIO2_LIBRARIES}
		${SQLITE_LIBRARIES}
		${LibArchive_LIBRARIES}
		${LIBXML2_LIBRARIES}
		${GPGME_VANILLA_LIBRARIES}
		${PACKAGEKIT_LIBRARIES}
)

include_directories(${CMAKE_BINARY_DIR}
		${CMAKE_CURRENT_SOURCE_DIR}
		${GLIB2_INCLUDE_DIR}
		${GEE_INCLUDE_DIRS}
		${GIO2_INCLUDE_DIR}
		${SQLITE_INCLUDE_DIR}
		${LibArchive_INCLUDE_DIR}
		${LIBXML2_INCLUDE_DIR}
		${GPGME_INCLUDES}
		${PACKAGEKIT_INCLUDE_DIRS}
)

add_definitions ("-DI_KNOW_THE_PACKAGEKIT_GLIB2_API_IS_SUBJECT_TO_CHANGE")

# Database tests
vala_add_executable (li-test-database ${LIUNITTEST_SOURCES} tests-database.vala
				BASE_DIR ${lisrc}
				PACKAGES ${LISTALLER_BASE_VALA_DEPS} PackageKitGlib-1.0
				VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi
				COMPILE_FLAGS --metadatadir=${CMAKE_SOURCE_DIR}/vapi
)
add_test(database-test li-test-database)

# IPK layout tests
vala_add_executable (li-test-ipk ${LIUNITTEST_SOURCES} tests-ipk.vala
				BASE_DIR ${lisrc}
				PACKAGES ${LISTALLER_BASE_VALA_DEPS} PackageKitGlib-1.0
				VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi
				COMPILE_FLAGS --metadatadir=${CMAKE_SOURCE_DIR}/vapi
)
add_test(ipk-test li-test-ipk ${CMAKE_CURRENT_SOURCE_DIR})

# IPK installer tests
vala_add_executable (li-test-installer ${LIUNITTEST_SOURCES} tests-installer.vala
				BASE_DIR ${lisrc}
				PACKAGES ${LISTALLER_BASE_VALA_DEPS} PackageKitGlib-1.0
				VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi
				COMPILE_FLAGS --metadatadir=${CMAKE_SOURCE_DIR}/vapi
)
add_test(installer-test li-test-installer ${CMAKE_CURRENT_SOURCE_DIR})

if(TOOLS)
# LiBuild tests
vala_add_executable (li-test-ipkbuild ${LIUNITTEST_SOURCES} tests-ipkbuild.vala
				BASE_DIR ${lisrc}
				PACKAGES ${LISTALLER_BASE_VALA_DEPS} PackageKitGlib-1.0
				VAPI_DIRS ${CMAKE_SOURCE_DIR}/vapi
				COMPILE_FLAGS --metadatadir=${CMAKE_SOURCE_DIR}/vapi
)
add_test(ipkbuild-test li-test-ipkbuild ${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_BINARY_DIR}/tools/appcompile/appcompile
	${CMAKE_BINARY_DIR}/tools/libuild/libuild
)
endif(TOOLS)
