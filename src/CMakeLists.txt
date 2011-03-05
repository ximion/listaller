# CMakeLists for Listaller source

set(lisrc ${CMAKE_SOURCE_DIR}/src CACHE INTERNAL "Listaller sourcecode dir")

set(LISTALLER_BASE_SOURCES ${lisrc}/util.vala
	${lisrc}/settings.vala
	${lisrc}/items.vala
	${lisrc}/database.vala
	${lisrc}/ipkpackage.vala
	CACHE INTERNAL "Listaller core source files"
)

# Variables used later to set library versioning
set(LILIBS_FULL_VERSION "${LISTALLER_VERSION_MAJOR}.${LISTALLER_VERSION_MINOR}.${LISTALLER_VERSION_PATCH}")
set(LILIBS_VERSION "${LISTALLER_VERSION_MAJOR}.${LISTALLER_VERSION_MINOR}")
set(LILIBS_SOVERSION "${LISTALLER_VERSION_MAJOR}")

add_subdirectory (listaller)
if(QT)
	add_subdirectory(listaller-qt)
endif(QT)
