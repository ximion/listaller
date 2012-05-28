# - This is an internal template to drive valac calls
#

# set up switch-yard for compile flags
set(VALA_COMPILE_FLAGS "")
set(VALA_COMPILE_FLAGS_DEBUG "-g")
set(VALA_COMPILE_FLAGS_RELEASE "")
set(VALA_COMPILE_FLAGS_RELWITHDEBINFO "-g")
set(VALA_COMPILE_FLAGS_MINSIZEREL "")

# figure out what CMAKE_CFG_INTDIR is at configure and build time
if(NOT "." STREQUAL ".")
  set(CFG "${CMAKE_CFG_INTDIR}")
else()
  set(CFG )
endif()
# select compile flags
if(VALA_COMPILE_FLAGS_${CFG})
  set(VALA_COMPILE_FLAGS
    "${VALA_COMPILE_FLAGS} ${VALA_COMPILE_FLAGS_${CFG}}")
endif()

# expand all references to CMAKE_CFG_INTDIR
set(command "${VALA_COMPILER};-C;-b;/home/matthias/Development/Listaller/listaller-devtools/apbuild;-d;/home/matthias/Development/Listaller/listaller-devtools/build/apbuild//${CMAKE_CFG_INTDIR};--pkg=posix;--pkg=glib-2.0;--pkg=gio-2.0;--pkg=gee-1.0;/home/matthias/Development/Listaller/listaller-devtools/apbuild/buildlist.vala")
set(out_files "/home/matthias/Development/Listaller/listaller-devtools/build/apbuild/buildlist.c")
if(NOT "." STREQUAL ".")
  string(REPLACE "." "${CMAKE_CFG_INTDIR}" command "${command}")
  string(REPLACE "." "${CMAKE_CFG_INTDIR}" out_files "${out_files}")
endif()

# make sure output-directories exist
set(dirs)
foreach(f IN LISTS out_files)
  get_filename_component(d "${f}" PATH)
  get_filename_component(d "${d}" ABSOLUTE)
  list(APPEND dirs "${d}")
endforeach()
list(REMOVE_DUPLICATES dirs)
foreach(d IN LISTS dirs)
  file(MAKE_DIRECTORY "${d}")
endforeach()

# tell user
if(VERBOSE)
  string(REPLACE ";" " " msg "${command}")
  message("${msg}")
endif()

# do the thing
execute_process(COMMAND
  ${command}
  RESULT_VARIABLE result
  OUTPUT_VARIABLE output
  ERROR_VARIABLE output
  )

# check whether the thing failed
if(result)
  message(FATAL_ERROR "${output}")
endif()
