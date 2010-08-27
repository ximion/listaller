
macro(li_set_build_project project_name project_bin widget)
  if(${widget} MATCHES qt)
    set(ligui_build_args_gtk "-B" "--ws=qt")
    set(widget_f qt4)
  else()
    if(${widget} MATCHES gtk)
      set(ligui_build_args_gtk "-B" "--ws=gtk2")
      set(widget_f gtk2)
    else()
      message(FATAL_ERROR "Invalid widgetset passed!")
    endif()   
  endif()
  
  set(${project_bin}_${widget}_OUT "${CMAKE_BINARY_DIR}/${widget_f}/${project_bin}")
  add_custom_command(OUTPUT ${${project_bin}_${widget}_OUT}
	  COMMAND ${LAZBUILD_EXE}
	  ARGS ${ligui_build_args_gtk} ${project_name}
	  
	  COMMAND ${CMAKE_COMMAND}
	  ARGS -E rename ${CMAKE_BINARY_DIR}/${project_bin} ${CMAKE_BINARY_DIR}/${widget_f}/${project_bin}
	  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
              
	  MAIN_DEPENDENCY ${project_name}
	  DEPENDS ${${project_bin}_base_sources}
  )
endmacro(li_set_build_project)
