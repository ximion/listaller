FILE(REMOVE_RECURSE
  "buildlist.c"
  "CMakeFiles/buildlist_precompile"
  "CMakeFiles/buildlist_precompile.dir/precompile.stamp"
)

# Per-language clean rules from dependency scanning.
FOREACH(lang)
  INCLUDE(CMakeFiles/buildlist_precompile.dir/cmake_clean_${lang}.cmake OPTIONAL)
ENDFOREACH(lang)
