#-----------------------------------------------------------------------------
#
# Testing for MusicBox
#
#-----------------------------------------------------------------------------

add_executable(util_string util/string.F90 stubs/ccpp_kinds.F90 ../src/util/string.F90
  ../src/util/assert.F90)
add_test(NAME util_string COMMAND util_string)

add_executable(util_convert util/convert.F90 stubs/ccpp_kinds.F90 ../src/util/convert.F90
  ../src/util/string.F90 ../src/util/constants.F90 ../src/util/assert.F90)
add_test(NAME util_convert COMMAND util_convert)

