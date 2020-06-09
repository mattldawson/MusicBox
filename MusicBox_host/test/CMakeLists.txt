#-----------------------------------------------------------------------------
#
# Testing for MusicBox
#
#-----------------------------------------------------------------------------

add_executable(util_string util/string.F90 ../src/util/string.F90 ../src/util/assert.F90)
add_test(NAME util_string COMMAND util_string)

