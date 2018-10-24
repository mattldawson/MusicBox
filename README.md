# MusicBox
The MICM box model host

to add variable:
 - edit MusicBox_host/src/MusicBox_var_defs.f90
   . add line to host cap table
   . add variable to micm_data_type
 - add corresponding line to chemistry module cap tables
 - declare variable and allocate memory in host model

build steps:
1) edit the MusicBox.F90 file and set the mdoel variable to either terminator or 3component
2) ccpp-framework/scripts/ccpp_prebuild.py --model=MusicBox_terminator (or --model=MusicBox_3component)
3) cd MusicBox_host
4) mkdir bin
5) cd bin
6) cmake --DPROJECT=terminator ../src  (or --DPROJECT=3component) (may add -DCMAKE_BUILD_TYPE=Debug for debugging)
7) make

execute:
> ./MusicBox


NOTE:  As of 9/2018, if a --DPROJECT is not specified, will get the following error:
OSError: [Errno 2] No such file or directory: '/home/cacraig/MusicBox-mine/MusicBox_host/bin/ccpp-framework/schemes/check/src/check-build/../../../../../src/tests'

To fix it, include this line between the cmake and make steps
> ln -s ../../ccpp-framework/src . ; > ln -s ../../ccpp-framework/schemes .     # TEMPORARY WORKAROUND
