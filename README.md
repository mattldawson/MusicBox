# MusicBox
The MICM box model host

to add variable:
 - edit MusicBox_host/src/MusicBox_var_defs.f90
   . add line to host cap table
   . add variable to micm_data_type
 - add corresponding line to chemistry module cap tables
 - declare variable and allocate memory in host model

build steps:
1) cp /terminator-data1/fvitt/micm_inputs/FW2000climo.f09_f09_mg17.cam6_0_030.n01.cam.h2.0001-01-01-00000.nc MusicBox_host/data/env_conditions.nc
2) ccpp-framework/scripts/ccpp_prebuild.py --model=MusicBox_terminator (or --model=MusicBox_3component or --model=MusicBox_user-defined )
3) cd MusicBox_host
> source etc/CENTOS_setup.sh
4) mkdir bin
5) cd bin
6) cmake -DPROJECT=terminator ../src  (or -DPROJECT=3component or -DPROJECT=user-defined) (may add -DCMAKE_BUILD_TYPE=Debug for debugging)
7) make

execute:
> ./MusicBox


NOTE:  As of 9/2018, if a -DPROJECT is not specified, will get the following error:
OSError: [Errno 2] No such file or directory: '/home/cacraig/MusicBox-mine/MusicBox_host/bin/ccpp-framework/schemes/check/src/check-build/../../../../../src/tests'

To fix it, include this line between the cmake and make steps
> ln -s ../../ccpp-framework/src . ; ln -s ../../ccpp-framework/schemes .     # TEMPORARY WORKAROUND
