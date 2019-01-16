# MusicBox
A Box (point) Chemistry kinetic solver using the CCPP framework and MICM

Please see https://wiki.ucar.edu/display/MusicBox/Quick+Start for details

build steps:
> cp /terminator-data1/fvitt/micm_inputs/MusicBox_env_cond_1col_c190109.nc MusicBox_host/data/env_conditions.nc
> ccpp-framework/scripts/ccpp_prebuild.py --model=MusicBox
> cd distribute_include_files.py /path/$CHEM_NAME.json
> cd ..
> cd MusicBox_host
> source etc/CENTOS_setup.sh
> mkdir bin
> cd bin
> cmake -DPROJECT=$CHEM_NAME ../src  (or -DPROJECT=3component or -DPROJECT=terminator) (may add -DCMAKE_BUILD_TYPE=Debug for debugging)
> make

execute:
> ./MusicBox