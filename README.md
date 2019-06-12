# MusicBox
A Box (point) Chemistry kinetic solver using the CCPP framework and MICM

Please see https://wiki.ucar.edu/display/MusicBox/Quick+Start for details

#### DISTRIBUTE WILL NEED TO BE REWORKED - right now it uses Chapman_v3_1547831703456
## build steps:
##  cd MICM_chemistry
##  ./distribute_include_files.py /path/$CHEM_NAME.json
##  cd ..

 cd MusicBox_host/data 

 wget  ftp://ftp.acom.ucar.edu/micm_environmental_conditions/MusicBox_env_cond_c190109.nc; mv MusicBox_env_cond_c190109.nc env_conditions.nc
 -- or --
 wget  ftp://ftp.acom.ucar.edu/micm_environmental_conditions/MusicBox_env_cond_1col_c190109.nc; mv MusicBox_env_cond_1col_c190109.nc env_conditions.nc

 cd ../../
 cd MusicBox_host
 source etc/CENTOS_setup.sh -- or -- source etc/Cheyenne_setup_intel.sh
 rm -rf build
 mkdir build; cd build
 cmake3 ../CMakeLists.txt -S ../src -B . -DCMAKE_BUILD_TYPE=Debug (with debug)
 -- or -- 
 cmake3 ../CMakeLists.txt -S ../src -B .  (no debug)
 make

execute:
 ./MusicBox
