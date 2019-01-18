# MusicBox
A Box (point) Chemistry kinetic solver using the CCPP framework and MICM

Please see https://wiki.ucar.edu/display/MusicBox/Quick+Start for details

get the MusicBox source code:
```
 git clone -b MusicBox01_0000 https://github.com/NCAR/MusicBox
 cd MusicBox
 manage_externals/checkout_externals
```    
git input data:
```
 cd MusicBox_host/data 
 wget ftp://ftp.acom.ucar.edu/micm_environmental_conditions/MusicBox_env_cond_c190109.nc; mv MusicBox_env_cond_c190109.nc env_conditions.nc
 -- or --
 wget ftp://ftp.acom.ucar.edu/micm_environmental_conditions/MusicBox_env_cond_1col_c190109.nc; mv MusicBox_env_cond_1col_c190109.nc env_conditions.nc
 -- or --
 wget ftp://ftp.acom.ucar.edu/micm_environmental_conditions/MusicBox_env_cond_1col_5days_repeated_c190118.nc; mv MusicBox_env_cond_1col_5days_repeated_c190118.nc env_conditions.nc
 cd ../../
```
build steps:
```
 cd MICM_chemistry
 ./distribute_include_files.py /path/$CHEM_NAME.json
 cd ..

 ccpp-framework/scripts/ccpp_prebuild.py --model=MusicBox
 cd MusicBox_host
 source etc/CENTOS_setup.sh -- or -- source etc/Cheyenne_setup_intel.sh
 rm -rf $CHEM_NAME
 mkdir $CHEM_NAME
 cd $CHEM_NAME
 cmake -DPROJECT=$CHEM_NAME ../src  (or -DPROJECT=3component or -DPROJECT=terminator) (may add -DCMAKE_BUILD_TYPE=Debug for debugging)
 make
```
execute:
```
 ./MusicBox
```