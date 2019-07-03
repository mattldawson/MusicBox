# MusicBox
A Box (point) Chemistry kinetic solver using the CCPP framework and MICM

Please see https://wiki.ucar.edu/display/MusicBox/Quick+Start for details

get the MusicBox source code:
```
 git clone https://github.com/NCAR/MusicBox
 cd MusicBox
 manage_externals/checkout_externals
```    
get input data:
```
 cd MusicBox_host/data 
 wget  ftp://ftp.acom.ucar.edu/micm_environmental_conditions/One_day_cycle_WACCM_samples_c20180626.nc; mv One_day_cycle_WACCM_samples_c20180626.nc env_conditions.nc
 -- or --
 wget  ftp://ftp.acom.ucar.edu/micm_environmental_conditions/Equatorial_Pacific_column_c20180626.nc; mv Equatorial_Pacific_column_c20180626.nc env_conditions.nc
```
build steps:
```
#### DISTRIBUTE WILL NEED TO BE REWORKED - right now it uses Chapman_v3_1547831703456
## build steps:
##  cd MICM_chemistry
##  ./distribute_include_files.py /path/$CHEM_NAME.json
##  cd ..

 cd ../../
 cd MusicBox_host
 source etc/CENTOS_setup.sh -- or -- source etc/Cheyenne_setup_intel.sh
 rm -rf build
 mkdir build; cd build
 cmake3 ../CMakeLists.txt -S ../src -B . -DCMAKE_BUILD_TYPE=Debug (with debug)
 -- or --
 cmake3 ../CMakeLists.txt -S ../src -B .  (no debug)


 make
```
execute:
```
 ./MusicBox
```
