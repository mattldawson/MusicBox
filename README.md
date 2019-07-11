# MusicBox
A Box (point) Chemistry kinetic solver using the CCPP framework and MICM

Please see https://wiki.ucar.edu/display/MusicBox/Quick+Start for details

get the MusicBox source code:
```
 git clone https://github.com/NCAR/MusicBox
 cd MusicBox
 manage_externals/checkout_externals
```    
get input data (environmental conditions), and configure a tag from the cafe-dev web server:
```
 burrito.py -tag_id 255
 eat_it.py -source_dir configured_tags/255
```
build steps:
```
 cd ../MusicBox_host
 source etc/CENTOS_setup.sh -- or -- source etc/Cheyenne_setup_intel.sh

 rm -rf build; mkdir build; cd build

 cmake3 ../CMakeLists.txt -S ../src -B . -DCMAKE_BUILD_TYPE=Debug (with debug)
 -- or --
 cmake3 ../CMakeLists.txt -S ../src -B .  (no debug)


 make
```
execute:
```
 ./MusicBox
```
