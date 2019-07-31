# MusicBox
A Box (point) Chemistry kinetic solver using the CCPP framework and MICM

Please see https://wiki.ucar.edu/display/MusicBox/Quick+Start for details

## Simple, fragile script
If you want to run with cafe-devel tag 265
```
 git clone https://github.com/NCAR/MusicBox
 cd MusicBox
 run_it.py -tag_id 265
```

## More robust process

get the MusicBox source code:
```
 git clone https://github.com/NCAR/MusicBox
 cd MusicBox
 manage_externals/checkout_externals
```    
get input data (environmental conditions), and configure a tag from the cafe-dev web server:
```
 cd Mechanism_collection
 python3 burrito.py -tag_id 265
 python3 eat_it.py -source_dir configured_tags/265
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

Users may find it informative to check out the options in the tag collection process:
```
python3 burrito.py --help
python3 eat_it.py --help
```

## Adding a new scheme

The steps to adding a new MusicBox scheme are as follows:
* Create XXX.F90 and XXX.meta files (the fortran code and its accompanying metadata file)
* Add the XXX.meta file to MusicBox_scheme_files.txt
* Add the scheme to MusicBox_suite.xml in the order in which it is to be run
