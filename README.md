# MusicBox
A Box (point) Chemistry kinetic solver using the CCPP framework and MICM

Please see https://wiki.ucar.edu/display/MusicBox/Quick+Start for details

## Using Docker 
### Install docker on your machine, preferably on one that is on the NCAR network (or can VPN onto that network)
https://docs.docker.com/docker-for-mac/install/
### For graphics
If you have no way to plot your data on your machine, this installation includes ncview but you will need to allow xwindows to operate
```On a Mac
Open XQuartz, 
set Preferences->Security->Allow connections from network clients to true, 
restart XQuartz, 
```
### Build and run model
```
docker build -t music-box-test . --build-arg TAG_ID=272
xhost + 127.0.0.1
docker run -it -e DISPLAY=host.docker.internal:0 music-box-test bash
cd MusicBox/MusicBox_host/build
./MusicBox
ncview ../MusicBox_output.nc
```
If you have your own way to display data, the xhost command, the -e DISPLAY, and the ncview commands do not need to be executed.



## Manual process for those wanting to run the code outside Docker

get the MusicBox source code:
```
 git clone https://github.com/NCAR/MusicBox
 cd MusicBox
 ./manage_externals/checkout_externals
```
get input data (environmental conditions), and configure a tag from the cafe-dev web server:

```
 cd Mechanism_collection
 python3 get_environmental_conditions.py
 python3 get_tag.py -tag_id 272
 python3 preprocess_tag.py -mechanism_source_path configured_tags/272
 python3 stage_tag.py -soure_dir_kinetics configured_tags/272
```
build steps:
```
 cd MusicBox_host
 source etc/CENTOS_setup.sh -- or -- source etc/Cheyenne_setup_intel.sh

 rm -rf build; mkdir build; cd build

 cmake3 ../src/CMakeLists.txt -S ../src -B . -DCMAKE_BUILD_TYPE=Debug (with debug)
 -- or --
 cmake3 ../src/CMakeLists.txt -S ../src -B .  (no debug)

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
* Use "type = scheme" in the metadata file
* Add the XXX.meta file to MusicBox_scheme_files.txt
* Add the scheme to MusicBox_suite.xml in the order in which it is to be run

## Adding a new ddt
The steps to adding a new ddt to MusicBox are as follows:
* Create XXX.F90 and XXX.meta files (a fortran module that contains the ddt and its accompanying metadata file)
* The metdata file will use "name = Name_of_your_ddt" and "type = ddt". 
* Each element in the ddt will be documented with its local name, standard name, type, etc.
* Add the XXX.meta file to MusicBox_ddt_files.txt
* Add the ddt to MusicBox_mod.F90 and MusicBox_mod.meta
* The ddt can then be passed in/out and "use"d within any module 

## Run MusicBox using Docker

To build and run MusicBox using Docker:
```
docker build -t music-box-test . --build-arg TAG_ID=272
docker run -it music-box-test bash
cd MusicBox/MusicBox_host/build
./MusicBox
```
Results will be in `MusicBox/MusicBox_host/MusicBox_output.nc`

## Building the documentation

Install [Doxygen](http://www.doxygen.nl/) if you don't already have it.

To build the MusicBox documentation, from the root MusicBox folder:
```
cd doc
doxygen
```
then open `doc/build/html/index.html` in a browser
