# MusicBox
The MICM box model host

to add variable:
 - edit MusicBox_host/src/MusicBox_var_defs.f90
   . add line to host cap table
   . add variable to micm_data_type
 - add corresponding line to chemistry module cap tables
 - declare variable and allocate memory in host model

build steps:
> ./ccpp-framework/scripts/ccpp_prebuild.py --model=MusicBox
> cd MusicBox_host
> mkdir bin
> cd bin
> cmake ../src  (or cmake -DCMAKE_BUILD_TYPE=Debug ../src)
> make

execute:
> ./MusicBox
