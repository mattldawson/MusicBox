# MusicBox
A Box (point) Gas Chemistry solver developed and maintained in the ACOM laboratory at the National Center for Atmospheric Research.

# Install and run test
## Install [docker](https://www.docker.com/get-started)

## Install [git](https://git-scm.com)

## Get the MusicBox source code
You need to copy the code base from the github repostory.  The manage_externals tool is an NCAR-specific tool used to collect other repositories on which this code base depends.
```
 git clone https://github.com/NCAR/MusicBox
 cd MusicBox
 ./manage_externals/checkout_externals
```

## Run MusicBox using Docker
In the MusicBox directory there is a Dockerfile. It contains a list of commands to build an environment (unix) and all the tools required to run the MusicBox application.  The following command-line tools build the environment, build the application, and then execute MusicBox.
```
docker build -t music-box-test . --build-arg TAG_ID=272
docker run -it music-box-test bash
cd MusicBox/MusicBox_host/build
./MusicBox
```
Results will be in `MusicBox/MusicBox_host/MusicBox_output.nc`
Do not exit the container yet.

## Look at the Results
```
docker ps
```
This will list all active containers.  The last column gives you the name of each container -- something nonsensical like "lucid_tereshkova".
Now you can copy the results from that container.
```
docker cp lucid_tereshkova:/MusicBox/MusicBox_host/MusicBox_output.nc .
```
Use your favorite tool for evaluating MusicBox_output.nc


# Users guide and additional details are being developed in a wiki
Please see the [MusicBox wiki](https://wiki.ucar.edu/display/MusicBox/Quick+Start) for additional details

