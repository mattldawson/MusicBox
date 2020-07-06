# MusicBox
A Box (point) Gas Chemistry solver developed and maintained in the ACOM laboratory at the National Center for Atmospheric Research.

# Install and run test
## Install [docker](https://www.docker.com/get-started)

## Install [git](https://git-scm.com)

## Get the MusicBox source code
From a terminal window, type in the following commands.
```
 git clone --recurse-submodules https://github.com/NCAR/MusicBox
 cd MusicBox
```

## Start the MusicBox server using Docker
In the MusicBox directory there is a Dockerfile. It contains a list of commands to build an environment (unix) and all the tools required to run the MusicBox application.  The following command-line tools build the environment, build the application, and then start the MusicBox server. (You will see a lot of text flying past, but everything is being installed in the Docker container and can be removed at after you're done using MusicBox.)

```
docker build -t music-box-test . --build-arg TAG_ID=chapman
docker run -p 8000:8000 -t music-box-test
```

Leave the terminal window open while you are working with MusicBox.

## Run MusicBox

In a web browser, navigate to [localhost:8000](http://localhost:8000) to access the MusicBox options.

## Shut down the server

In the terminal window where you started MusicBox server in, type `ctrl-C` to shut down the server when you are done.

## Removing the Docker container

If you're done with MusicBox and would like to remove the container that was built in the first step, in any terminal window run:

```
docker system prune
```

# Users guide and additional details are being developed in a wiki
Please see the [MusicBox wiki](https://wiki.ucar.edu/display/MusicBox/Quick+Start) for additional details

