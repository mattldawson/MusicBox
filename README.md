# MusicBox
A Box (point) Gas Chemistry solver developed and maintained in the ACOM laboratory at the National Center for Atmospheric Research.

# Install and run test
## Install [docker](https://www.docker.com/get-started)

## Install [git](https://git-scm.com)

## Get the MusicBox source code
Open a terminal window, navigate to the directory where you would like to save MusicBox, and run:

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

If you're done with MusicBox and would like to remove the containers and the image we built, in any terminal window run:

```
docker system prune
docker rmi music-box-test
```

# Configure, install, and run MusicBox

## Get the MusicBox source code

If you have already cloned the MusicBox git repository, you can skip this step. If not, open a terminal window, navigate to the directory where you would like to save MusicBox, and run:

```
 git clone --recurse-submodules https://github.com/NCAR/MusicBox
 cd MusicBox
```

## Build and run the Docker container

We will build the container the same way we did for the standard test. In a terminal window, navigate to the `MusicBox/` folder and run:

```
docker build -t music-box-test . --build-arg TAG_ID=chapman
```

Now, we will run a slightly modified `docker run` command:

```
docker run -p 8000:8000 -it music-box-test bash
```

Notice that `-t` has been replaced by `-it`. This tells Docker that instead of just running the container, we would like to "interact" with the container as it's running. The `bash` command at the end tells Docker to instead of running the default executable (which is the web server that allows us to run and analyze model results in a browser), it should start a BASH shell in this terminal window.

## Notes for working in the container

Before you modify anything, note that everything you do in the container, stays in the container. The only exception is when we run the web server (described below), because we explicitly told Docker to connect port 8000 of the container to port 8000 on our local machine with the `-p 8000:8000`.

All the files you modify in the container will remain in the container unless you copy them out, but this must be done from outside the container.

Anything you install in the container is not installed on your local machine. So, if your favorite tool is available from the Fedora package manager (Fedora in the OS we chose for the container), you can install it with:

```
dnf install -y my_favorite_tool
```

It will be available as long as you're working in the container, but it will not be installed on your local machine. (And only text-based tools can be used in the container, unless you go through the processes of setting up X-11 for the container.)

We will describe how to save your modifications below.

## Configure and run the model

If you type `ls`, you will see the contents of the container:

```
MechanismToCode  bin   dev  home  lib64       media  music_box_interactive  proc  run   srv  tmp  var
MusicBox         boot  etc  lib   lost+found  mnt    opt                    root  sbin  sys  usr
```

Refer to the MusicBox documentation for instructions on how to modify the model conditions (in `MusicBox/MusicBox_host/`) environmental conditions (in `MusicBox/MusicBox_host/data/`) or the MusicBox or MICM source code (in `MusicBox/MusicBox_host/src/` and `MusicBox/MICM_chemistry/src/`). If you make changes to the source code, navigate to `MusicBox/MusicBox_host/build/` and rebuild the model with:

```
cmake ../src
make
```

## Start the web server and run the model

Now that the model configuration is finished, we need to start the web server so that we can run the model and analyze results. (Remember that we told `docker run` to run the BASH shell instead of its default executable, the web server.) Return to the root directory, and run the server:

```
cd /
python3 music_box_interactive/manage.py runserver 0.0.0.0:8000
```

## Run the model and analyze results

In a web browser, navigate to [localhost:8000](http://localhost:8000) to run the model and analyze the results.

## Shut down the server

In the terminal window where you started MusicBox server in, type `ctrl-C` to shut down the server when you are done.

To exit the container, type `exit`. If you get a warning about stopped processes, type `exit` once more.

## Save or discard the modifications you made

After you exit the container, if you were to rerun:

```
docker run -p 8000:8000 -it music-box-test bash
```

you would enter a container that does not contain any of the modifications you made. This is because the `music-box-test` tag still points to the image created by the `docker build` command. If you would like to save the changes you made while in the container, you can give your modified image a tag as well. After exiting the container, you can run:

```
docker ps -l
```

from any terminal window to see containers that you have exited. It will look something like:

```
CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS                     PORTS               NAMES
b4f0e161ae64        music-box-test      "bash"              30 minutes ago      Exited (0) 2 minutes ago                       dreamy_chandrasekhar
```

We will create an image that includes your modifications with the name `my-music-box` using your container id `b4f0e161ae64` as follows:

```
docker commit b4f0e161ae64 my-music-box
```

If you would like to see what images are available on your system, you can run `docker images`. Now, to run a BASH shell in your modified image you can run:

```
docker run -p 8000:8000 -it my-music-box bash
```

... or, you can just run the web server for your modified image in the background with:

```
docker run -p 8000:8000 -t my-music-box python3 music_box_interactive/manage.py runserver 0.0.0.0:8000
```
... and navigate to [localhost:8000](http://localhost:8000) to rerun the model or analyze the output. (Note that the modified image does not default to running the web server, so we must specify to run it.)

Remember that when you run:

```
docker system prune
```

... you remove **all** stopped containers. So, if you have not committed your changes to an image, they will be lost. You can remove the images with:

```
docker rmi music-box-test
docker rmi my-music-box
```


# Users guide and additional details are being developed in a wiki
Please see the [MusicBox wiki](https://wiki.ucar.edu/display/MusicBox/Quick+Start) for additional details

