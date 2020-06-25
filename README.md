# MusicBox
A Box (point) Gas Chemistry solver developed and maintained in the ACOM laboratory at the National Center for Atmospheric Research.

# Install and run test
## Install [docker](https://www.docker.com/get-started)

## Install [git](https://git-scm.com)

## Get the MusicBox source code
```
 git clone https://github.com/NCAR/MusicBox
 cd MusicBox
```

## Run MusicBox using Docker
In the MusicBox directory there is a Dockerfile. It contains a list of commands to build an environment (unix) and all the tools required to run the MusicBox application.  The following command-line tools build the environment, build the application, and then execute MusicBox.

```
docker build -t music-box-test . --build-arg TAG_ID=272
docker run -it music-box-test bash
cd MusicBox/MusicBox_host/build
./MusicBox
```

Results will be in `MusicBox/MusicBox_host/MusicBox_output.nc`.

Do not exit the container yet.

## Plot output

To see the variables included in the output file, in the `build/` folder you can run:

```
ncdump -h ../MusicBox_output.nc
```

To generate simple plots from the MusicBox output, you can use the `plot.py` script in the `build/` folder:

```
python3 plot.py O3
python3 plot.py O3 CO2
python3 plot.py rate_O_O3_1
```

These commands generate plots in `MusicBox/MusicBox_host/`. You can include as many variables as you want from the output file in a single plot, but they should all have the same units because they will be plotted on the same y axis.

Do not exit the container yet.

## Copy results

In a separate terminal window, run the following

```
docker ps
```

This will list all active containers.  The last column gives you the name of each container&mdash;something nonsensical like "lucid_tereshkova".
Now you can copy the results from that container.

```
docker cp lucid_tereshkova:/MusicBox/MusicBox_host/MusicBox_output.nc .
```

You can also copy out any plots you made.

```
docker cp lucid_tereshkova:/MusicBox/MusicBox_host/plot_O3.pdf .
docker cp lucid_tereshkova:/MusicBox/MusicBox_host/plot_O3_CO2.pdf .
docker cp lucid_tereshkova:/MusicBox/MusicBox_host/plot_rate_O_O3_1.pdf .
```

# Users guide and additional details are being developed in a wiki
Please see the [MusicBox wiki](https://wiki.ucar.edu/display/MusicBox/Quick+Start) for additional details

