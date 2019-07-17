#!/usr/bin/python3
from shutil import copyfile as cp
import os
import argparse

# Everything here is fragile

# command line options
parser = argparse.ArgumentParser(
                      description='Run MICM with a given tag',
                      formatter_class=argparse.ArgumentDefaultsHelpFormatter
                      )

parser.add_argument('-tag_id', type=str, required=True,
                    help='Source directory containing kinetics FORTRAN code')

args = parser.parse_args()


# where are we?
parent_dir = os.getcwd()

# checkout the code
os.system('git clone https://github.com/gold2718/ccpp-framework ')
os.chdir('ccpp-framework')
os.system('git checkout 8e89f83')
os.chdir('..')

os.system('git clone -b sparse_beta --depth 1 https://github.com/NCAR/MICM_chemistry')
os.system('git clone -b v1.0 --depth 1 https://github.com/NCAR/burrito Mechanism_collection')


# get a tag, environment_conditions, run it through the preprocessor, and place fortran in the right places in MICM_chemistry
os.chdir('Mechanism_collection')
os.system('/usr/bin/python3 burrito.py -overwrite True -tag_id ' + args.tag_id)
os.system('/usr/bin/python3 eat_it.py -source_dir configured_tags/' + args.tag_id)


# build and run
os.chdir('../MusicBox_host')
# source etc/CENTOS_setup.sh -- or -- source etc/Cheyenne_setup_intel.sh
os.environ["CC"] = "/opt/local/bin/gcc"
os.environ["CC"] = "/opt/local/bin/gcc"
os.environ["CXX"] = "/opt/local/bin/g++"
os.environ["F77"] = "/opt/local/bin/gfortran"
os.environ["F90"] = "/opt/local/bin/gfortran"
os.environ["FC"] = "/opt/local/bin/gfortran"
os.environ["NETCDF"] = "/opt/local"
os.environ["LD_LIBRARY_PATH"] = "/opt/local/lib64:/opt/local/lib"

print(os.environ.get('CC'))


# now build
os.system('rm -rf build; mkdir build')
os.chdir('build')
print(os.getcwd())
os.system('cmake3 ../CMakeLists.txt -S ../src -B . ')
os.system('make')

#run?
os.system('./MusicBox')

