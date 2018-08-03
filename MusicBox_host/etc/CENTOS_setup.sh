#!/bin/bash

echo "Setting environment variables for SCM-CCPP on CENTOS with gcc/gfortran"

export CC=/opt/local/bin/gcc
export CXX=/opt/local/bin/g++
export F77=/opt/local/bin/gfortran
export F90=/opt/local/bin/gfortran
export FC=/opt/local/bin/gfortran

export NETCDF=/opt/local
export LD_LIBRARY_PATH="/opt/local/lib64:/opt/local/lib"
