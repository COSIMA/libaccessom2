#!/bin/bash

module purge
module load cmake/3.6.2
module load netcdf/4.4.1.1
module load intel-fc/17.0.1.132
module load openmpi/1.10.2

source ./build.sh nci
