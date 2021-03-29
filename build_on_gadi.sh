#!/bin/bash

module purge
module load intel-compiler/2020.3.304
module load netcdf/4.7.4p
module load openmpi/4.0.2

MYDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Args: platform and whether Cmake should search for NetCDF libraries
source $MYDIR/build.sh nci OFF
