#!/bin/bash

PLATFORM=$1
if [ -z "$2" ]
then
    FIND_NETCDF=ON
else
    FIND_NETCDF=$2
fi

MYDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $MYDIR && \
mkdir -p build && \
cd build && \
cmake -DPLATFORM=$PLATFORM -DFIND_NETCDF=$FIND_NETCDF ../ && \
make VERBOSE=1; && \
cd -
