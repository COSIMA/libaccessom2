#!/bin/bash -x

PLATFORM=$1

MYDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $MYDIR && \
mkdir -p build && \
cd build && \
cmake -DPLATFORM=$PLATFORM ../ && \
make VERBOSE=1; \

cd -
