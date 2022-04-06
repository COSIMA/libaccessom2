
| GitHub CI | Travis CI | Jenkins Tests |
|:----------------:|:----------------:|:------------------:|
|[![GitHub Build Status](https://github.com/COSIMA/libaccessom2/workflows/CI/badge.svg)](https://github.com/COSIMA/libaccessom2/actions?query=workflow%3ACI)|[![Build Status](https://travis-ci.org/COSIMA/libaccessom2.svg?branch=master)](https://travis-ci.org/COSIMA/libaccessom2) |[![Test Status](https://accessdev.nci.org.au/jenkins/buildStatus/icon?job=ACCESS-OM2/libaccessom2)](https://accessdev.nci.org.au/jenkins/job/ACCESS-OM2/job/libaccessom2/)|

# libaccessom2

libaccessom2 is a library that is linked into all of the ACCESS-OM2 component models, including YATM, CICE and MOM. libaccessom2 provides functionality used by all models as well as providing a interface to inter-model communication and synchronisation tasks. Using a common library reduces code duplication and provides a uniform way for all models to be integrated into ACCESS-OM2.

libaccessom2 functionality includes:
- simplified interface to the OASIS3-MCT coupler API
- date handling, logging and simple performance timers
- configuration synchronisation between models
- a single configuration file for common configs (accessom2.nml)

Further information about ACCESS-OM2 can be found in the [ACCESS-OM2 wiki](https://github.com/COSIMA/access-om2/wiki)

## Downloading

This respository contains submodules, so you will need to clone it with the `--recursive` flag:
```
git clone --recursive https://github.com/COSIMA/libaccessom2.git
```

To update a previous clone of this repository to the latest version, you will need to do 
```
git pull
```
followed by
```
git submodule update --init --recursive
```
to update all the submodules.

## Configuration

libaccessom2 has a single configuration file called `accessom2.nml` which is usually found in the top-level of the model configuration directory (also known as the `control` directory). This configuration contains model-wide configuration. Presently the options most important and worthy of explanation are:

* `forcing_start_date` the date (and time) when forcing begins.
* `forcing_end_date` the start (and time) at which the forcing ends. The time between the `forcing_start_date` and `forcing_end_date` is called the forcing period. The model will be forced by a continuous repetition of this period.
* `restart_period`: interval of time between successive model restarts. This is provided as a tuple: years, months, seconds. This breaks the entire experiment into a collection of runs or segments.

These is no configruation option that controls when an experiment ends, it will simply continue until it is stopped.

# YATM

This repository also includes YATM (Yet another data-driven atmosphere model). The purpose of YATM is to keep track of the current model time, then read current atmospheric forcing data and deliver it to the rest of the model via the coupler (OASIS3-MCT).

YATM uses two configuration files: `atm.nml`, and `forcing.json`. The latter is used to define details of the model coupling fields. The former is only used to configure the river runoff remapping (discussed below).

## Date handling

A unique feature of YATM is that it does not read forcing data by iterating over records. That is, the code does not explicitly read and deliver the 1st forcing record followed by the 2nd etc. The reason for this is that when accounting for complications such as different calendar types, fields with different periods, restarts, etc. this approach can quickly become complex and is error prone. Instead YATM iterates over datetime objects. At the current date (and time) YATM finds all matching forcing fields, reads them from disk, delivers them the coupler and then incrementes current date (and time). This simplification has led to much more concise and easy to understand code.

To further simplify things YATM gathers a lot of its configuration automatically from the forcing dataset metadata. For example the calendar type and timestep information.

## River runoff remapping

It is difficult to regrid river runoff in a distributed memory system because moving runoff from a land point to the nearest ocean point may involve an interprocess communication. It makes more sense to regrid the river runoff within YATM since it is a single process application.

YATM regrids runoff in a two step process:

1. Apply a conservative regridding operation to move the runoff from the source grid to the ACCESS-OM2 ocean/ice grid. The remapping interpolation weights are calculated using [ESMF\_RegridWeightGen](https://www.earthsystemcog.org/projects/regridweightgen/) from [ESMF](https://www.earthsystemcog.org/projects/esmf/).
2. Find any runoff that the previous step has distributed to land points and move it to the nearest ocean neighbour. This is done using an efficient nearest neighbour data structure called a [k-dimensional tree](https://en.wikipedia.org/wiki/K-d_tree). The [kdtree2](https://github.com/jmhodges/kdtree2) Fortran package is used for this.

# Ice and ocean stubs

This repository also includes ice and ocean stubs. These are stand-ins for the the ice and ocean models. They demonstrate how libaccessom2 can be used and are also very useful for testing.

# Build

How to build libaccessom2, YATM, ice\_stub and ocean\_stub on gadi (NCI):

```{bash}
git clone --recursive https://github.com/COSIMA/libaccessom2.git
cd libaccessom2
./build_on_gadi.sh
```

# Run tests on Gadi (NCI)

First do build as above. Then to get some computer resources:

```{bash}
qsub -I -P x77 -q normal -lncpus=4 -lmem=16Gb -lwalltime=3:00:00 -lstorage=gdata/ua8+gdata/qv56+gdata/hh5+gdata/ik11+gdata/v45+gdata/rt52
```

The tests: `JRA55_IAF JRA55_IAF_SINGLE_FIELD JRA55_RYF JRA55_RYF_MINIMAL JRA55_v1p4_IAF ERA5` can all be run manually as follows. Replace `JRA55_IAF` with the test to be run.

```{bash}
export LIBACCESSOM2_ROOT=$(pwd)
module load openmpi
cd tests/
./copy_test_data.sh
cd JRA55_IAF
rm -rf log ; mkdir log ; rm -f accessom2_restart.nml ; cp ../test_data/i2o.nc ./ ; cp ../test_data/o2i.nc ./
export UCX_LOG_LEVEL=error; mpirun -np 1 $LIBACCESSOM2_ROOT/build/bin/yatm.exe : -np 1 $LIBACCESSOM2_ROOT/build/bin/ice_stub.exe : -np 1 $LIBACCESSOM2_ROOT/build/bin/ocean_stub.exe
```

If Python3 and pytest are installed then all of the above and some additional tests can be run with:

```{bash}
module load openmpi
python -m pytest tests/
```

Any individual pytest test can be run using pytest as follows:

```{bash}
module load openmpi
python -m pytest test_stubs.py::TestStubs::test_field_scaling
```

The above is the only way to run the `FORCING_SCALING` test case because it relies on the Python test code to create one of the inputs.


