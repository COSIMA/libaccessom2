
# libaccessom2

libaccessom2 is a library that is linked into all of the ACCESS-OM2 component models, including YATM, CICE and MOM. libaccessom2 provides functionality used by all models as well as providing a interface to inter-model communication and synchronisation tasks. Using a common library reduces code duplication and provides a uniform way for all models to be integrated into ACCESS-OM2.

libaccessom2 functionality includes:
    * simplified interface to the OASIS3-MCT coupler API
    * date handling, logging and simple performance timers
    * configuration synchronisation between models
    * a single configuration file for common configs (accessom2.nml)

Further information about ACCESS-OM2 can be found in the [ACCESS-OM2 wiki](https://github.com/OceansAus/access-om2/wiki)

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

To further simplify things YATM gathers a lot of it's configuration automatically from the forcing dataset metadata. For example the calendar type and timestep information.

## River runoff remapping

It is difficult to regrid river runoff in a distributed memory system because moving runoff from a land point to the nearest ocean point may involve an interprocess communication. It makes more sense to regrid the river runoff within YATM since it is a single process application.

YATM regrids runoff in a two step process:

1. Apply a conservative regridding operation to move the runoff from the source grid to the ACCESS-OM2 ocean/ice grid. The remapping interpolation weights are calculated using [ESMF\_RegridWeightGen](https://www.earthsystemcog.org/projects/regridweightgen/) from [ESMF](https://www.earthsystemcog.org/projects/esmf/).
2. Find any runoff that the previous step has distributed to land points and move it to the nearest ocean neighbour. This is done using an efficient nearest neighbour data structure called a [k-dimensional tree](https://en.wikipedia.org/wiki/K-d_tree). The [kdtree2](https://github.com/jmhodges/kdtree2) Fortran package is used for this.

# Ice and ocean stubs

This repository also includes ice and ocean stubs. These executables can be used as a stand-in for the CICE and MOM for testing purposes.

# Build

How to build libaccessom2, YATM, ice\_stub and ocean\_stub:

```{bash}
mkdir build
cd build
cmake ../
```

# Run tests

```{bash}
cd tests/minimal
rm accessom2_restart_datetime.nml ; cp ../test_data/i2o.nc ./ ; cp ../test_data/o2i.nc ./ ; cp ../test_data/a2i.nc ./
time mpirun --mca orte_base_help_aggregate 0 --mca opal_abort_print_stack 1 -np 1  ../../build/bin/atm : -np 1 ../../build/bin/ice_stub : -np 1 ../../build/bin/ocean_stub
```

