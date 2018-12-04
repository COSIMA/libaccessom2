
# libaccessom2

libaccessom2 is a library that is linked into all of the ACCESS-OM2 component models, including YATM, CICE and MOM. libaccessom2 provides functionality used by all models as well as providing a interface to inter-model communication and synchronisation tasks. Using a common library reduces code duplication and provides a uniform way for all models to be integrated into ACCESS-OM2.

libaccessom2 functionality includes:
    * simplified interface to the OASIS3-MCT coupler API
    * date handling, logging and simple performance timers
    * configuration synchronisation between models
    * a single configuration file for common configs (accessom2.nml)

Further information about ACCESS-OM2 can be found in the [ACCESS-OM2 wiki](https://github.com/OceansAus/access-om2/wiki)

## Date handling and other configuration

The model tracks two datetime variables throughout a run:

1. The forcing datetime. This is the current date and time of the forcing used to drive the model.
2. The experiment datetime. This is the current date and time according to the model. It is simply a counter that continuously increments according to the timestep (and the calendar type).

Typically these two datetimes will have the same month, day, hour, minute, and second but may differ in the year. For example a Repeat Year Forcing experiment may start with experiment and forcing datetime of 1990-01-01T00:00:00 but as the run progresses to the second year the experiment datetime will be 1991 but the forcing datetime will go back to 1990.

Here is a description of all the namelist variables used to control datetime variables:

* forcing\_start\_datetime, forcing\_end\_datetime: the start and end of the forcing period. The model will be forced by a continuous repetition of the forcing period.
* restart\_period: interval of time between successive model restarts. This is provided as a tuple: years, months, seconds. This breaks the entire experiment into a collection of segments.

Other key variables in datetime management are:

* exp\_start\_datetime: the experiment start datetime. this is defined to be the same as the forcing\_period\_start\_datetime and cannot be set by the user.
* exp\_cur\_datetime, forcing\_cur\_datetime: the current experiment and forcing datetime. These are maintained by the model including across restarts and cannot be set by the user.
* dt: the timestep, this is not set by the user but is read from the forcing files.
* calendar: 'noleap' or 'gregorian' calendar, once again not set by the user but read from forcing files.


# YATM

This repository also includes YATM (Yet another data-driven atmosphere model). The purpose of YATM is to keep track of the current model time, then read current atmospheric forcing data and deliver it to the rest of the model via the coupler (OASIS3-MCT).

## River runoff remapping

It is difficult to regrid river runoff in a distributed memory system because moving runoff from a land point to the nearest ocean point may involve an interprocess communication. It makes more sense to regrid the river runoff within YATM since it is a single process application.

YATM regrids runoff in a two step process:
    1. Apply a conservative regridding operation to move the runoff from the source grid to the ACCESS-OM2 ocean/ice grid. The grid remapping interpolation weights are calculated using [ESMF\_RegridWeightGen](https://www.earthsystemcog.org/projects/regridweightgen/) from [ESMF](https://www.earthsystemcog.org/projects/esmf/).
    2. Find any runoff that the previous step has distributed to land points and move it to the nearest ocean neighbour. This is done using an efficient nearest neighbour data structure called a [k-dimensional tree](https://en.wikipedia.org/wiki/K-d_tree). The [kdtree2](https://github.com/jmhodges/kdtree2) Fortran package is used for this.

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

