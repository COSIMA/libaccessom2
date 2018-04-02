# YATM

Yet another data-driven atmosphere model. Uses OASIS coupler to deliver atmospheric forcing fields to other models.

# Build

```{bash}
mkdir build
cd build
cmake ../
```

# Run tests

cd tests/minimal
cp ../test_data/i2o.nc ./ ; cp ../test_data/o2i.nc ./ ; cp ../test_data/a2i.nc ./
time mpirun --mca orte_base_help_aggregate 0 --mca opal_abort_print_stack 1 -np 1  ../../build/bin/atm : -np 1 ../../build/bin/ice_stub : -np 1 ../../build/bin/ocean_stub

# How does YATM handle dates?

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

# What am I doing now

CI tests (in priority):
    - Reproducing previous results (e.g. compare against current ACCESS-OM2 output).
    - RYF and IAF dates
    - RYF and IAF restarts
    - minimal fields this looks tricky because the dest is masked.
