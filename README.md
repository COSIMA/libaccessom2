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

1. The experiment datetime. This is the current date and time according to the model. It is simply a counter that increments according to the timestep (and the calendar type).
2. The forcing datetime. This is the current date and time of the forcing used to drive the model.

Typically these two datetimes will have the same month, day, hour, minute, and second but may differ in the year. For example a Repeat Year Forcing experiment may start with experiment and forcing datetime of 1990-01-01T00:00:00 but as the run progresses to the second year the experiment datetime will be 1991 but the forcing datetime will go back to 1990.

Here is a description of all the namelist variables used to control the datetimes:
    * exp\_start\_datetime: the datetime at which an experiment started. This must match a date in the forcing dataset.
    * restart\_period: interval of time between successive model restarts. This is provided as a tuple: years, months, seconds. This breaks the entire experiment into a collection of segments.
    * forcing\_period: interval of time that a forcing runs before being repeated. This is provided as a tuple: years, months, seconds.

Notice that there is no `exp\_end\_datetime`.

Other key variables in datetime management are:
    * exp\_cur\_datetime, forcing\_cur\_datetime: the
    * dt: the timestep, this is not set by the user but is read from the forcing files.
    * calendar: 'noleap' or 'gregorian' calendar, once again not controlled by the user but read from forcing files.

# What am I doing now

Test datetime handling so that there


