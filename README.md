# matm

A data-driven atmosphere model. Uses OASIS coupler to deliver atmospheric forcing fields to other models.

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

# What am I doing now

Doing full JRA55 RYF and IAF forcing tests. Sorting out issues with short atm timestep.
