# matm

A data-driven atmosphere model. Uses OASIS coupler to deliver atmospheric forcing fields to other models.

# Build

```{bash}
mkdir build
cd build
cmake ../
```

# Run tests

# What am I doing now

Set up a test case that passes fields from atm to the ice\_stub printing out checksums as it goes. Then compare these to the checksums from the ACCESS-OM2 model.

This will require changes to the coupler so that it can be used by both atm and the ice\_stub.
