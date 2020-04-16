#!/bin/bash

mkdir -p test_data

# Copy over OASIS remap weights
cp /g/data/ik11/inputs/access-om2/input_rc/common_1deg_jra55/* test_data/

# Copy over river runoff remap weights
cp /g/data/ik11/inputs/access-om2/input_rc/yatm_1deg/rmp_jrar_to_cict_CONSERV.nc test_data/

# Copy ice grid and OASIS restarts
cp /g/data/ik11/inputs/access-om2/input_rc/cice_1deg/* test_data/
