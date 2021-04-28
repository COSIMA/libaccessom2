
import pytest
from pathlib import Path
from jinja2 import Template
import random
import subprocess as sp
import json
import netCDF4 as nc
import numpy as np
from utils import create_nc_file

forcing_tmpl = Template("""
{
  "description": "JRA55-do V1.3 RYF 1990-91 forcing",
  "inputs": [
    {
      "filename": "/g/data/ua8/JRA55-do/RYF/v1-3/RYF.rsds.1990_1991.nc",
      "fieldname": "rsds",
      "cname": "swfld_ai",
      "pertubations": [
        {{spatial}}
        {{spatiotemporal}}
        {{temporal}}
        {{constant}}
      ]
    }
  ]
}""")

perturb_tmpl = Template("""
{
    "type": "{{type}}",
    "dimension": "{{dimension}}",
    "value": "{{value}}",
    "calendar": "{{calendar}}"
}""")


class TestForcingPerturbations:

    @pytest.mark.parametrize("perturbation_type", ['scaling', 'offset'])
    def test_spatial(self, perturbation_type):
        """
        Test spatial scaling and offset
        """

        # Create 2d pertubation file
        perturb_value = './test_input.nc'

        perturb_str = perturb_tmpl.render(type=perturbation_type,
                                          dimension='spatial',
                                          value=str(perturb_value),
                                          calendar='forcing')
        with open('forcing.json', 'w') as f:
            s = forcing_tmpl.render(constant=perturb_str)
            f.write(s)

        # Read forcing input
        with open('forcing.json') as f:
            config = json.load(f)

        forcing_file = config['inputs'][0]['filename']
        fieldname = config['inputs'][0]['fieldname']

        with nc.Dataset(forcing_file) as f:
            # Read out a time point
            time_var = f.variables['time']
            times = nc.num2date(time_var[:], time_var.units)
            tidx = random.randint(0, len(times))

            date_str = times[tidx].strftime('%Y-%m-%dT%H:%M:%S')
            src_data = f.variables[fieldname][tidx, :]

        nx = src_data.shape[1]
        ny = src_data.shape[0]
        data_array = np.random.rand(ny, nx)
        create_nc_file(perturb_value, fieldname, data_array)

        ret = sp.run(['./forcing_test.exe', date_str])
        assert ret.returncode == 0

        assert Path('test.nc').exists()
        with nc.Dataset('test.nc') as f:
            dest_data = f.variables[fieldname][:]

        if perturbation_type == 'scaling':
            assert np.allclose(src_data*data_array, dest_data)
        else:
            assert np.allclose(src_data+data_array, dest_data)
	

    @pytest.mark.parametrize("perturbation_type", ['scaling', 'offset'])
    def test_temporal(self, perturbation_type):
        """
        Test temporal scaling and offset
        """

        pass


    @pytest.mark.parametrize("perturbation_type", ['scaling', 'offset'])
    def test_spatiotemporal(self, perturbation_type):
        """
        Test spatial scaling and offset
        """

        pass


    @pytest.mark.parametrize("perturbation_type", ['scaling', 'offset'])
    def test_constant(self, perturbation_type):
        """
        Test constant scaling and offset
        """

        perturb_value = random.randint(0, 100)

        perturb_str = perturb_tmpl.render(type=perturbation_type,
                                          dimension='constant',
                                          value=str(perturb_value),
                                          calendar='forcing')
        with open('forcing.json', 'w') as f:
            s = forcing_tmpl.render(constant=perturb_str)
            f.write(s)

        # Read forcing input
        with open('forcing.json') as f:
            config = json.load(f)

        forcing_file = config['inputs'][0]['filename']
        fieldname = config['inputs'][0]['fieldname']

        with nc.Dataset(forcing_file) as f:
            # Read out a time point
            time_var = f.variables['time']
            times = nc.num2date(time_var[:], time_var.units)
            tidx = random.randint(0, len(times))

            date_str = times[tidx].strftime('%Y-%m-%dT%H:%M:%S')
            src_data = f.variables[fieldname][tidx, :]

        ret = sp.run(['./forcing_test.exe', date_str])
        assert ret.returncode == 0

        assert Path('test.nc').exists()
        with nc.Dataset('test.nc') as f:
            dest_data = f.variables[fieldname][:]

        if perturbation_type == 'scaling':
            assert np.array_equal(src_data*perturb_value, dest_data)
        else:
            assert np.array_equal(src_data+perturb_value, dest_data)

