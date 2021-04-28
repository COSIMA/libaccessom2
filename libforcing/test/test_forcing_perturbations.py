
import pytest
from pathlib import Path
from jinja2 import Template
import random
import subprocess as sp
import json
import netCDF4 as nc
import numpy as np
from utils import create_nc_file

FORCING_FILE = "/g/data/ua8/JRA55-do/RYF/v1-3/RYF.rsds.1990_1991.nc"
FORCING_FIELDNAME = "rsds"
COUPLING_FIELDNAME = "swfld_ai"

forcing_tmpl = Template("""
{
  "description": "JRA55-do V1.3 RYF 1990-91 forcing",
  "inputs": [
    {
      "filename": "{{forcing_filename}}",
      "fieldname": "{{forcing_fieldname}}",
      "cname": "{{coupling_fieldname}}",
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


def get_forcing_field_shape():
    with nc.Dataset(FORCING_FILE) as f:
        return f.variables[FORCING_FIELDNAME].shape


class TestForcingPerturbations:

    def run_test(self, forcing_config, perturb_config):

        forcing_file = forcing_config['inputs'][0]['filename']
        fieldname = forcing_config['inputs'][0]['fieldname']

        # Read out a random time point to test against
        with nc.Dataset(forcing_file) as f:
            time_var = f.variables['time']
            times = nc.num2date(time_var[:], time_var.units)
            tidx = random.randint(0, len(times))

            date_str = times[tidx].strftime('%Y-%m-%dT%H:%M:%S')
            src_data = f.variables[fieldname][tidx, :]

        # Run fortran code with given datetime
        ret = sp.run(['./forcing_test.exe', date_str])
        assert ret.returncode == 0

        # Read fortran code output
        assert Path('test_output.nc').exists()
        with nc.Dataset('test_output.nc') as f:
            dest_data = f.variables[fieldname][:]

        # Get the configured pertubation value
        perturb_value = perturb_config['value']
        if Path(perturb_value).exists():
            with nc.Dataset(perturb_value) as f:
                data_array = f.variables[fieldname][:]
        else:
            data_array = int(perturb_value)

        # Do the pertubation in Python code and check that it is as expected
        if perturb_config['type'] == 'scaling':
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
            s = forcing_tmpl.render(forcing_filename=FORCING_FILE,
                                    forcing_fieldname=FORCING_FIELDNAME,
                                    coupling_fieldname=COUPLING_FIELDNAME,
                                    spatial=perturb_str)
            f.write(s)
            forcing_config = json.loads(s)
 
        shape =  get_forcing_field_shape()
        nx = shape[2]
        ny = shape[1]
        data_array = np.random.rand(ny, nx)
        create_nc_file(perturb_value, FORCING_FIELDNAME, data_array)

        self.run_test(forcing_config, json.loads(perturb_str))


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
            s = forcing_tmpl.render(forcing_filename=FORCING_FILE,
                                    forcing_fieldname=FORCING_FIELDNAME,
                                    coupling_fieldname=COUPLING_FIELDNAME,
                                    constant=perturb_str)
            f.write(s)
            forcing_config = json.loads(s)
 
        self.run_test(forcing_config, json.loads(perturb_str))

