
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
      "perturbations": [
        {{perturbation0}}
        {{perturbation1}}
      ]
    }
  ]
}""")

perturb_tmpl = Template("""
{
    "type": "{{type}}",
    "dimension": "{{dimension}}",
    "value": "{{value}}",
    "calendar": "{{calendar}}",
    "comment": "",
}""")


separable_perturb_tmpl = Template("""
{
    "type": "{{type}}",
    "dimension": ["{{dimension1}}", "{{dimension2}}"],
    "value": ["{{value1}}", "{{value2}}"],
    "calendar": "{{calendar}}",
    "comment": ""
}""")


def get_forcing_field_shape():
    with nc.Dataset(FORCING_FILE) as f:
        return f.variables[FORCING_FIELDNAME].shape

def get_forcing_field_times():
    with nc.Dataset(FORCING_FILE) as f:
        time_var = f.variables['time']
        times = nc.num2date(time_var[:], time_var.units)
        return times, time_var.units, time_var.calendar


class TestForcingPerturbations:

    def run_simple_test(self, ptype, pdimension, pvalue, pcalendar):

        if ptype == 'separable':
            perturb_str = separable_perturb_tmpl.render(type=ptype,
                                                        dimension1=pdimension[0],
                                                        dimension2=pdimension[1],
                                                        value1=str(pvalue[0]),
                                                        value2=str(pvalue[1]),
                                                        calendar=pcalendar)
        else:
            perturb_str = perturb_tmpl.render(type=ptype,
                                              dimension=pdimension,
                                              value=str(pvalue),
                                              calendar=pcalendar)

        with open('forcing.json', 'w') as f:
            s = forcing_tmpl.render(forcing_filename=FORCING_FILE,
                                    forcing_fieldname=FORCING_FIELDNAME,
                                    coupling_fieldname=COUPLING_FIELDNAME,
                                    perturbation0=perturb_str)
            f.write(s)

        # Read out a random time point to test against
        with nc.Dataset(FORCING_FILE) as f:
            time_var = f.variables['time']
            times = nc.num2date(time_var[:], time_var.units)
            tidx = random.randint(0, len(times))

            date_str = times[tidx].strftime('%Y-%m-%dT%H:%M:%S')
            src_data = f.variables[FORCING_FIELDNAME][tidx, :]

        # Run fortran code with given datetime
        ret = sp.run(['./forcing_test.exe', date_str])
        assert ret.returncode == 0

        # Read fortran code output
        assert Path('test_output.nc').exists()
        with nc.Dataset('test_output.nc') as f:
            dest_data = f.variables[FORCING_FIELDNAME][:]

        # Get the configured perturbation value
        if ptype == 'separable':
            assert Path(str(pvalue[0])).exists()
            assert Path(str(pvalue[1])).exists()

            with nc.Dataset(pvalue[0]) as f:
                temporal_perturb_array = f.variables[FORCING_FIELDNAME][tidx]
            with nc.Dataset(pvalue[1]) as f:
                spatial_perturb_array = f.variables[FORCING_FIELDNAME][:]

            perturb_array = spatial_perturb_array*temporal_perturb_array
        else:
            if Path(str(pvalue)).exists():
                with nc.Dataset(pvalue) as f:
                    if pdimension == 'spatial':
                        perturb_array = f.variables[FORCING_FIELDNAME][:]
                    elif pdimension  == 'temporal':
                        perturb_array = f.variables[FORCING_FIELDNAME][tidx]
                    else:
                        assert pdimension == 'spatiotemporal'
                        perturb_array = f.variables[FORCING_FIELDNAME][tidx, :]
            else:
                assert pdimension == 'constant'
                perturb_array = float(pvalue)

        # Do the perturbation in Python code and check that it is as expected
        if ptype == 'scaling':
            assert np.allclose(src_data*perturb_array, dest_data)
        else:
            assert np.allclose(src_data+perturb_array, dest_data)


    @pytest.mark.parametrize("perturb_type", ['scaling', 'offset'])
    @pytest.mark.parametrize("calendar", ['forcing', 'experiment'])
    def test_constant(self, perturb_type, calendar):
        """
        Test constant scaling and offset
        """

        perturb_value = random.random()
        self.run_simple_test(perturb_type, 'constant', perturb_value, calendar)


    @pytest.mark.parametrize("calendar", ['forcing', 'experiment'])
    def test_separable(self, calendar):
        """
        Test separable scaling and offset
        """

        temporal_perturb_value = './test_temporal_input.nc'

        times, time_units, t_cal = get_forcing_field_times()
        data_array = np.random.rand(len(times))
        create_nc_file(temporal_perturb_value, FORCING_FIELDNAME, data_array,
                       time_vals=times,
                       time_units=time_units, calendar=t_cal)

        spatial_perturb_value = './test_spatial_input.nc'
        shape =  get_forcing_field_shape()
        nx = shape[2]
        ny = shape[1]

        data_array = np.random.rand(ny, nx)
        create_nc_file(spatial_perturb_value, FORCING_FIELDNAME, data_array)

        self.run_simple_test('separable', ['temporal', 'spatial'],
                             [temporal_perturb_value, spatial_perturb_value],
                             calendar)


    @pytest.mark.parametrize("perturb_type", ['scaling', 'offset'])
    def test_temporal(self, perturb_type):
        """
        Test temporal scaling and offset
        """

        # Create 2d perturbation file
        perturb_value = './test_input.nc'

        times, time_units, calendar = get_forcing_field_times()
        data_array = np.random.rand(len(times))
        create_nc_file(perturb_value, FORCING_FIELDNAME, data_array,
                       time_vals=times,
                       time_units=time_units, calendar=calendar)

        self.run_simple_test(perturb_type, 'temporal', perturb_value, 'forcing')


    @pytest.mark.parametrize("perturb_type", ['scaling', 'offset'])
    def test_spatial(self, perturb_type):
        """
        Test spatial scaling and offset
        """

        # Create 2d perturbation file
        perturb_value = './test_input.nc'

        shape =  get_forcing_field_shape()
        nx = shape[2]
        ny = shape[1]
        data_array = np.random.rand(ny, nx)
        create_nc_file(perturb_value, FORCING_FIELDNAME, data_array)

        self.run_simple_test(perturb_type, 'spatial', perturb_value, 'forcing')


    @pytest.mark.parametrize("perturb_type", ['scaling', 'offset'])
    def test_spatiotemporal(self, perturb_type):
        """
        Test spatiotemporal scaling and offset
        """

        # Create 2d perturbation file
        perturb_value = './test_input.nc'

        times, time_units, calendar = get_forcing_field_times()
        shape =  get_forcing_field_shape()
        nx = shape[2]
        ny = shape[1]

        data_array = np.random.rand(len(times), ny, nx)
        create_nc_file(perturb_value, FORCING_FIELDNAME, data_array,
                       time_vals=times,
                       time_units=time_units, calendar=calendar)

        self.run_simple_test(perturb_type, 'spatiotemporal', perturb_value, 'forcing')


