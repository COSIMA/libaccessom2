
import pytest
from pathlib import Path
from jinja2 import Template
import random
import subprocess as sp
import json
import netCDF4 as nc
import numpy as np

forcing_tmpl = Template("""
{
  "description": "JRA55-do V1.3 RYF 1990-91 forcing",
  "inputs": [
    {
      "filename": "/g/data/ua8/JRA55-do/RYF/v1-3/RYF.rsds.1990_1991.nc",
      "fieldname": "rsds",
      "cname": "swfld_ai",
      "pertubations": [
        {{scaling_spatial}}
        {{scaling_spatiotemporal}}
        {{scaling_temporal}}
        {{scaling_constant}}
      ]
    }
  ]
}""")


class TestForcingPertubations:

    TEST_DATA = Path('../../test_data')

    def test_scaling_constant(self):
        """
        Test constant offset
        """

        scale_value = random.randint(0, 100)

        constant_str = '{"type": "scaling", "dimension": "constant",' + \
                         '"value":' + str(scale_value) + '}'
        with open('forcing.json', 'w') as f:
            s = forcing_tmpl.render(scaling_constant=constant_str)
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

        assert np.array_equal(src_data*scale_value, dest_data)
