
import pytest
from pathlib import Path
from jinja2 import Template
import random
import subprocess as sp

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

        constant_str = '{"type": "scaling", "dimension": "constant", "value": 10}'
        with open('forcing.json', 'w') as f:
            s = forcing_tmpl.render(scaling_constant=constant_str)
            f.write(s)

        ret = sp.run(['./forcing_test.exe'])
        assert ret.returncode == 0

