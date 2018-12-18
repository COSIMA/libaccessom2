
import os
import f90nml
import json
import ast
import pytest
import datetime
import dateutil.parser
from collections import OrderedDict
from helper import Helper

class LogItem:

    def __init__(self, field_name, field_file,
                 field_index, forcing_datetime, checksum):
        self.forcing_datetime = forcing_datetime
        self.field_name = field_name
        self.field_file = field_file
        self.field_index = field_index
        self.checksum = checksum


def extract_field_name(checksum):
    k = list(checksum.keys())
    assert len(k) == 1
    k = k[0]

    return k.split('-')[2]
    

def build_log_items(forcing_update_dts, field_update_files,
                    field_update_indices, checksums):
    log_items = []

    assert len(forcing_update_dts) == len(field_update_files) == \
                len(field_update_indices) == len(checksums)

    for i in range(len(forcing_update_dts)):
        field_name = extract_field_name(checksums[i])
        item = LogItem(field_name, field_update_files[i], 
                       field_update_indices[i], forcing_update_dts[i],
                       checksums[i])
        log_items.append(item)

    return log_items

def remove_duplicate_runoff_checksums(checksums):

    new_checksums = []
    found_runoff_checksums = []
    num_removed_runoff_checksums = 0

    for c in checksums:
        k = list(c.keys())
        v = list(c.values())
        assert len(k) == 1
        if 'runof_ai' in k[0]:
            if v[0] not in found_runoff_checksums:
                new_checksums.append(c)
                found_runoff_checksums.append(v[0])
            else:
                num_removed_runoff_checksums += 1
        else:
            new_checksums.append(c)

    return new_checksums, num_removed_runoff_checksums

@pytest.fixture
def helper():
    return Helper()

@pytest.fixture(params=['JRA55_IAF', 'JRA55_RYF'])
def exp(request):
    yield request.param

class TestStubs:

    @pytest.mark.fast
    def test_run(self, helper, exp):
        """
        Check that the default configurations run.
        """

        ret, output, log, matm_log = helper.run_exp(exp)
        assert ret == 0


    def test_unchanged_forcing_checksums(self, helper, exp):
        """
        Test that checksums have not changed.
        """

        ret, output, log, matm_log = helper.run_exp(exp)
        assert ret == 0

        run_checksums = helper.filter_checksums(log)
        stored_checksums = helper.checksums(exp)

        # Check that keys are the same
        assert set(run_checksums.keys()) == set(stored_checksums.keys())
        # Check that everything is the same
        assert run_checksums == stored_checksums


    @pytest.mark.slow
    def test_forcing_fields(self, helper, exp):
        """
        Check that dates and checksums from YATM match those calculated here
        """

        ret, output, log, matm_log = helper.run_exp(exp)
        assert ret == 0

        # Get the experiment start and end dates
        exp_dir = os.path.join(helper.test_dir, exp)
        accessom2_config = os.path.join(exp_dir, 'accessom2.nml')
        with open(accessom2_config) as f:
            nml = f90nml.read(f)
            forcing_start_date = nml['date_manager_nml']['forcing_start_date']
            forcing_end_date = nml['date_manager_nml']['forcing_end_date']
            forcing_start_date = dateutil.parser.parse(forcing_start_date)
            forcing_end_date = dateutil.parser.parse(forcing_end_date)

        # Parse some YATM output
        def dicts_to_list(key_name, log_str): 
            lines = filter(lambda x : key_name in x, log_str.splitlines())
            out = []
            for l in lines:
                out += list(ast.literal_eval(l.strip()).values())
            return out

        forcing_update_dts = dicts_to_list('forcing_update_field-datetime', matm_log)
        forcing_update_dts = [dateutil.parser.parse(d) for d in forcing_update_dts]
        field_update_files = dicts_to_list('field_update_data-file', matm_log)
        field_update_indices = dicts_to_list('field_update_data-index', matm_log)
        tmp_chk = filter(lambda x : 'checksum' in x, matm_log.splitlines())
        checksums = []
        for c in tmp_chk:
            checksums.append(ast.literal_eval(c.strip()))

        # Parse forcing.json
        forcing_config = os.path.join(exp_dir, 'forcing.json')
        with open(forcing_config) as f:
            forcing = json.load(f)

        # Check that first forcing time corrosponds to forcing_start_date
        assert forcing_update_dts[0] == forcing_start_date 

        # Check that field dt is all the same and as expected 
        uniq_dt = list(OrderedDict.fromkeys(forcing_update_dts))
        dt = [b - a for a, b in zip(uniq_dt, uniq_dt[1:])]
        assert set(dt).pop() == datetime.timedelta(hours=3)

        # Remove duplicate runoff checksums
        checksums, num_removed = remove_duplicate_runoff_checksums(checksums)

        assert len(forcing_update_dts) == len(field_update_files) == \
                 len(field_update_indices) == len(checksums)

        log_items = build_log_items(forcing_update_dts, field_update_files,
                                    field_update_indices, checksums)

        # Check that indices for a particular year are sequential and increasing

        # Check that we have the right numbers of duplicate indices, i.e. all
        # forcing fields have the same indices except for runoff

        # Check that indices go back to 0 when crossing a year boundary

        # Iterate over forcing in Python, check that Fortran code did the same
        # by comparing the checksums of each field.


    def test_restart(self, helper, exp):
        """
        Test that model restarts at the correct date.
        """
        pass
