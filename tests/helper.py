
import os
import sys
import shutil
import glob
import shlex
import subprocess as sp

run_cmd = 'mpirun --mca orte_base_help_aggregate 0 --mca opal_abort_print_stack 1 --mca btl self,sm -np 1 {atm_exe} : -np 1 {ice_exe} : -np 1 {ocean_exe}'

def silentremove(filename):
    try:
        os.remove(filename)
    except FileNotFoundError:
        pass

class Helper:

    def __init__(self):
        self.test_dir = os.path.dirname(os.path.realpath(__file__))
        self.test_data_dir = os.path.join(self.test_dir, 'test_data')
        self.atm_exe = os.path.join(self.test_dir, '..',
                                    'build', 'bin', 'atm')
        self.ice_exe = os.path.join(self.test_dir, '..',
                                    'build', 'bin', 'ice_stub')
        self.ocean_exe = os.path.join(self.test_dir, '..',
                                      'build', 'bin', 'ocean_stub')

    def checksums(self, exp_dir):
        """
        Return checksums for experiment in exp_dir.
        """

        checksums = os.path.join(self.test_dir, exp_dir, 'checksums.txt')

    def filter_checksums(self, output):
        """
        Filter out and return checksums from output.
        """
        pass

    def filter_dates(self, output):
        """
        Filter out and return checksums from dates.
        """
        pass

    def run_exp(self, exp_dir):
        """
        Run the test experiment in exp_dir
        """

        def clean_exp():
            silentremove('accessom2_restart_datetime.nml')
            map(silentremove, glob.glob('*.nc'))

        def copy_oasis_restarts():
            shutil.copy(os.path.join(self.test_data_dir, 'i2o.nc'), './')
            shutil.copy(os.path.join(self.test_data_dir, 'o2i.nc'), './')
            shutil.copy(os.path.join(self.test_data_dir, 'a2i.nc'), './')

        cur_dir = os.getcwd()
        os.chdir(os.path.join(self.test_dir, exp_dir))
        clean_exp()
        copy_oasis_restarts()
        cmd = shlex.split(run_cmd.format(atm_exe=self.atm_exe,
                                         ice_exe=self.ice_exe,
                                         ocean_exe=self.ocean_exe))
        retcode = 0
        try:
            output = sp.run(cmd)
        except CalledProcessError as e:
            retcode = e.returncode

        return retcode, output

if __name__ == '__main__':
    sys.exit(run_exp('JRA55_RYF'))
