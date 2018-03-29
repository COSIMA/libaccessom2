
import sys
import datetime as dt
import f90nml

class TimeManager:

    def __init__(self, exp_start_date, restart_period, forcing_period,
                 dt, calendar):
        self.exp_start_date = exp_start_date
        self.forcing_start_date = exp_start_date
        self.restart_period = restart_period
        self.forcing_period = forcing_period
        self.dt = dt
        self.calendar = calendar

        self.exp_cur_date = exp_cur_date
        self.forcing_cur_date = exp_cur_date

        # Read in exp_cur_date and focing_cur_date from restart file.
        self.restart_file = 'accessom2_datetime.nml'
        if os.path.exists(restart_file):
            with open(restart_file) as f:
                self.exp_cur_date = dt.strptime(f.readline(), '%Y-%m-%dT%H:%M:%S')
                self.forcing_cur_date = dt.strptime(f.readline(), '%Y-%m-%dT%H:%M:%S')

        self.forcing_end_date = self._forcing_end_date()

    def _forcing_end_date(self):


    def deinit(self):

        with open(restart_file) as f:
            print(self.exp_cur_date, file=f)
            print(self.forcing_cur_date, file=f)

    def next_forcing_datetime(self, forcing_cur_date):

        new_date = focing_cur_date + dt.timedelta(seconds=self.dt)
        if new_date.month == 2 and new_date.day == 29 and self.calendar == 'noleap':
            new_date += dt.timedelta(days=1)

        if new_date >= self.forcing_end_date:
            new_date = self.forcing_start_date

        return new_date

    def next_exp_datetime(self, exp_cur_date):

        new_date = exp_cur_date + dt.timedelta(seconds=self.dt)

        if new_date.month == 2 and new_date.day == 29 and self.calendar == 'noleap':
            new_date += dt.timedelta(days=1)

        return new_date

def main():

    tm = TimeManager(
            dt.datetime(1900, 1, 1),
            (0, 0, 86400),
            (0, 0, 86400),
            3600,
            'noleap')


if __name__ == '__main__':
    sys.exit(main())
