
import sys
import datetime as dt
import f90nml

class Period:

    def __init__(self, years, months, seconds):
        self.years = years
        self.months = months
        self.seconds = seconds


class TimeManager:

    def __init__(self, forcing_start_datetime, forcing_end_datetime, restart_period
                 dt, calendar):
        self.exp_start_datetime = forcing_start_datetime
        self.forcing_start_datetime = forcing_start_datetime
        self.forcing_end_datetime = forcing_end_datetime
        self.restart_period = restart_period
        self.dt = dt
        self.calendar = calendar

        self.exp_cur_date = self.exp_start_datetime
        self.forcing_cur_date = self.forcing_start_datetime

        # Read in exp_cur_date and focing_cur_date from restart file.
        self.restart_file = 'accessom2_datetime.nml'
        if os.path.exists(restart_file):
            nml = f90nml.read(restart_file)
            exp_cur_str = nml['do_not_edit']['exp_cur_datetime']
            forcing_cur_str = nml['do_not_edit']['forcing_cur_datetime']

            self.exp_cur_datetime = dt.strptime(exp_cur_str,
                                                '%Y-%m-%dT%H:%M:%S')
            self.forcing_cur_datetime = dt.strptime(forcing_cur_str,
                                                    '%Y-%m-%dT%H:%M:%S')

        self.run_end_datetime = self._run_end_datetime()

    def _run_end_datetime(self):

        end_datetime = self.exp_cur_datetime

        seconds = self.restart_period.seconds
        if seconds > 0:
            ! Adding secconds is awkward because we need to handle
            ! noleap calendar.
            assert restart_period.months == 0 
            assert restart_period.years == 0 

            extra_minutes = 0
            extra_hours = 0
            extra_days = seconds / 86400
            seconds = seconds % 86400

            if seconds > 0:
                extra_hours = seconds / 3600
                seconds = seconds % 3600

                if seconds > 0:
                    extra_minutes = seconds / 60
                    seconds = seconds % 60

            for d in extra_days:
                end_datetime += dt.timedelta(days=1)
                
                if new_date.month == 2 and new_date.day == 29 and \
                    self.calendar == 'noleap':
                    end_datetime += dt.timedelta(days=1)

        else:
            if self.restart_period.months > 0:
                assert restart_period.seconds == 0 
                assert restart_period.years == 0 

                extra_years = months / 12
                extra_months = months % 12

            else
                assert restart_period.seconds == 0 
                assert restart_period.months == 0 
                assert restart_period.years > 0 

                extra_years = restart_period.years
                extra_months = 0

            end_datetime = dt.datetime(end_datetime.year + extra_years,
                                       end_datetime.month + months,
                                       end_datetime.day,
                                       end_datetime.hour,
                                       end_datetime.minute,
                                       end_datetime.second)
        return end_datetime
            

    def deinit(self):

        d = {}
        d['do_not_edit']['exp_cur_datetime'] = self.exp_cur_datetime
        d['do_not_edit']['forcing_cur_datetime'] = self.forcing_cur_datetime
        nml = f90nml.Namelist(d)

        nml.write(self.restart_file, force=True)


    def progress_forcing_datetime(self):
        """
        Increment the forcing datetime by dt.
        """

        cur = self.forcing_cur_datetime 
        cur += dt.timedelta(seconds=self.dt)
        if cur.month == 2 and cur.day == 29 and self.calendar == 'noleap':
            cur += dt.timedelta(days=1)

        if cur >= self.forcing_end_date:
            cur = self.forcing_start_date

        self.forcing_cur_datetime = cur

    def get_forcing_cur_datetime(self):
        return self.forcing_cur_datetime

    def progress_exp_datetime(self):

        cur = self.exp_cur_datetime
        cur += dt.timedelta(seconds=self.dt)
        if cur.month == 2 and cur.day == 29 and self.calendar == 'noleap':
            cur += dt.timedelta(days=1)

        if cur >= self.run_end_datetime:
            cur = self.run_end_datetime

        self.exp_cur_datetime = cur

    def get_exp_cur_datetime(self):
        return self.exp_cur_datetime

    def run_finished(self):
        if self.exp_cur_datetime >= self.run_end_datetime:
            return True
        else:
            return False


def main():

    restart_period = Period(0, 0, 86400)

    tm = TimeManager(
            dt.datetime(1900, 1, 1),
            dt.datetime(1901, 1, 1),
            (0, 0, 86400),
            3600,
            'noleap')

    while True:
        print tm.get_exp_cur_datetime()
        print tm.get_forcing_cur_datetime()

        tm.progress_exp_datetime()
        tm.progress_forcing_datetime()

        if run_finished():
            break



if __name__ == '__main__':
    sys.exit(main())
