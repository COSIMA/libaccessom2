
import os
import sys
import datetime as dt
import f90nml

class Period:

    def __init__(self, years, months, seconds):
        self.years = years
        self.months = months
        self.seconds = seconds


class TimeManager:

    def __init__(self, forcing_start_datet, forcing_end_datet, restart_period,
                 timestep, calendar):
        self.exp_start_datet = forcing_start_datet
        self.forcing_start_datet = forcing_start_datet
        self.forcing_end_datet = forcing_end_datet
        self.restart_period = restart_period
        self.timestep = timestep
        self.calendar = calendar

        self.exp_cur_datet = self.exp_start_datet
        self.forcing_cur_datet = self.forcing_start_datet

        # Read in exp_cur_date and focing_cur_date from restart file.
        self.restart_file = 'accessom2_datet.nml'
        if os.path.exists(self.restart_file):
            nml = f90nml.read(self.restart_file)
            exp_cur_str = nml['do_not_edit']['exp_cur_datet']
            forcing_cur_str = nml['do_not_edit']['forcing_cur_datet']

            self.exp_cur_datet = dt.datetime.strptime(exp_cur_str,
                                                     '%Y-%m-%dT%H:%M:%S')
            self.forcing_cur_datet = dt.datetime.strptime(forcing_cur_str,
                                                          '%Y-%m-%dT%H:%M:%S')

        self.run_end_datet = self._run_end_datet()

    def _run_end_datet(self):

        end_datet = self.exp_cur_datet

        seconds = self.restart_period.seconds
        if seconds > 0:
            # Adding secconds is awkward because we need to handle
            # noleap calendar.
            assert self.restart_period.months == 0
            assert self.restart_period.years == 0

            extra_minutes = 0
            extra_hours = 0
            extra_days = seconds // 86400
            seconds = seconds % 86400

            if seconds > 0:
                extra_hours = seconds // 3600
                seconds = seconds % 3600

                if seconds > 0:
                    extra_minutes = seconds // 60
                    seconds = seconds % 60

            for i in range(extra_days):
                end_datet += dt.timedelta(days=1)

                if end_datet.month == 2 and end_datet.day == 29 and \
                    self.calendar == 'noleap':
                    end_datet += dt.timedelta(days=1)

        else:
            if self.restart_period.months > 0:
                assert self.restart_period.seconds == 0
                assert self.restart_period.years == 0

                extra_years = months // 12
                extra_months = months % 12

            else:
                assert self.restart_period.seconds == 0
                assert self.restart_period.months == 0
                assert self.restart_period.years > 0

                extra_years = restart_period.years
                extra_months = 0

            end_datet = dt.datetime(end_datet.year + extra_years,
                                       end_datet.month + months,
                                       end_datet.day,
                                       end_datet.hour,
                                       end_datet.minute,
                                       end_datet.second)
        return end_datet


    def deinit(self):

        d = {}
        d['do_not_edit'] = {}
        d['do_not_edit']['exp_cur_datet'] = \
            self.exp_cur_datet.strftime('%Y-%m-%dT%H:%M:%S')
        d['do_not_edit']['forcing_cur_datet'] = \
            self.forcing_cur_datet.strftime('%Y-%m-%dT%H:%M:%S')
        nml = f90nml.Namelist(d)

        nml.write(self.restart_file, force=True)


    def progress_forcing_datet(self):
        """
        Increment the forcing datetime by dt.
        """

        cur = self.forcing_cur_datet
        cur += dt.timedelta(seconds=self.timestep)
        if cur.month == 2 and cur.day == 29 and self.calendar == 'noleap':
            cur += dt.timedelta(days=1)

        if cur >= self.forcing_end_datet:
            cur = self.forcing_start_datet

        self.forcing_cur_datet = cur

    def get_forcing_cur_datet(self):
        return self.forcing_cur_datet

    def progress_exp_datet(self):

        cur = self.exp_cur_datet
        cur += dt.timedelta(seconds=self.timestep)
        if cur.month == 2 and cur.day == 29 and self.calendar == 'noleap':
            cur += dt.timedelta(days=1)

        if cur >= self.run_end_datet:
            cur = self.run_end_datet

        self.exp_cur_datet = cur

    def get_exp_cur_datet(self):
        return self.exp_cur_datet

    def run_finished(self):
        if self.exp_cur_datet >= self.run_end_datet:
            return True
        else:
            return False


def main():

    restart_period = Period(0, 0, 172800)

    tm = TimeManager(
            dt.datetime(1900, 1, 1),
            dt.datetime(1900, 1, 2),
            restart_period,
            3600,
            'noleap')

    while True:
        print(tm.get_exp_cur_datet())
        print(tm.get_forcing_cur_datet())

        tm.progress_exp_datet()
        tm.progress_forcing_datet()

        if tm.run_finished():
            tm.deinit()
            break


if __name__ == '__main__':
    sys.exit(main())
