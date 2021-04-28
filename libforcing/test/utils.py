
import netCDF4 as nc

def create_nc_file(output_name, var_name, data_array):

    time_dim = False
    if len(data_array.shape) == 3:
        time_dim = True
        ny = data_array.shape[1]
        nx = data_array.shape[2]
    else:
        assert len(data_array.shape) == 2
        ny = data_array.shape[0]
        nx = data_array.shape[1]

    with nc.Dataset(output_name, 'w') as f:

        if time_dim:
            f.createDimension('time', None)
            t = f.createVariable('time', 'f8', dimensions=('time'))
            t.standard_name = 'time'
            t.long_name = 'time'
            t.units = 'days since 1900-01-01 00:00:00'
            t.calendar = 'standard'
            t.time_origin = '1900-01-01:00:00:00'
            t.axis = 'T'

        f.createDimension('lon', nx)
        f.createDimension('lat', ny)

        lon = f.createVariable('lon', 'f8', dimensions=('lon'))
        lon.standard_name = 'longitude'
        lon.long_name = 'longitude'
        lon.units = 'degrees_east'
        lon.axis = 'X'

        lat = f.createVariable('lat', 'f8', dimensions=('lat'))
        lat.standard_name = 'latitude'
        lat.long_name = 'latitude'
        lat.units = 'degrees_north'
        lat.axis = 'Y'

        if time_dim:
            dimensions = ('lat', 'lon', 'time')
        else:
            dimensions = ('lat', 'lon')
        var = f.createVariable(var_name, 'f4', dimensions=dimensions)
        var[:] = data_array

