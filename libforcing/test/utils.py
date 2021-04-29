
import netCDF4 as nc

def create_nc_file(output_name, var_name, data_array,
                   time_vals=None,
                   time_units='days since 1900-01-01 00:00:00',
                   calendar='noleap'):

    time_dim = False
    spatial_dims = False
    if len(data_array.shape) == 3:
        time_dim = True
        spatial_dims = True
        ny = data_array.shape[1]
        nx = data_array.shape[2]
    elif len(data_array.shape) == 2:
        spatial_dims = True
        ny = data_array.shape[0]
        nx = data_array.shape[1]
    else:
        assert len(data_array.shape) == 1
        time_dim = True

    with nc.Dataset(output_name, 'w') as f:

        if time_dim:
            f.createDimension('time', None)
            t = f.createVariable('time', 'f8', dimensions=('time'))
            t.standard_name = 'time'
            t.long_name = 'time'
            t.units = time_units
            t.calendar = calendar
            t.axis = 'T'
            if time_vals is not None:
                t[:] = nc.date2num(time_vals, time_units, calendar=calendar)

        if spatial_dims:
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

        if time_dim and spatial_dims:
            dimensions = ('time', 'lat', 'lon')
        elif spatial_dims and not time_dim:
            dimensions = ('lat', 'lon')
        else:
            assert time_dim and not spatial_dims
            dimensions = ('time')

        var = f.createVariable(var_name, 'f4', dimensions=dimensions)
        var[:] = data_array
