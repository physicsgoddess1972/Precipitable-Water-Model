import datetime
from datetime import date as dte
from datetime import datetime as dt
from siphon.simplewebservice import wyoming
from numpy import *
from metpy.calc import *
from metpy.units import units
from xarray import *
import matplotlib.pyplot as plt

import os
import backoff
import requests

# Re-usable decorator with exponential wait.
retry_timeout = backoff.on_exception(
    wait_gen=backoff.expo,
    exception=(
        requests.exceptions.HTTPError,
        requests.exceptions.Timeout,
        requests.exceptions.ConnectionError
    ),
    max_tries= int(os.getenv("REQUESTS_MAX_RETRIES", 100)),
)

@retry_timeout
def script():
    dt_rng  = array([dte.today() - datetime.timedelta(days=x) for x in range(0,365)])
    station = ["ABQ", "EPZ"]
    T       = []
    pw      = []
    for i in range(0, len(dt_rng)):
        z_12 = dt.combine(dt_rng[i], datetime.time(12, 0))
        z_00 = dt_rng[i]
        try:
            p_00_abq    = array(wyoming.WyomingUpperAir.request_data(z_00, station[0])['pressure']) * units.hPa
            dp_00_abq   = array(wyoming.WyomingUpperAir.request_data(z_00, station[0])['dewpoint']) * units.degC
            pw_00_abq   = precipitable_water(dp_00_abq,p_00_abq, top=(600 + 0.001) * units.hPa)
            T_00_abq    = array(wyoming.WyomingUpperAir.request_data(z_00, station[0])['temperature'][12])

            p_12_abq    = array(wyoming.WyomingUpperAir.request_data(z_12, station[0])['pressure']) * units.hPa
            dp_12_abq   = array(wyoming.WyomingUpperAir.request_data(z_12, station[0])['dewpoint']) * units.degC
            pw_12_abq   = precipitable_water(dp_12_abq,p_12_abq, top=(600 + 0.001) * units.hPa)
            T_12_abq    = array(wyoming.WyomingUpperAir.request_data(z_12, station[0])['temperature'][12])

            p_00_epz    = array(wyoming.WyomingUpperAir.request_data(z_00, station[1])['pressure']) * units.hPa
            dp_00_epz   = array(wyoming.WyomingUpperAir.request_data(z_00, station[1])['dewpoint']) * units.degC
            pw_00_epz   = precipitable_water(dp_00_epz,p_00_epz, top=(600 + 0.001) * units.hPa)
            T_00_epz    = array(wyoming.WyomingUpperAir.request_data(z_00, station[1])['temperature'][12])

            p_12_epz    = array(wyoming.WyomingUpperAir.request_data(z_12, station[1])['pressure']) * units.hPa
            dp_12_epz   = array(wyoming.WyomingUpperAir.request_data(z_12, station[1])['dewpoint']) * units.degC
            pw_12_epz   = precipitable_water(dp_12_epz,p_12_epz, top=(600 + 0.001) * units.hPa)
            T_12_epz    = array(wyoming.WyomingUpperAir.request_data(z_12, station[1])['temperature'][12])

            pw_abq  = DataArray(array(pw_00_abq,pw_12_abq)).mean()
            pw_epz  = DataArray(array(pw_00_epz,pw_12_epz)).mean()
            pw_avg  = mean(array(pw_abq,pw_epz))
            pw.append(pw_avg)

            T_abq   = mean(array([T_00_abq, T_12_abq]))
            T_epz   = mean(array([T_00_epz, T_12_epz]))
            T_avg   = mean(array([T_abq, T_epz]))
            T.append(T_avg)
        except ValueError:
            continue
    plt.title(r"Comparison of TPW and T$_{3k}$")
    plt.scatter(T, pw)
    plt.plot(T, poly1d(polyfit(T,pw,1))(T), '--k')
    dy = poly1d(polyfit(T,pw,1))(T)[1] - poly1d(polyfit(T,pw,1))(T)[0]
    dx = T[1] - T[0]
    print("Slope of best-fit: {:.2f}".format(dy/dx))
    plt.xlabel(r"Temperature at $\sim$3 km [$^{\circ}$C]")
    plt.ylabel("Precipitable water [mm]")
script()
plt.show()
