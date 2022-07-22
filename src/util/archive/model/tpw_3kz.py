<<<<<<< HEAD
import datetime
from datetime import date as dte
from datetime import datetime as dt
from siphon.simplewebservice import wyoming
from numpy import *
from metpy.calc import *
from metpy.units import units
from xarray import *
import matplotlib.pyplot as plt
import csv
import os
import backoff
import requests


fname = "./data_file.csv"
file = open(fname, "a")
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
def datafile():
    dt_rng  = array([dte.today() - datetime.timedelta(days=x) for x in range(0,365)])
    station = ["ABQ", "EPZ"]
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

            T_abq   = mean(array([T_00_abq, T_12_abq]))
            T_epz   = mean(array([T_00_epz, T_12_epz]))
            T_avg   = mean(array([T_abq, T_epz]))
            print(z_00)
            file.write(str(z_00.isoformat()) + "," +str(pw_avg) + "," + str(T_avg) + "\n")
        except ValueError:
            continue

def script():
    file_r = open(fname, "r")
    T       = []
    pw      = []
    with file_r as csvfile:
        for row in csv.reader(file_r, delimiter=','):
            pw_avg  = float(row[1].split('/')[0])
            T_avg   = float(row[2].split('/')[0])
            pw.append(pw_avg)
            T.append(T_avg)
    plt.title(r"Comparison of TPW and T$_{3k}$")
    plt.scatter(T, pw)
    plt.plot(T, poly1d(polyfit(T,pw,1))(T), '--k')
    dy = poly1d(polyfit(T,pw,1))(T)[1] - poly1d(polyfit(T,pw,1))(T)[0]
    dx = T[1] - T[0]
    print("Slope of best-fit: {:}".format(dy/dx))
    plt.xlabel(r"Temperature at $\sim$3 km [$^{\circ}$C]")
    plt.ylabel("Precipitable water [mm]")

script()
plt.show()
=======
import datetime
from datetime import date as dte
from datetime import datetime as dt
from siphon.simplewebservice import wyoming
from numpy import *
from metpy.calc import *
from metpy.units import units
from xarray import *
import matplotlib.pyplot as plt
import csv
import os
import backoff
import requests


fname = "./data_file.csv"
file = open(fname, "a")
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
def datafile():
    dt_rng  = array([dte.today() - datetime.timedelta(days=x) for x in range(0,365)])
    station = ["ABQ", "EPZ"]
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

            T_abq   = mean(array([T_00_abq, T_12_abq]))
            T_epz   = mean(array([T_00_epz, T_12_epz]))
            T_avg   = mean(array([T_abq, T_epz]))
            print(z_00)
            file.write(str(z_00.isoformat()) + "," +str(pw_avg) + "," + str(T_avg) + "\n")
        except ValueError:
            continue

def script():
    file_r = open(fname, "r")
    T       = []
    pw      = []
    with file_r as csvfile:
        for row in csv.reader(file_r, delimiter=','):
            pw_avg  = float(row[1].split('/')[0])
            T_avg   = float(row[2].split('/')[0])
            pw.append(pw_avg)
            T.append(T_avg)
    plt.title(r"Comparison of TPW and T$_{3k}$")
    plt.scatter(T, pw)
    plt.plot(T, poly1d(polyfit(T,pw,1))(T), '--k')
    dy = poly1d(polyfit(T,pw,1))(T)[1] - poly1d(polyfit(T,pw,1))(T)[0]
    dx = T[1] - T[0]
    print("Slope of best-fit: {:}".format(dy/dx))
    plt.xlabel(r"Temperature at $\sim$3 km [$^{\circ}$C]")
    plt.ylabel("Precipitable water [mm]")

script()
plt.show()
>>>>>>> 47addb531525c32ecd60a78ae867ca064c77857a
