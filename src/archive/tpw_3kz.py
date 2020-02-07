import datetime
from datetime import date as dte
from datetime import datetime as dt
from siphon.simplewebservice import wyoming
from numpy import *
from metpy.calc import *
from metpy.units import units

dt_rng = [dte.today() - datetime.timedelta(days=x) for x in range(0,2)]
station = ["ABQ", "EPZ"]
P  = []
DP = []
T  = []
for i in range(0, len(dt_rng)):
    try:
        P.append(wyoming.WyomingUpperAir.request_data(dt_rng[i], station[0])['pressure'])
        DP.append(wyoming.WyomingUpperAir.request_data(dt_rng[i], station[0])['dewpoint'])
        T.append(wyoming.WyomingUpperAir.request_data(dt_rng[i], station[0])['temperature'])
    except ValueError:
        continue
P       = array(P) * units.hPa
DP      = array(DP) * units.degC

pw = []
for i in range(0, len(P)):
    pw.append(precipitable_water(DP[i],P[i], bottom=850 * units.hPa, top=600 * units.hPa))

plt.scatter(T, pw)
plt.show()
