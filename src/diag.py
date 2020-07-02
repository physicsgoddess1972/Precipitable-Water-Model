# Diagnostic Algthm
import csv
import sys
import datetime
from numpy import *
from datetime import date as dte
from datetime import datetime as dt

sys.path.append("../../siphon-master/siphon/simplewebservice")
import wyoming

import os
import requests
import pprint as pp

fname = "../data/master_data.csv"
data = loadtxt(fname, skiprows=1,delimiter=",", dtype=str, unpack=True, usecols=(0,3,4,5,6))

date = data[0]
pw_abq_12z = data[1]
pw_abq_00z = data[2]
pw_epz_12z = data[3]
pw_epz_00z = data[4]
print()
for i in range(0, len(date)):
    dat = dt.strptime(date[i], "%m/%d/%Y")
    try:
        pw00 = wyoming.WyomingUpperAir.request_data(dat + datetime.timedelta(days=1), "EPZ")['pw'][0]
        # pw12 = wyoming.WyomingUpperAir.request_data(dt.combine(dat,
        #                                                         datetime.time(12, 0)), "EPZ")['pw'][0]
    except ValueError:
        pw00 = "NaN"
#        pw12 = "NaN"
    # print(pw12)
    # print(pw_abq_00z[i])
    if float(pw00) == float(pw_epz_00z[i]):
        print("\33[92m{} 00Z Complete\33[0m".format(date[i]))
    else:
        print("\33[91m{} 00Z Failed\33[0m".format(date[i]))
# pp.pprint(date)
