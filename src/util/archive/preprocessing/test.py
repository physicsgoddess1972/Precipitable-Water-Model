import csv
import sys
import datetime
from numpy import *
from datetime import date as dte
from datetime import datetime

sys.path.append("../../../siphon-master/siphon/simplewebservice")
import wyoming


date = datetime(2020, 1, 1, 0)

pw00 = wyoming.WyomingUpperAir.request_data(date, "EPZ")['pw']
print(pw00)
