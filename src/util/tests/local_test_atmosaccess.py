# Copyright (c) 2017 Siphon Contributors.
# Distributed under the terms of the BSD 3-Clause License.
# SPDX-License-Identifier: BSD-3-Clause
"""Test Wyoming upper air dataset access."""

from numpy import *
import sys
import datetime as dt
import time
from atmosaccess.NOAAaccess import data, dataset, datatypes, data_allday
from mesowest import MesoWest

import pandas as pd

def closest(lst, K, d):
    lst = asarray(lst)
    list = []
    tmp2 = dt.datetime.combine(d, K)
    for i in range(len(lst)):
        tmp1 = dt.datetime.combine(d, lst[i])
        list.append(abs(tmp1 - tmp2))
    idx = asarray(list).argmin()
    return lst[idx]


def test_noaaimport_rh(test_date):
    ## Add 1 to day
    df1 = data.NOAAData.request_data(test_date, '72362093040', 'HourlyRelativeHumidity')['HourlyRelativeHumidity']
    df2 = MesoWest.request_data(test_date+ dt.timedelta(days=1), 'KONM')['relative_humidity']
    # print(df1.values[0])
    # print(df2)

def test_noaaimport_temp(test_date):
    ## Add 1 to day
    df1 = data.NOAAData.request_data(test_date, '72362093040', 'HourlyDryBulbTemperature')['HourlyDryBulbTemperature']
    df2 = MesoWest.request_data(test_date+ dt.timedelta(days=1), 'KONM')['temperature'][70]
    print(df1.values[0])
    print(df2)

def test_noaaimport_time(test_date):
    df1 = data_allday.NOAADataDay.request_data(test_date, '72362093040', 'HourlyRelativeHumidity')['DATE']

    print(df1[0].split("T")[1])

date = dt.datetime(2019, 1, 27, 14, 35)
print(date)
test_noaaimport_rh(date)
test_noaaimport_temp(date)
test_noaaimport_time(date)
# test_mesowest(dt.datetime(2020, 6,6))

