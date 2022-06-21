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

# 7/15/2021,overcast,16:02,15:55,29.0,30.39,29.26,25.62,26.57,26.54,-Inf,-2.3,11.3,-Inf,12.7,-Inf,45.5,45.7,-Inf,45.7,
# 7/16/2021,overcast,15:58,15:55,25.0,32.11,24.38,26.41,34.78,26.8,-Inf,-5.7,6.7,-Inf,7.5,-Inf,47.3,49.2,-Inf,49.5,cirrus partly cloudy
# 7/17/2021,overcast,16:46,16:55,38.0,27.78,28.18,25.74,35.48,29.92,-Inf,-2.7,15.2,-Inf,17.2,-Inf,40.4,38.8,-Inf,43.3,rain
# 7/18/2021,overcast,16:46,16:55,23.0,33.5,31.97,30.21,38.38,41.08,-Inf,12.6,4.6,-Inf,3.8,-Inf,53.6,54.6,-Inf,54,pre-rain
# 7/19/2021,overcast,16:10,19:15,31.0,30.61,28.03,19.93,37.22,35.5,-Inf,-21,-1.7,-Inf,-1,-Inf,50.6,52.2,-Inf,52.7,partly cloudy smoke haze
# 7/20/2021,overcast,16:06,16:15,22.0,32.39,28.0,28.45,30.98,20.27,-Inf,-19.4,-0.7,-Inf,0,-Inf,54.5,55.7,-Inf,55.9,smoke haze
# 7/21/2021,overcast,16:09,16:15,25.0,32.22,27.34,23.76,23.9,18.54,-Inf,-22.5,0.5,-Inf,1,-Inf,48.6,51.1,-Inf,50.8,haze smoke blanket


## Generated
# 1/24/2021,overcast,10:11,10:15,36,9.44,8.82,9.13,7.98,11.17,NaN,-18.8,-13,-Inf,-11.1,10.8,9.8,9.3,-Inf,10,puffy spaced out clouds
# 1/25/2021,overcast,10:07,10:15,48,2.78,6.28,5.95,6.42,5.43,-20,-18.7,-14.3,-Inf,-14.5,4.6,4,2.6,-Inf,1.6,blanket of clouds
# 1/26/2021,overcast,10:32,10:35,48,2.22,6.54,4.17,10.0,7.42,-13.6,-13.1,-13.8,-Inf,-9.7,6.4,4.3,2.5,-Inf,4.6,
# 1/27/2021,clear sky,9:50,09:55,52,-1.67,3.39,4.73,3.62,3.76,NaN,NaN,NaN,-Inf,NaN,0.4,1,-0.3,-Inf,-0.6,
# 1/28/2021,overcast,16:06,16:15,26,6.67,3.82,7.22,5.15,6.07,NaN,NaN,-28.5,-Inf,-34.6,7.4,6.9,4.2,-Inf,4.6,
# 1/29/2021,clear sky,16:03,15:55,25,15.56,7.82,5.38,11.54,15.33,NaN,NaN,-33.3,-Inf,-33.9,11.9,11.2,9.1,-Inf,9.5,
# 1/30/2021,clear sky,16:00,15:55,21,11.11,3.46,4.21,7.19,4.72,NaN,NaN,-37.9,-Inf,-37.5,11.4,10.8,10.2,-Inf,11.5,
