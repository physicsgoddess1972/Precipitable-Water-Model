# Copyright (c) 2017 Siphon Contributors.
# Distributed under the terms of the BSD 3-Clause License.
# SPDX-License-Identifier: BSD-3-Clause
"""Test Wyoming upper air dataset access."""

from numpy import *
import sys
import datetime as dt
import time
from src.util.mesowest import MesoWest
from siphon.simplewebservice.wyoming import WyomingUpperAir

import pandas as pd
from metpy.units import units

def closest(lst, K, d):
    lst = asarray(lst)
    list = []
    tmp2 = dt.datetime.combine(d, K)
    for i in range(len(lst)):
        tmp1 = dt.datetime.combine(d, lst[i])
        list.append(abs(tmp1 - tmp2))
    idx = asarray(list).argmin()
    return lst[idx]


def test_mesowest(test_date):
    ## Add 1 to day
    df = MesoWest.request_data(test_date, '44009')
    df = MesoWest.request_data(test_date, 'KONM')
    print(test_date)

date = dt.datetime(2020, 8, 21)
for i in range(0, 1):
    test_mesowest(date - dt.timedelta(days=i))
# test_mesowest(dt.datetime(2020, 6,6))

def test_wyoming(test_date):
    df = WyomingUpperAir.request_data(test_date, 'ABQ')
    print(df.pw)
# test_wyoming(date)
