# Copyright (c) 2017 Siphon Contributors.
# Distributed under the terms of the BSD 3-Clause License.
# SPDX-License-Identifier: BSD-3-Clause
"""Test Wyoming upper air dataset access."""

from numpy import *
import sys
import datetime as dt
import time
from mesowest import MesoWest, WyomingUpperAir
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

test_date = dt.datetime(2020, 7, 3)

def test_mesowest():
    ## Add 1 to day
    df = MesoWest.request_data(test_date, 'KONM')
    in_time = pd.to_datetime("10:19:00").time()
    df_tm = df.loc[(df['Time'] == closest(df['Time'], in_time, test_date))]
    print(df_tm)
test_mesowest()

def test_wyoming():
    df = WyomingUpperAir.request_data(test_date, 'ABQ')
    print(df.pw)
# test_wyoming()
