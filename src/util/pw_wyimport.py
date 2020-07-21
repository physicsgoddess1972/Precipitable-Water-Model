####
## Title: 	Precipitable Water Extraction
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
####
import os
import csv
import sys
import requests
from numpy import *
import pandas as pd

import time
import datetime
from datetime import date as dte
from datetime import datetime as dt

from metpy.units import units
from siphon.simplewebservice.wyoming import WyomingUpperAir

from mesowest import MesoWest

from rich import print, box
from rich.panel import Panel
from rich.table import Table
from rich.progress import track

from rich.progress import (
    BarColumn,
    DownloadColumn,
    TextColumn,
    TransferSpeedColumn,
    TimeRemainingColumn,
    Progress,
    TaskID,
)

progress = Progress(TextColumn("[bold blue]{task.fields[filename]}", justify="right"),
                    BarColumn(bar_width=None),
                    "[progress.percentage]{task.percentage:>3.1f}%",
                    TimeRemainingColumn())

## Timeout Retry
REQUESTS_MAX_RETRIES = int(os.getenv("REQUESTS_MAX_RETRIES", 10))
adapter = requests.adapters.HTTPAdapter(max_retries=REQUESTS_MAX_RETRIES)

## Stations used
station = ['ABQ', 'EPZ']
## Hours to pull
hour    = [00, 12]

## Data file used for model input
fname   = '../../data/master_data.csv'
## Data file used for user input
wname   = '../../data/cool_data.csv'

def closest(lst, K, d):
    lst = asarray(lst)
    list = []
    tmp2 = dt.combine(d, K)
    for i in range(len(lst)):
        list.append(abs(dt.combine(d, lst[i]) - tmp2))
    idx = asarray(list).argmin()
    return lst[idx]

def wyoming_import(end_date, station):
    try:
        df_12    = WyomingUpperAir.request_data(dt.combine(end_date, datetime.time(12, 0)), station[0])
        pw12     = df_12.pw[0]
    except ValueError:
        pw12 = "NaN"
    try:
        df_00   = WyomingUpperAir.request_data(end_date + datetime.timedelta(days=1), station[0])
        pw00    = df_00.pw[0]
    except ValueError:
        pw00 = "NaN"

    data_abq = [station[0], [end_date, pw12, pw00]]

    try:
        df_12   = WyomingUpperAir.request_data(dt.combine(end_date, datetime.time(12, 0)), station[1])
        pw12    = df_12.pw[0]
    except ValueError:
        pw12 = "NaN"
    try:
        df_00   = WyomingUpperAir.request_data(end_date + datetime.timedelta(days=1), station[1])
        pw00    = df_00.pw[0]
    except ValueError:
        pw00 = "NaN"
    data_epz = [station[1], [end_date, pw12, pw00]]
    return data_abq, data_epz

def impt(end_date, idx):
    cool_data = []
    with filew as csvfile:
        next(csv.reader(csvfile, delimiter=","))
        for row in readw:
            vicki_time  = row[1].split('/')
            condition   = row[2].split('/')
            te_sky      = row[3].split('/')
            flir_sky    = row[4].split('/')
            ames1_sky   = row[5].split('/')
            ames2_sky   = row[6].split('/')
            te_gro      = row[7].split('/')
            flir_gro    = row[8].split('/')
            ames1_gro   = row[9].split('/')
            ames2_gro   = row[10].split('/')
            comments    = row[11].split('/')
            cool_data.append([vicki_time, condition,
            te_sky, flir_sky, ames1_sky, ames2_sky, te_gro,
            flir_gro, ames1_gro, ames2_gro,
            comments])
    i = 0
    ex = "requests.exception.HTTPError"
    while ex == "requests.exception.HTTPError":
        try:
            data_abq, data_epz = wyoming_import(end_date, station)
            ex = None
        except requests.exceptions.HTTPError as exception:
            time.sleep(60)
            data_abq, data_epz = wyoming_import(end_date, station)
            ex = str(exception)
            i =+ 1

    neat = []
    for i in range(idx, idx + 1):
        neat.append(cool_data[i])
    neat = neat[::-1]

    df_mw = MesoWest.request_data(end_date + datetime.timedelta(days=1), "KONM")
    in_time = pd.to_datetime(neat[0][0][0]).time()
    if df_mw.columns[0] == 'time(mdt)':
        df_tm = df_mw.loc[(df_mw['time(mdt)'] == closest(df_mw['time(mdt)'], in_time, end_date))]
        thyme   = df_tm['time(mdt)'].values[0]
    elif df_mw.columns[0] == 'time(mst)':
        df_tm = df_mw.loc[(df_mw['time(mst)'] == closest(df_mw['time(mst)'], in_time, end_date))]
        thyme   = df_tm['time(mst)'].values[0]

    rh      = df_tm['relative_humidity'].values[0]
    temp    = round((float(df_tm['temperature'].values[0]) * units.degF).to(units.degC).magnitude, 2)

    data_mesowest = [rh, thyme, temp]

    with open(fname, "a") as csvfile:
        csvfile.write(str(end_date.strftime("%-m/%-d/%Y")) + ","
        + str(neat[0][0][0]) + ","
        + str(data_mesowest[1].strftime("%H:%M")) + ","
        + str(neat[0][1][0]) + ","
        + str(data_mesowest[0]) + ","
        + str(data_mesowest[2]) + ","
        + str(data_abq[1][1]) + ","
        + str(data_abq[1][2]) + ","
        + str(data_epz[1][1]) + ","
        + str(data_epz[1][2]) + ","
        + str(neat[0][2][0]) + ","
        + str(neat[0][3][0]) + ","
        + str(neat[0][4][0]) + ","
        + str(neat[0][5][0]) + ","
        + str(neat[0][6][0]) + ","
        + str(neat[0][7][0]) + ","
        + str(neat[0][8][0]) + ","
        + str(neat[0][9][0]) + ","
        + str(neat[0][10][0]) + "\n")

full_len = len(loadtxt(wname, delimiter=",", dtype=str, usecols=(0))) - 1
last     = list(loadtxt(wname, delimiter=",", dtype=str, usecols=(0))).index(str(loadtxt(fname, delimiter=",", dtype=str, usecols=(0))[-1]))

task_id = progress.add_task("download", filename="Data Import")
for i in range(last, full_len - 1):
    filew   = open(wname, "r")
    file    = open(fname, "r")

    readf  = csv.reader(file, delimiter=',')
    readw   = csv.reader(filew, delimiter=",")
    date = dt.strptime(str(loadtxt(wname, delimiter=",", dtype=str, usecols=(0))[i+1]), "%m/%d/%Y")
    impt(date , i)
    progress.update(task_id, advance=(100./((full_len - 1) - last)),refresh=True)
