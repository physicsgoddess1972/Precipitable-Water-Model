import csv
import sys,os
import datetime
from numpy import *
from datetime import date as dte
from datetime import datetime as dt

sys.path.insert(0, os.path.abspath('../siphon/siphon/'))

from simplewebservice import wyoming

station = ['ABQ', 'EPZ']
hour    = [00, 12]

fname   = '../Precipitable-Water-Model/data/master_data.csv'
wname   = '../Precipitable-Water-Model/data/cool_data.csv'

filew   = open(wname, "r")
file    = open(fname, "r")
reader  = csv.reader(file, delimiter=',')
readw   = csv.reader(filew, delimiter=",")

cool_data = []
with filew as csvfile:
    next(csv.reader(csvfile, delimiter=","))
    for row in readw:
        condition   = row[1].split('/')
        rh          = row[2].split('/')
        vicki_time  = row[7].split('/')
        nws_time    = row[8].split('/')
        nws_temp    = row[9].split('/')
        te_sky      = row[10].split('/')
        flir_sky    = row[11].split('/')
        ames1_sky   = row[12].split('/')
        ames2_sky   = row[13].split('/')
        te_gro      = row[14].split('/')
        flir_gro    = row[15].split('/')
        ames1_gro   = row[16].split('/')
        ames2_gro   = row[17].split('/')
        comments    = row[18].split('/')
        cool_data.append([condition, rh, vicki_time,
        nws_time, nws_temp, te_sky, flir_sky, ames1_sky,
        ames2_sky, te_gro, flir_gro, ames1_gro, ames2_gro,
        comments])

date    = []
with file as csvfile:
    next(csv.reader(csvfile, delimiter=","))
    for row in reader:
        day     = row[0].split('/')[1]
        month   = row[0].split('/')[0]
        year    = row[0].split('/')[2]
        date.append(dt(int(year), int(month), int(day), hour=0))
today = dt.today().date()
dt_rng = [date[-1].date() + datetime.timedelta(days=x) for x in range(abs(date[-1].date() - today).days)][1:]
dt_rng.append(today)

data_abq = []
for i in range(len(dt_rng)):
    pw12 = wyoming.WyomingUpperAir.request_data(dt.combine(dt_rng[i], datetime.time(12, 0)), station[0])['pw'][0]
    pw00 = wyoming.WyomingUpperAir.request_data(dt_rng[i] + datetime.timedelta(days=1), station[0])['pw'][0]
    data_abq.append([station[0], [dt_rng[i], pw12, pw00]])

data_epz = []
for i in range(len(dt_rng)):
    pw12 = wyoming.WyomingUpperAir.request_data(dt.combine(dt_rng[i], datetime.time(12, 0)), station[1])['pw'][0]
    pw00 = wyoming.WyomingUpperAir.request_data(dt_rng[i] + datetime.timedelta(days=1), station[1])['pw'][0]
    data_epz.append([station[1], [dt_rng[i], pw12, pw00]])

neat = []
for i in range(1,len(dt_rng) + 1):
    print(dt_rng[-i])
    neat.append(cool_data[-i])
neat = neat[::-1]

file_a = open(fname, "a")
with file_a as csvfile:
    for i in range(len(dt_rng)):
        csvfile.write(str(dt_rng[i].strftime("%-m/%-d/%Y")) + ","
        + str(neat[i][0][0]) + ","
        + str(neat[i][1][0]) + ","
        + str(data_abq[i][1][1]) + ","
        + str(data_abq[i][1][2]) + ","
        + str(data_epz[i][1][1]) + ","
        + str(data_epz[i][1][2]) + ","
        + str(neat[i][2][0]) + ","
        + str(neat[i][3][0]) + ","
        + str(neat[i][4][0]) + ","
        + str(neat[i][5][0]) + ","
        + str(neat[i][6][0]) + ","
        + str(neat[i][7][0]) + ","
        + str(neat[i][8][0]) + ","
        + str(neat[i][9][0]) + ","
        + str(neat[i][10][0]) + ","
        + str(neat[i][11][0]) + ","
        + str(neat[i][12][0]) + ",\n")

print("Done")
