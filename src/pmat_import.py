####
# ' @title Precipitable Water Model Analysis Tool: Preprocessing
# ' @author Spencer Riley
# ' @docs https://docs.pmat.app
####
import csv, os, yaml
import requests
from numpy import *
import pandas as pd
from io import StringIO
from bs4 import BeautifulSoup
import dateutil.parser

import time
import datetime
from datetime import datetime as dt

from metpy.units import units
from siphon.simplewebservice.wyoming import WyomingUpperAir
from siphon.http_util import HTTPEndPoint

import warnings
warnings.simplefilter("ignore")

class MesoWest(HTTPEndPoint):
    """Download and parse data from the University of Utah's MesoWest archive."""

    def __init__(self):
        """Set up endpoint."""
        super(MesoWest, self).__init__('https://mesowest.utah.edu/')

    @classmethod
    def request_data(cls, date, site_id, **kwargs):
        r"""Retrieve upper air observations from the University of Utah MesoWest archive.

        Parameters
        ----------
        date : datetime
            The date of the desired observation.
        site_id : str
            The four letter MesoWest identifier of the station for which data should be
            downloaded.
            https://mesowest.utah.edu/cgi-bin/droman/meso_station.cgi?area=1
        kwargs
            Arbitrary keyword arguments to use to initialize source
        Returns
        -------
            :class:`pandas.DataFrame` containing the data
        """
        endpoint = cls()
        df = endpoint._get_data(date, site_id)
        return df

    def _get_data(self, date, site_id):
        """Download and parse upper air observations from an online archive.
        Parameters
        ----------
        date : datetime
            The date of the desired observation.
        site_id : str
            The four letter MesoWest identifier of the station for which data should be
            downloaded.
            https://mesowest.utah.edu/cgi-bin/droman/meso_station.cgi?area=1
        Returns
        -------
            :class:`pandas.DataFrame` containing the data
        """
        raw_data = self._get_data_raw(date, site_id)
        soup = BeautifulSoup(raw_data, 'html.parser')
        names = pd.DataFrame.from_records(
            [[td.find_next(text=True).strip('\n\t\t') for td in tr.find_all('small')] for tr in soup.find_all('th')])[
            1].dropna(how='any', axis=0).reset_index(drop=True)
        df = pd.DataFrame.from_records([[td.find_next(text=True).strip("\n\t\t") for td in tr.find_all("td")] for tr in
                                        soup.find_all('tr')]).dropna(how='any', axis=0).reset_index(drop=True)
        df = df.replace(r'^\s*$', nan, regex=True).replace('N/A', inf)

        df[0] = pd.to_datetime(df[0], errors='coerce')
        try:
            df[0] = df[0].apply(lambda x: x.time())
        except ValueError as err:
            df[0] = "NaT"
        name = []
        for i in range(1, len(df.columns)):
            if str(df[i].dtypes) == 'object':
                if str(df[i].iloc[0]).replace(' ', '').isalpha():
                    pass
                else:
                    for j in range(0, len(df)):
                        if not ('<' in str(df[i].iloc[j]) or '>' in str(df[i].iloc[j])):
                            df[i].iloc[j] = float(df[i].iloc[j])
            else:
                pass
            if names.tolist()[len(df.columns) + i - 1] != "":
                name.append(names.tolist()[i] + " " + names.tolist()[len(df.columns) + i - 1])
            else:
                name.append(names.tolist()[i])

        name.insert(0, names.tolist()[0])
        df = df.replace(inf, 'N/A')
        df.columns = [(x.lower()).replace(" ", "_") for x in name]
        return df

    def _get_data_raw(self, date, site_id):
        """Download data from the University of Utah's MesoWest archive.
        Parameters
        ----------
        date: datetime
            Date for which data should be downloaded
        site_id : str
            Site id for which data should be downloaded
            https://mesowest.utah.edu/cgi-bin/droman/meso_station.cgi?area=1
        Returns
        -------
        text of the server response
        """
        path = ('cgi-bin/droman/meso_table_mesowest.cgi?stn={stid}'
                '&unit=0&time=LOCAL'
                '&day1={date:%d}&month1={date:%m}&year1={date:%Y}&hour1={date:%H}'
                '&graph=0&past=1').format(date=date, stid=site_id)
        resp = self.get_path(path)
        # See if the return is valid, but has no data
        if resp.text.find("Can't") != -1:
            raise ValueError(
                'No data available for {date:%Y-%m-%d %HZ} '
                'for station {stid}.'.format(date=date, stid=site_id))
        return resp.text

class PMAT_Import():
    """
    @title closest
    @brief A function that computes the closest value
    @param lst The date of the desired observation.
    @param K
    @param d
    @return
    """
    def closest(lst, K, d):
        lst = asarray(lst)
        list = []
        tmp2 = dt.combine(d, K)
        for i in range(len(lst)):
            list.append(abs(dt.combine(d, lst[i]) - tmp2))
        idx = asarray(list).argmin()
        return lst[idx]

    """
    @title wyoming_import
    @brief Imports Wyoming Data for specified site and date
    @param end_date 
    @param station
    @return 
    """
    def wyoming_import(end_date, station):
        try:
            df_12 = WyomingUpperAir.request_data(dt.combine(end_date, datetime.time(12, 0)), station)
            pw12 = df_12.pw[0]
        except (ValueError, IndexError):
            pw12 = "NaN"
        except requests.exceptions.HTTPError:
            pw12 = "Error"
        try:
            df_00 = WyomingUpperAir.request_data(end_date + datetime.timedelta(days=1), station)
            pw00 = df_00.pw[0]
        except (ValueError, IndexError):
            pw00 = "NaN"
        except requests.exceptions.HTTPError:
            pw00 = "Error"
        return [station, [end_date, pw12, pw00]]

    def external_import(path, end_date, type):
        thing = list(pd.read_csv(dir + path, delimiter=",").columns)
        test  = thing.index([x for x in thing if type in x][0])
        dty = [x for x in thing if type in x][0][0:2].lower()

        efname = open(dir + path, "r")
        reade  = loadtxt(efname, dtype=str, delimiter=",")
        date_fmt = cnfg[0]['external'][ext_bool.index(dty)][dty][0]['date']
        for i in range(0, len(reade)-1):
            if dt.strptime(str(reade[1+i][0]), date_fmt) == end_date:
                data = reade[1+i][test]
                break
            else:
                if dt.strptime(str(reade[1+i][0]), date_fmt) < end_date:
                    continue
                else:
                    data = "NaN"
                    break
        return data

    """
    @title mesowest_import
    @brief Imports Mesowest Data for specified site and date
    @param end_date 
    @param station
    @param in_time
    @return 
    """
    def mesowest_import(end_date, station, in_time):
        df_mw = MesoWest.request_data(end_date + datetime.timedelta(days=1), station.strip(" "))
        mw_header = df_mw.columns
        for i in range(len(mw_header)):
            if "time(" in mw_header[i]:
                tau = i
            else:
                continue

        if (str(in_time) in ['00:00:00', 'NaT']) or (str(df_mw[mw_header[tau]][0]) == 'NaT'):
            rh = "NaN"
            temp = "NaN"
            thyme = "NaT"
        else:
            df_tm = df_mw.loc[(df_mw[mw_header[tau]] == PMAT_Import.closest(df_mw[mw_header[tau]], in_time, end_date))]

            thyme = df_tm[mw_header[tau]].values[0]
            rh = int(df_tm['relative_humidity'].values[0])
            temp = round((float(df_tm['temperature'].values[0]) * units.degF).to(units.degC).magnitude, 2)

            if str(rh) == "nan":
                rh = "NaN"
            if str(temp) == "nan":
                temp = "NaN"
        return [thyme, rh, temp]

    """
    @title impt
    @brief 
    @param end_date 
    @param idx
    """
    def impt(end_date, idx):
        cool_data = []
        with filew as csvfile:
            next(csv.reader(csvfile, delimiter=","))
            for row in readw:
                sky = [[] for _ in range(len(indx[0]))]
                gro = [[] for _ in range(len(indx[1]))]
                mtime = row[1].split('/')
                condition = row[2].split('/')
                for j in range(len(indx[0])):
                    sky[j] = row[indx[0][j]].split('/')
                for k in range(len(indx[1])):
                    gro[k] = row[indx[1][k]].split('/')
                comments = row[-1].split('/')
                cool_data.append([mtime, condition, sky, gro, comments])

        neat = []
        for i in range(idx, idx + 1):
            neat.append(cool_data[i])
        neat = neat[::-1]

        if str(neat[0][0][0]) == "00:00":
            fin_tme = "NaT"
        else:
            fin_tme = neat[0][0][0]

        d = {'Date': end_date.strftime("%Y-%m-%d"),
             'Condition': neat[0][1][0],
             'Time': fin_tme}

        if "wyoming" in keys:
            wy_data = []
            for j in wy_station:
                i = 0
                wy_out = PMAT_Import.wyoming_import(end_date, j.strip(" "))
                while "Error" in wy_out[1]:
                    while "Error" == wy_out[1][1]:
                        time.sleep(10)
                        wy_out[1][1] = PMAT_Import.wyoming_import(end_date, j.strip(" "))[1][1]
                    while "Error" == wy_out[1][2]:
                        time.sleep(10)
                        wy_out[1][2] = PMAT_Import.wyoming_import(end_date, j.strip(" "))[1][2]
                    i = + 1
                wy_data.append(wy_out)
            for i in range(len(wy_data)):
                d["PW " + str(wy_station[i]).strip(" ") + "_" + "12Z"] = wy_data[i][1][1]
                d["PW " + str(wy_station[i]).strip(" ") + "_" + "00Z"] = wy_data[i][1][2]

        if "mesowest" in keys:
            mw_data = []
            for j in mw_station:
                mw_data.append(PMAT_Import.mesowest_import(end_date, j,
                                                           pd.to_datetime(neat[0][0][0]).time()))
            for i in range(len(mw_data)):
                if str(mw_data[i][0]) == "NaT":
                    d[str(mw_station[i]).strip(" ") + "_" + "Time"] = "NaT"
                else:
                    d[str(mw_station[i]).strip(" ") + "_" + "Time"] = mw_data[i][0].strftime("%H:%M")
                d[str(mw_station[i]).strip(" ") + "_" + "RH"] = mw_data[i][1]
                d[str(mw_station[i]).strip(" ") + "_" + "Temp"] = mw_data[i][2]

        if "external" in keys:
            if "pw" in ext_bool:
                pw_ext = []
                for i in pw_data:
                    pw_ext.append(PMAT_Import.external_import(i, end_date, "PW"))
                for i in range(len(pw_data)):
                    d["PW " + str(pw_id[i])] = pw_ext

            if "rh" in ext_bool:
                rh_ext = []
                for i in rh_date:
                    rh_ext.append(PMAT_Import.external_import(i, end_date, "RH"))
                for i in range(len(rh_data)):
                    d["RH " + str(rh_id[i])] = rh_ext

        for i in range(len(sky)):
            d[str(headr[indx[0]][i])] = neat[0][2][i]
        for i in range(len(gro)):
            d[str(headr[indx[1]][i])] = neat[0][3][i]
        d["comments"] = str(neat[0][4][0])
        out = pd.DataFrame(d)

        if os.stat(wname).st_size == 0:
            out.to_csv(wname, index=False, header=True)
        else:
            out.to_csv(wname, index=False, mode="a", header=False)

dir = "./util/tests/data/"

## Data file used for configuration parameters
cname = dir + "_pmat.yml"
## Data file used for user input
rname = dir + 'cool_data.csv'
## Data file used for model input
wname = dir + 'master_data.csv'

## Hours to pull
hour = [00, 12]

## Imports Wyoming and MesoWest Site IDs
cnfg = list(yaml.safe_load_all(open(cname)))[0][3]['import']
## Collects data from cname for PW data collection
keys = [list(x.keys())[0] for x in cnfg]
if ('external' in keys):
    ext_bool = [list(x.keys())[0] for x in cnfg[0]['external']]
    if ('pw' in ext_bool):
        pw_cnfg = cnfg[0]['external'][ext_bool.index('pw')]['pw']
        pw_data = list(map(lambda x: x['path'], pw_cnfg))
        pw_id   = list(map(lambda x: x['id'], pw_cnfg))
    if ('rh' in ext_bool):
        rh_cnfg = cnfg[0]['external'][ext_bool.index('rh')]['rh']
        rh_data = list(map(lambda x: x['path'], pw_cnfg))
        rh_id   = list(map(lambda x: x['id'], pw_cnfg))

if ('wyoming' in keys):
    wy_station = list(map(lambda x: x['id'], cnfg[keys.index('wyoming')]['wyoming']))

## Collects data from cname for PW data collection
if ('mesowest' in keys):
    mw_station = list(map(lambda x: x['id'], cnfg[keys.index('mesowest')]['mesowest']))

## Retrives column index for sensors
headr = pd.read_csv(rname, delimiter=",").columns
indx = [[], []]

for i in range(len(headr)):
    if "Sky" in headr[i]:
        indx[0].append(i)
    elif "Ground" in headr[i] or "Standard" in headr[i]:
        indx[1].append(i)

full_len = len(loadtxt(rname, delimiter=",", dtype=str, usecols=0)) - 1

try:
    last = list(loadtxt(rname,
                        delimiter=",",
                        dtype=str,
                        usecols=0)).index(str(loadtxt(wname,
                                                      delimiter=",",
                                                      dtype=str,
                                                      usecols=0)[-1]))
except IndexError:
    last = 0


for i in range(last, full_len - 1):
    filew = open(rname, "r")
    readw = csv.reader(filew, delimiter=",")
    # print("Collecting {0:d} out of {1:d} days of data\t\t"
    #       "Progress: {2:.2f}%".format(i, full_len - 1,
    #                                   i / (full_len - 1) * 100), end='\r')

    PMAT_Import.impt(dt.strptime(str(loadtxt(rname,
                                             delimiter=",",
                                             dtype=str,
                                             usecols=(0))[i + 1]), "%Y-%m-%d"), i)