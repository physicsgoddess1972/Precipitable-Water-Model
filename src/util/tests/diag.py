# Diagnostic Algthm
import csv
import sys
import datetime
from numpy import *
from datetime import date as dte
from datetime import datetime as dt
import pandas as pd
sys.path.append("./archive")
from mesowest import WyomingUpperAir, MesoWest
import time
import os
import requests
import pprint as pp

from rich import print, box
from rich.panel import Panel
from rich.progress import track
from rich.table import Table
from rich.console import Console

from rich.progress import (
    BarColumn,
    DownloadColumn,
    TextColumn,
    TransferSpeedColumn,
    TimeRemainingColumn,
    Progress,
    TaskID,
)
## Timeout Retry
REQUESTS_MAX_RETRIES = int(os.getenv("REQUESTS_MAX_RETRIES", 10))
adapter = requests.adapters.HTTPAdapter(max_retries=REQUESTS_MAX_RETRIES)

output = Console(record=True)
progress_log = Progress(console=output)

progress = Progress(TextColumn("[bold blue]{task.fields[filename]}", justify="right"),
                    BarColumn(bar_width=None),
                    "[progress.percentage]{task.percentage:>3.1f}%",
                    TimeRemainingColumn())

class Diagnostic:
    def closest(lst, K, d):
        lst = asarray(lst)
        list = []
        tmp2 = dt.combine(d, K)
        for i in range(len(lst)):
            list.append(abs(dt.combine(d, lst[i]) - tmp2))
        idx = asarray(list).argmin()
        return lst[idx]

    def diag_abq(self, date,pw_abq00z, pw_abq12z):
        dat = dt.strptime(date, "%m/%d/%Y")
        try:
            pw00 = WyomingUpperAir.request_data(dat + datetime.timedelta(days=1), "ABQ").pw[0]
        except ValueError:
            pw00 = "NaN"
        if pw_abq00z == "NaN" or pw00 == "NaN":
            final_00    = "[bold orange3]NaN"
            fail_00     = None
        else:
            if float(pw00) == float(pw_abq00z):
                final_00 = "[bold green]Success" # Success
                fail_00  = None
            else:
                final_00 = "[bold red]Fail" # Fail
                fail_00  = "[red]ABQ 00: {} != {}".format(pw00, pw_abq_00z)
        try:
            pw12 = WyomingUpperAir.request_data(dt.combine(dat,datetime.time(12, 0)), "ABQ").pw[0]
        except ValueError:
            pw12 = "NaN"
        if pw_abq12z == "NaN" or pw12 == "NaN":
            final_12    = "[bold orange3]NaN"
            fail_12     = None
        else:
            if float(pw12) == float(pw_abq12z):
                final_12 = "[bold green]Success" # Success
                fail_12  = None
            else:
                final_12 = "[bold red]Fail" # Fail
                fail_12  = "[red]ABQ 12: {} != {}".format(pw12, pw_abq_12z)
        return final_00, final_12, fail_00, fail_12
    def diag_epz(self, date, pw_epz00z, pw_epz12z):
        dat = dt.strptime(date, "%m/%d/%Y")
        try:
            pw00 = WyomingUpperAir.request_data(dat + datetime.timedelta(days=1), "EPZ").pw[0]
        except ValueError:
            pw00 = "NaN"
        if pw_epz00z == "NaN" or pw00 == "NaN":
            final_00    = "[bold orange3]NaN"
            fail_00     = None
        else:
            if float(pw00) == float(pw_epz00z):
                final_00 = "[bold green]Success" # Success
                fail_00  = None
            else:
                final_00 = "[bold red]Fail" # Fail
                fail_00  = "[red]EPZ 00: {} != {}".format(pw00, pw_epz00z)
        try:
            pw12 = WyomingUpperAir.request_data(dt.combine(dat,datetime.time(12, 0)), "EPZ").pw[0]
        except ValueError:
            pw12 = "NaN"
        if pw_epz12z == "NaN" or pw12 == "NaN":
            final_12    = "[bold orange3]NaN"
            fail_12     = None
        else:
            if float(pw12) == float(pw_epz12z):
                final_12 = "[bold green]Success" # Success
                fail_12  = None
            else:
                final_12 = "[bold red]Fail" # Fail
                fail_12  = "[red]EPZ 12: {} != {}".format(pw12, pw_epz12z)
        return final_00, final_12, fail_00, fail_12
    def diag_mesowest(self, date, vtime, rh):
        dat = dt.strptime(date, "%m/%d/%Y")
        df_mw = MesoWest.request_data(dat + datetime.timedelta(days=1), "KONM")
        in_time = pd.to_datetime(vtime).time()
        df_tm = df_mw.loc[(df_mw['Time'] == Diagnostic.closest(df_mw['Time'], in_time, dat))]
        data_mesowest = [df_tm['RH'].values.tolist()[0],
                              df_tm['Time'].values.tolist()[0],
                              round(df_tm['Temp'].values.tolist()[0], 2)]
        if float(rh) == float(data_mesowest[0]):
            final_rh = "[bold green]Success" # Success
            fail_rh  = None
        else:
            final_rh = "[bold red]Fail" # Fail
            fail_rh  = "[red]RH: {} != {}".format(rh, data_mesowest[0])
        return final_rh, fail_rh
    def eval_table(self,date, abq00, abq12, epz00, epz12, rh, fail):
        table = Table(show_header=False, box=None, padding=(0,1,0,0))
        table.add_column("Date", width=12, justify="left", no_wrap=True)
        table.add_column("ABQ 00", width=12, justify="left", no_wrap=True)
        table.add_column("ABQ 12", width=12, justify="left", no_wrap=True)
        table.add_column("EPZ 00", width=12, justify="left", no_wrap=True)
        table.add_column("EPZ 12", width=12, justify="left", no_wrap=True)
        table.add_column("RH", width=12, justify="left", no_wrap=True)
        table.add_row("{}".format(date),"{}".format(abq00),"{}".format(abq12),"{}".format(epz00),"{}".format(epz12),"{}".format(rh))

        if [fail[i] != None for i in range(0, len(fail))] != [False, False, False, False, False]:
            indx = [fail[i] != None for i in range(0, len(fail))].index(True)
            table_fail = Table(show_header=False, box=None, padding=(0,1,0,0))
            table_fail.add_column("Error", width=100, justify="left", no_wrap=True)
            table_fail.add_row("[bold red]{}".format(fail[indx]))
            progress_log.log(table)
            progress_log.log(table_fail)
        else:
            progress_log.log(table)

if __name__ == '__main__':
    fname = "../data/master_data.csv"
    data = loadtxt(fname, skiprows=1,delimiter=",", dtype=str, unpack=True, usecols=(0,2,3,4,5,6,7))

    date       = data[0]
    rh         = data[1]
    pw_abq_12z = data[2]
    pw_abq_00z = data[3]
    pw_epz_12z = data[4]
    pw_epz_00z = data[5]
    vtime      = data[6]
    table = Table(box=None, padding=(0,1,0,0))
    table.add_column("Date", width=12, justify="left", no_wrap=True)
    table.add_column("ABQ 00", width=12, justify="left", no_wrap=True)
    table.add_column("ABQ 12", width=12, justify="left", no_wrap=True)
    table.add_column("EPZ 00", width=12, justify="left", no_wrap=True)
    table.add_column("EPZ 12", width=12, justify="left", no_wrap=True)
    table.add_column("RH", width=12, justify="left", no_wrap=True)

    progress_log.log(table)
    task_id = progress.add_task("download", filename="Diagnostic Algorithm")
    def main(i):
        D = Diagnostic()
        final_abq_00, final_abq_12, fail_abq_00, fail_abq_12 = D.diag_abq(date[i], pw_abq_00z[i], pw_abq_12z[i])
        final_epz_00, final_epz_12, fail_epz_00, fail_epz_12 = D.diag_epz(date[i], pw_epz_00z[i], pw_epz_12z[i])
        final_rh, fail_rh = D.diag_mesowest(date[i],vtime[i], rh[i])
        fail = [fail_abq_00, fail_abq_12, fail_epz_00, fail_epz_12, fail_rh]
        D.eval_table(date[i], final_abq_00, final_abq_12, final_epz_00, final_epz_12, final_rh, fail)
        progress.update(task_id, advance=100./(len(data[0])),refresh=True)

    for i in range(0+200, len(data[0])):
        ex = "requests.exception.HTTPError"
        while ex == "requests.exception.HTTPError":
            try:
                main(i)
                ex = None
            except requests.exceptions.HTTPError as exception:
                time.sleep(60)
                main(i)
                ex = str(exception)
        with open("output.txt", "a") as f:
            export = output.export_text(clear=True)
            if export[0] == "[":
                f.write(export)
            else:
                f.write(export[5:])
