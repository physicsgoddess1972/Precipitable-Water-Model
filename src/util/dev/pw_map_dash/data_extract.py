from utah import Utah
from mesowest import MesoWest
import pandas as pd
from datetime import datetime as dt

state_sensors = Utah.request_data()
import os

outdir = './data'
if not os.path.exists(outdir):
    os.mkdir(outdir)

for i in list(state_sensors.keys()):
    dict = {}
    outname = "sensors_{}.csv".format(i)
    fullname = os.path.join(outdir, outname)
    for j in state_sensors[i]:
        df_mw = MesoWest.request_data(dt.strptime('6/26/2021', "%m/%d/%Y"), j)
        if "relative_humidity" in df_mw.columns:
            dict.setdefault(i, {})[j] = state_sensors[i][j]
        else:
            pass
    print(dict)
    df = pd.DataFrame.from_dict(dict)
    print(df)
    df[5] = df[5].str.replace('\n', '')
    df.to_csv(fullname, index=False, header=True)
