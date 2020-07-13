from datetime import datetime
from io import StringIO
import warnings, sys

from bs4 import BeautifulSoup
import numpy as np
import pandas as pd

from http_util import HTTPEndPoint

warnings.simplefilter("ignore")

def get_wind_components(speed, wdir):
    u = -speed * np.sin(wdir)
    v = -speed * np.cos(wdir)
    return u, v


class WyomingUpperAir(HTTPEndPoint):
    """Download and parse data from the University of Wyoming's upper air archive."""

    def __init__(self):
        """Set up endpoint."""
        super(WyomingUpperAir, self).__init__('http://weather.uwyo.edu/cgi-bin/sounding')

    @classmethod
    def request_data(cls, time, site_id, **kwargs):
        r"""Retrieve upper air observations from the Wyoming archive.

        Parameters
        ----------
        time : datetime
            The date and time of the desired observation.

        site_id : str
            The three letter ICAO identifier of the station for which data should be
            downloaded.

        kwargs
            Arbitrary keyword arguments to use to initialize source

        Returns
        -------
            :class:`pandas.DataFrame` containing the data

        """
        endpoint = cls()
        df = endpoint._get_data(time, site_id)
        return df

    def _get_data(self, time, site_id):
        r"""Download and parse upper air observations from an online archive.

        Parameters
        ----------
        time : datetime
            The date and time of the desired observation.

        site_id : str
            The three letter ICAO identifier of the station for which data should be
            downloaded.

        Returns
        -------
            :class:`pandas.DataFrame` containing the data

        """
        raw_data = self._get_data_raw(time, site_id)
        soup = BeautifulSoup(raw_data, 'html.parser')

        tabular_data = StringIO(soup.find_all('pre')[0].contents[0])
        col_names = ['pressure', 'height', 'temperature', 'dewpoint', 'direction', 'speed']
        df = pd.read_fwf(tabular_data, skiprows=5, usecols=[0, 1, 2, 3, 6, 7], names=col_names)
        df['u_wind'], df['v_wind'] = get_wind_components(df['speed'],
                                                         np.deg2rad(df['direction']))

        # Drop any rows with all NaN values for T, Td, winds
        df = df.dropna(subset=('temperature', 'dewpoint', 'direction', 'speed',
                               'u_wind', 'v_wind'), how='all').reset_index(drop=True)

        # Parse metadata
        meta_data = soup.find_all('pre')[1].contents[0]
        lines = meta_data.splitlines()

        # If the station doesn't have a name identified we need to insert a
        # record showing this for parsing to proceed.
        if 'Station number' in lines[1]:
            lines.insert(1, 'Station identifier: ')

        station = lines[1].split(':')[1].strip()
        station_number = int(lines[2].split(':')[1].strip())
        sounding_time = datetime.strptime(lines[3].split(':')[1].strip(), '%y%m%d/%H%M')
        latitude = float(lines[4].split(':')[1].strip())
        longitude = float(lines[5].split(':')[1].strip())
        elevation = float(lines[6].split(':')[1].strip())
        pw = float(lines[-1].split(':')[1].strip())

        df['station'] = station
        df['station_number'] = station_number
        df['time'] = sounding_time
        df['latitude'] = latitude
        df['longitude'] = longitude
        df['elevation'] = elevation
        df['pw'] = pw

        # Add unit dictionary
        df.units = {'pressure': 'hPa',
                    'height': 'meter',
                    'temperature': 'degC',
                    'dewpoint': 'degC',
                    'direction': 'degrees',
                    'speed': 'knot',
                    'u_wind': 'knot',
                    'v_wind': 'knot',
                    'station': None,
                    'station_number': None,
                    'time': None,
                    'latitude': 'degrees',
                    'longitude': 'degrees',
                    'elevation': 'meter',
                    'pw': 'millimeter'}
        return df

    def _get_data_raw(self, time, site_id):
        """Download data from the University of Wyoming's upper air archive.

        Parameters
        ----------
        time : datetime
            Date and time for which data should be downloaded
        site_id : str
            Site id for which data should be downloaded

        Returns
        -------
        text of the server response

        """
        path = ('?region=naconf&TYPE=TEXT%3ALIST'
                '&YEAR={time:%Y}&MONTH={time:%m}&FROM={time:%d%H}&TO={time:%d%H}'
                '&STNM={stid}').format(time=time, stid=site_id)

        resp = self.get_path(path)
        # See if the return is valid, but has no data
        if resp.text.find("Can't") != -1:
            raise ValueError(
                'No data available for {time:%Y-%m-%d %HZ} '
                'for station {stid}.'.format(time=time, stid=site_id))

        return resp.text

class MesoWest(HTTPEndPoint):
    def __init__(self):
        """Set up endpoint."""
        super(MesoWest, self).__init__('https://mesowest.utah.edu/cgi-bin/droman/meso_table_mesowest.cgi')

    @classmethod
    def request_data(cls, time, site_id, **kwargs):
        endpoint = cls()
        df = endpoint._get_data(time, site_id)
        return df

    def _get_data(self, time, site_id):
        raw_data = self._get_data_raw(time, site_id)
        soup = BeautifulSoup(raw_data, 'html.parser')
        soup_super_lst = []
        for j in range(0, len(soup.find_all("tr"))):
            soup_tmp = soup.find_all("tr")[j].find_all('td')
            soup_lst = []
            for i in soup_tmp:
                soup_tmp1 = list(i)[0].strip('\n\t\t')
                try:
                    soup_lst.append(float(soup_tmp1))
                except:
                    soup_lst.append(soup_tmp1)
            soup_super_lst.append(soup_lst)
        col_names = ['Time', 'Temp', 'Dewpoint', 'WBT', 'RH']#, 'Wind Speed', 'Wind Direction', 'Pressure', 'Sea Level Pressure', 'Altimeter', '1500m Pressure', 'Condition', 'Visibility']
        df_i = pd.DataFrame(soup_super_lst)
        df_i = df_i[[0,1,2,3,4]].reset_index(drop=True)
        df = df_i.dropna(how='any', axis=0)
        df.columns = col_names
        df.Time = pd.to_datetime(df['Time']).apply(lambda x: x.time())
        try:
            df.Temp = (df.Temp - 32.) * (5./9.)
        except TypeError:
            df.Temp = np.nan
        try:
            df.Dewpoint = (df.Dewpoint - 32.) * (5./9.)
        except TypeError:
            df.Dewpoint = np.nan
        try:
            df.WBT = (df.WBT - 32.) * (5./9.)
        except TypeError:
            df.WBT = np.nan
        return df

    def _get_data_raw(self, time, site_id):
        path = ('?stn={stid}'
                '&unit=0&time=LOCAL'
                '&day1={time:%d}&month1={time:%m}&year1={time:%Y}&hour1={time:%H}'
                '&graph=0&past=1').format(time=time, stid=site_id)
        resp = self.get_path(path)
        # See if the return is valid, but has no data
        if resp.text.find("Can't") != -1:
            raise ValueError(
                'No data available for {time:%Y-%m-%d %HZ} '
                'for station {stid}.'.format(time=time, stid=site_id))

        return resp.text
