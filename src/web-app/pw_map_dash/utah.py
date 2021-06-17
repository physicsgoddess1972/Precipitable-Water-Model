from datetime import datetime
from io import StringIO

from bs4 import BeautifulSoup
import numpy as np
import pandas as pd

import warnings

warnings.simplefilter("ignore")

from http_util import HTTPEndPoint

class Utah(HTTPEndPoint):
    """Download and parse data from the University of Utah's MesoWest archive."""
    def __init__(self):
        """Set up endpoint."""
        super(Utah, self).__init__('https://mesowest.utah.edu/')

    @classmethod
    def request_data(cls, **kwargs):
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
        df = endpoint._get_data()
        return df

    def _get_data(self):
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
        raw_data = self.get_path('cgi-bin/droman/meso_station.cgi?area=1').text
        soup = BeautifulSoup(raw_data, 'html.parser')
        names = pd.DataFrame.from_records([[td.find_next(text=True) for td in tr.find_all(text=True, recursive=True)] for tr in soup.find_all('p')])
        state = {}

        for i in range(2, 140902, 2):
            info = names[i].tolist()
            label = names[i - 1].tolist()
            list = [k for k in [j.split("  ") for j in info][0] if k]
            sts = [n for n in [m.split(" ") for m in [list[1]] if m][0] if n]
            print(sts)
            if list[1].isupper() and list[1].isalpha() and (len(sts) == 3 or len(sts) == 2):
                state.setdefault(sts[0], {})[label[0]] = list
            elif list[1].isalnum() or list[1].isdigit():
                sts1 = [n for n in [m.split(" ") for m in [list[2]] if m][0] if n][0]
                state.setdefault(sts1, {})[label[0]] = list
            else:
                state.setdefault("XX", {})[label[0]] = list
        return state