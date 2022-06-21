####
## Title: 	MesoWest Data Extraction Submodule
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
####

import pandas as pd
import json
import warnings

warnings.simplefilter("ignore")

from http_util import HTTPEndPoint

class NOAA_Access(HTTPEndPoint):
    def __init__(self):
        """Set up endpoint."""
        super(NOAA_Access, self).__init__('https://www.ncei.noaa.gov/')

    @classmethod
    def request_data(cls, date, site_id, data, **kwargs):
        endpoint = cls()
        df = endpoint._get_data(date, site_id, data)
        return df

    @classmethod
    def request_dataset(cls, **kwargs):
        endpoint = cls()
        df = endpoint._get_datasets()
        return df

    @classmethod
    def request_dataTypes(cls, datasets, **kwargs):
        endpoint = cls()
        df = endpoint._get_data_types(datasets)
        return df

    def _get_data(self, date, site_id, data):
        raw_data = self._get_data_raw(date, site_id, data)
        df = pd.DataFrame(json.loads(raw_data))
        return df

    def _get_datasets(self):
        path = self.get_path("access/services/support/v3/datasets.json")
        dict = json.loads(path.text)['results']
        df = pd.DataFrame(dict)
        pd.set_option('display.max_rows', df.shape[0] + 1)
        return df

    def _get_data_types(self, datasets):
        path = self.get_path("access/services/support/v3/datasets/{}.json".format(datasets))
        dict = json.loads(path.text)['dataTypes']
        df = pd.DataFrame(dict)
        pd.set_option('display.max_rows', df.shape[0] + 1)
        return df

    def _get_data_raw(self, date, site_id, data):
        path = ('access/services/data/v1?dataset=local-climatological-data&stations={stid}'
                '&dataTypes={data}'
                '&startDate={date:%Y}-{date:%m}-{date:%d}'
                '&endDate={date:%Y}-{date:%m}-{date:%d}'
                '&boundingBox=90,-180,-90,180'
                '&includeAttributes=true&format=json&units=metric'
                '&includeStationName=true'
                ).format(date=date, stid=site_id, data=data)
        resp = self.get_path(path)
        return resp.text
