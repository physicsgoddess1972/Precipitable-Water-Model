from noaa import NOAA_Access
import time
import datetime
test = NOAA_Access.request_data(datetime.date(2021, 5, 7), '72365023050', 'HourlyRelativeHumidity')
test1 = NOAA_Access.request_dataTypes('')
print(test)