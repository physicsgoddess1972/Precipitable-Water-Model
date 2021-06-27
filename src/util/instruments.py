####
## Title: 	Instrument Configuration Converter
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
####
import yaml, csv

with open("../../data/instruments.yml") as f:
    my_dict = yaml.safe_load(f)

with open("../../data/instruments.conf", 'x', newline='') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=my_dict[0]['sensor'].keys())
    writer.writeheader()
    for data in my_dict:
        writer.writerow(data['sensor'])
