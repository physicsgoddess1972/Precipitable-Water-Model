<<<<<<< HEAD
####
## Title: 	Instrument Configuration Converter
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
####
import yaml, csv

with open("../../data/instruments.yml") as f:
    my_dict = yaml.safe_load(f)

with open("instruments.conf", 'x', newline='') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=my_dict[0]['sensor'].keys())
    writer.writeheader()
    for data in my_dict:
        writer.writerow(data['sensor'])
=======
####
## Title: 	Instrument Configuration Converter
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
####
import yaml, csv

with open("../../data/instruments.yml") as f:
    my_dict = yaml.safe_load(f)

with open("instruments.conf", 'x', newline='') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=my_dict[0]['sensor'].keys())
    writer.writeheader()
    for data in my_dict:
        writer.writerow(data['sensor'])
>>>>>>> 47addb531525c32ecd60a78ae867ca064c77857a
