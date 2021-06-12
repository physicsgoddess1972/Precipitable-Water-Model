import yaml, csv, argparse

parser = argparse.ArgumentParser(description="YAML Conversion Module")
parser.add_argument("-I", type=str, help="ID for data import", dest="I")

args = parser.parse_args()

with open("../data/%s/instruments.yml".format(args.I)) as f:
    my_dict = yaml.safe_load(f)
with open("../data/%s/instruments.conf".format(args.I), 'w', newline='') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=my_dict[0]['sensor'].keys())
    writer.writeheader()
    for data in my_dict:
        writer.writerow(data['sensor'])
