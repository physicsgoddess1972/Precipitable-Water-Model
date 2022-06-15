import yaml, argparse

parser = argparse.ArgumentParser(description='Updating documentation')
parser.add_argument('-t', dest='t')
args = parser.parse_args()


def changelog():
    with open("./_data/changelog.yml") as f:
        my_dict = list(yaml.load_all(f, Loader=yaml.FullLoader))

    with open("../docsrc/changelog.rst", 'w', newline='') as csvfile:
        csvfile.write("***********\nChangelog\n***********\n\n")
        for i in my_dict:
            if i['released']:
                name = i['name']
                date = i['date']
                tagline = i['tagline']
                version = i['version']
                csvfile.write("{a}\nPMAT {b}\n{a}\n\n".format(a="="*(len(name)+5), b=name))
                csvfile.write(":Version: {}\n".format(version))
                csvfile.write(':Date: {}\n'.format(date))
                csvfile.write(':Tagline: {}\n\n'.format(tagline))
                for j in i['changes'].keys():
                    csvfile.write('{a}\n{b}\n{a}\n\n'.format(a="-"*(len(j)), b=j.capitalize()))
                    for k in list(i['changes'][str(j)]):
                        if list(k.keys())[0] == "updated":
                            csvfile.write('- [Updated] {}\n'.format(list(k.values())[0]))
                        elif list(k.keys())[0] == "added":
                            csvfile.write('- [Added] {}\n'.format(list(k.values())[0]))
                        elif list(k.keys())[0] == "fixed":
                            csvfile.write('- [Fixed] {}\n'.format(list(k.values())[0]))
                        elif list(k.keys())[0] == "misc":
                            csvfile.write('- [Misc] {}\n'.format(list(k.values())[0]))
                    csvfile.write("\n")

                csvfile.write("\n")

if args.t == 'changelog':
    changelog()

