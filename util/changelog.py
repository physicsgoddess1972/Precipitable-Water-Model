import yaml

with open("./changelog.yml") as f:
    my_dict = yaml.safe_load(f)

with open("../CHANGELOG.md", 'w', newline='') as csvfile:
    csvfile.write('<a id="top"></a>\n<div class="section timeline">\n')
    for i in my_dict['releases'].keys():
        if my_dict['releases'][i]['released']:
            name = my_dict['releases'][i]['name']
            date = my_dict['releases'][i]['date']
            tagline = my_dict['releases'][i]['tagline']
            csvfile.write("<div class='timeline-item'>\n<div class='content'>")
            csvfile.write('<div class="collapsible">\n')
            csvfile.write('<div class="collapsible-header">\n')
            csvfile.write('<h2>PMAT {}.0 ({}) <span class="label label-rounded text-light text-capitalize tag-date">{}</span></h2>\n'.format(i, name, date))
            csvfile.write('</div>\n')
            csvfile.write('<div class="panel">\n')
            csvfile.write('<h4 style="color:black">{}</h4>\n'.format(tagline))
            csvfile.write('<div>\n')
            for j in my_dict['releases'][i]['changes'].keys():
                csvfile.write('<h3>{}</h3>\n'.format(j.capitalize()))
                for k in list(my_dict['releases'][i]['changes'][str(j)]):
                    csvfile.write('<li style="list-style: none;">\n')
                    if list(k.keys())[0] == "updated":
                        csvfile.write('<span class="label label-rounded text-light text-capitalize tag-changed">Updated</span>\n')
                    elif list(k.keys())[0] == "added":
                        csvfile.write('<span class="label label-rounded text-light text-capitalize tag-added">Added</span>\n')
                    csvfile.write(list(k.values())[0] + "\n")
                    csvfile.write("</li><br>\n")
            csvfile.write('</div></div></div></div></div>')
    csvfile.write('</div>')