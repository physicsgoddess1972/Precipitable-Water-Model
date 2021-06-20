import yaml, argparse

parser = argparse.ArgumentParser(description='Updating documentation')
parser.add_argument('-t', dest='t')
args = parser.parse_args()


def changelog():
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
                csvfile.write(
                    '<h2>PMAT {}.0 ({}) <span class="label label-rounded text-light text-capitalize tag-date">{}</span></h2>\n'.format(
                        i, name, date))
                csvfile.write('</div>\n')
                csvfile.write('<div class="panel">\n')
                csvfile.write('<h4 style="color:black">{}</h4>\n'.format(tagline))
                csvfile.write('<div>\n')
                for j in my_dict['releases'][i]['changes'].keys():
                    csvfile.write('<h3>{}</h3>\n'.format(j.capitalize()))
                    for k in list(my_dict['releases'][i]['changes'][str(j)]):
                        csvfile.write('<li style="list-style: none;">\n')
                        if list(k.keys())[0] == "updated":
                            csvfile.write(
                                '<span class="label label-rounded text-light text-capitalize tag-changed"><x>(</x>Updated<x>)</x></span>\n')
                        elif list(k.keys())[0] == "added":
                            csvfile.write(
                                '<span class="label label-rounded text-light text-capitalize tag-added"><x>(</x>Added<x>)</x></span>\n')
                        elif list(k.keys())[0] == "fixed":
                            csvfile.write(
                                '<span class="label label-rounded text-light text-capitalize tag-fixed"><x>(</x>Fixed<x>)</x></span>\n')
                        csvfile.write(list(k.values())[0] + "\n")
                        csvfile.write("</li><br>\n")
                csvfile.write('</div></div></div></div></div>')
        csvfile.write('</div>')


def research():
    with open("./research.yml") as f:
        my_dict = yaml.safe_load(f)
    with open("../RESEARCH.md", 'w', newline='') as csvfile:
        csvfile.write('<a id="top"></a>\n')
        csvfile.write('<div class="collapsible" id="papers">\n')
        csvfile.write('<div class="collapsible-header">\n<h2>Papers</h2>\n</div>\n')
        csvfile.write('<div class="panel">\n')
        for i in list(my_dict['research']['papers']):
            title = i['paper']['title']
            author = i['paper']['author']
            journal = i['paper']['journal']
            doi = i['paper']['doi']
            pdf = i['paper']['pdf']
            csvfile.write('<div class="collapsible_1">\n<div class="panel">\n')
            csvfile.write('<h2 style="text-align: center; font-size: 15px">\n{}\n</h2>\n'.format(title))
            csvfile.write('<b style="font-weight: bold">\n{}\n</b>\n'.format(author))
            csvfile.write('<i style="float right">\n{}\n</i>\n'.format(journal))
            csvfile.write('<br><br>\n')
            csvfile.write('<div style="display: flex">\n')
            csvfile.write(
                '<a class="button" target="_blank" style="width: 100%; text-align: center" href="https://doi.org/{}">Web View</a>\n'.format(
                    doi))
            csvfile.write(
                '<a class="button" target="_blank" style="width: 100%; text-align: center" href="{}">PDF View</a>\n'.format(
                    pdf))
            csvfile.write('</div></div></div></div>\n')
        csvfile.write('</div>\n')
        csvfile.write('<div class="collapsible" id="posters">\n')
        csvfile.write('<div class="collapsible-header">\n<h2>Posters</h2>\n</div>\n')
        csvfile.write('<div class="panel">\n')
        for i in list(my_dict['research']['posters']):
            title = i['poster']['title']
            author = i['poster']['author']
            conference = i['poster']['conference']
            abstract = i['poster']['abstract']
            image = i['poster']['image']

            csvfile.write('<div class="collapsible_1">\n<div class="panel">\n')
            csvfile.write('<img src="https://github.com/physicsgoddess1972/Precipitable-Water-Model/blob/docs/docs/assets/img/poster/{}?raw=true" width="100%">\n'.format(image))
            csvfile.write('<h2 style="text-align: center; font-size: 15px">{}</h2>\n'.format(title))
            csvfile.write('<h3 class="research-info" style="float: left">{}</h3>'.format(author))
            csvfile.write(
                '<h3 class="research-info" style="float: right; text-align: right">{}</h3>\n'.format(conference))
            csvfile.write('<p>\n{}\n</p>\n'.format(abstract))
            csvfile.write('</div></div>')
        csvfile.write('</div></div>')


def dash():
    with open("./dash.yml") as f:
        my_dict = yaml.safe_load(f)
    with open("../docs/assets/external/results.html", 'w', newline='') as csvfile:
        csvfile.write('<script type="text/javascript">$(document).ready(function(){$(".tabs").tabs();});</script>\n')
        csvfile.write('<div class="mdl-grid demo-content">\n')
        csvfile.write('<div class="mdl-shadow--2dp mdl-cell mdl-cell--stretch mdl-cell--12-col">\n')
        csvfile.write('\t<div class="row">\n\t\t<div class="col s12">\n')
        csvfile.write('\t\t\t<ul class="tabs">\n')
        for i in list(my_dict['dash']):
            csvfile.write('\t\t\t\t<li class="tab col s3"><a href="#{0}-{1}">{2}, {3}</a></li>\n'.format(
                (i['data']['city']).lower().replace(" ", ""), (i['data']['state']).lower(), i['data']['city'],
                i['data']['state']))
        csvfile.write('\t\t\t</ul>\n\t\t</div>\n')
        for i in list(my_dict['dash']):
            state = (i['data']['state']).lower()
            city = (i['data']['city']).lower().replace(" ", "")
            csvfile.write('\t\t<div class="col s12" id="{0}-{1}">\n'.format(city, state))
            csvfile.write('\t\t\t<div style="display: flex;">\n')
            csvfile.write('\t\t\t\t<div class="data-nav" style="height: 250px">\n')
            csvfile.write('\t\t\t\t\t<div class="mdl-card__actions mdl-card--border">\n')
            csvfile.write('\t\t\t\t\t\t<div class="row">\n')
            csvfile.write('\t\t\t\t\t\t\t<div class="col s12">\n')
            csvfile.write('\t\t\t\t\t\t\t\t<ul class="tabs">\n')
            csvfile.write(
                '\t\t\t\t\t\t\t\t\t<li class="tab col s3"><a href="#clear-{}"><i class="material-icons">brightness_5</i></a></li>\n'.format(
                    state))
            csvfile.write(
                '\t\t\t\t\t\t\t\t\t<li class="tab col s3"><a href="#over-{}"><i class="material-icons">cloud</i></a></li>\n'.format(
                    state))
            csvfile.write(
                '\t\t\t\t\t\t\t\t\t<li class="tab col s3"><a href="#chart-{}"><i class="material-icons">insert_chart_outlined</i></a></li>\n'.format(
                    state))
            csvfile.write(
                '\t\t\t\t\t\t\t\t\t<li class="tab col s3"><a href="#poster-{}"><i class="material-icons">table_chart</i></a></li>\n'.format(
                    state))
            csvfile.write('\t\t\t\t\t\t\t\t</ul>\n\t\t\t\t\t\t\t</div>\n\t\t\t\t\t\t</div>\n')
            csvfile.write('\t\t\t\t\t\t<div id="clear-{}" class="col s12">\n'.format(state))
            csvfile.write('\t\t\t\t\t\t\t<h5>Plot Set: Clear Sky</h5>\n')
            csvfile.write('\t\t\t\t\t\t\t<ul>\n')
            csvfile.write(
                '\t\t\t\t\t\t\t\t<li><a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/raw/pmat-{}-{}/figs/results/time_series.pdf" download>Time Series</a></li>\n'.format(
                    city, state))
            csvfile.write(
                '\t\t\t\t\t\t\t\t<li><a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/raw/pmat-{}-{}/figs/results/analytics.pdf" download>Analytics</a></li>\n'.format(
                    city, state))
            csvfile.write(
                '\t\t\t\t\t\t\t\t<li><a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/raw/pmat-{}-{}/figs/results/sensor.pdf" download>Sensor Time Series</a></li>\n'.format(
                    city, state))
            csvfile.write(
                '\t\t\t\t\t\t\t\t<li><a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/raw/pmat-{}-{}/figs/results/pacman.pdf" download>Pacman</a></li>\n'.format(
                    city, state))
            csvfile.write('\t\t\t\t\t\t\t</ul>\n\t\t\t\t\t\t</div>\n')
            csvfile.write('\t\t\t\t\t\t<div id="over-{}" class="col s12">\n'.format(state))
            csvfile.write('\t\t\t\t\t\t\t<h5>Plot Set: Overcast</h5>\n')
            csvfile.write('\t\t\t\t\t\t\t<ul>\n')
            csvfile.write(
                '\t\t\t\t\t\t\t\t<li><a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/raw/pmat-{}-{}/figs/results/time_series_overcast.pdf" download>Time Series</a></li>\n'.format(
                    city, state))
            csvfile.write(
                '\t\t\t\t\t\t\t\t<li><a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/raw/pmat-{}-{}/figs/results/analytics_overcast.pdf" download>Analytics</a></li>\n'.format(
                    city, state))
            csvfile.write(
                '\t\t\t\t\t\t\t\t<li><a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/raw/pmat-{}-{}/figs/results/sensor_overcast.pdf" download>Sensor Time Series</a></li>\n'.format(
                    city, state))
            csvfile.write(
                '\t\t\t\t\t\t\t\t<li><a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/raw/pmat-{}-{}/figs/results/pacman_overcast.pdf" download>Pacman</a></li>\n'.format(
                    city, state))
            csvfile.write('\t\t\t\t\t\t\t</ul>\n\t\t\t\t\t\t</div>\n')
            csvfile.write('\t\t\t\t\t\t<div id="chart-{}" class="col s12">\n'.format(state))
            csvfile.write('\t\t\t\t\t\t\t<h5>Plot Set: Charts</h5>\n')
            csvfile.write('\t\t\t\t\t\t\t<ul>\n')
            csvfile.write(
                '\t\t\t\t\t\t\t\t<li><a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/raw/pmat-{}-{}/figs/results/charts.pdf" download>Charts</a></li>\n'.format(
                    city, state))
            csvfile.write('\t\t\t\t\t\t\t</ul>\n\t\t\t\t\t\t</div>\n')
            csvfile.write('\t\t\t\t\t\t<div id="poster-{}" class="col s12">\n'.format(state))
            csvfile.write('\t\t\t\t\t\t\t<h5>Plot Set: Poster</h5>\n')
            csvfile.write('\t\t\t\t\t\t\t<ul>\n')
            csvfile.write(
                '\t\t\t\t\t\t\t\t<li><a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/raw/pmat-{}-{}/figs/results/poster.pdf" download>Poster</a></li>\n'.format(
                    city, state))
            csvfile.write('\t\t\t\t\t\t\t</ul>\n\t\t\t\t\t\t</div>\n')
            csvfile.write('\t\t\t\t\t</div>\n\t\t\t\t</div>\n')
            csvfile.write('\t\t\t<div class="img-small-{}" style="width: 50%; padding-top: 15px; padding-right: 5px"></div>\n'.format(state))
            csvfile.write('\t\t\t</div>\n\t\t</div>\n')
        csvfile.write('\t</div>\t\n</div>\n</div>')


if args.t == 'dash':
    dash()

if args.t == 'changelog':
    changelog()

if args.t == 'research':
    research()
