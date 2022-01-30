import yaml, argparse

parser = argparse.ArgumentParser(description='Updating documentation')
parser.add_argument('-t', dest='t')
args = parser.parse_args()


def changelog():
    with open("./changelog.yml") as f:
        my_dict = list(yaml.load_all(f))

    with open("./changelog.rst", 'w', newline='') as csvfile:
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
                    csvfile.write('{a}\n{b}\n{a}\n\n'.format(a="-"*(len(name)), b=j.capitalize()))
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


def research():
    with open("./research.yml") as f:
        my_dict = list(yaml.load_all(f, Loader=yaml.FullLoader))

    with open("../RESEARCH.md", 'w', newline='') as csvfile:
        csvfile.write('<a id="top"></a>\n')
        csvfile.write('<div class="collapsible" id="papers">\n')
        csvfile.write('\t<div class="collapsible-header">\n\t\t<h2>Papers</h2>\n\t</div>\n')
        csvfile.write('\t<div class="panel">\n')
        for i in list(my_dict):
            if 'paper' in i:
                title = i['paper']['title']
                author = i['paper']['author']
                journal = i['paper']['journal']
                doi = i['paper']['doi']
                pdf = i['paper']['pdf']
                status = i['paper']['status']
                csvfile.write('\t\t<div class="collapsible_1">\n\t\t\t<div class="panel">\n')
                csvfile.write('\t\t\t\t<h2 style="text-align: center; font-size: 15px">{}</h2>\n'.format(title))
                csvfile.write('\t\t\t\t<b style="font-weight: bold">{}</b>\n\t\t\t\t<br>\n'.format(author))
                csvfile.write('\t\t\t\t<i>{}</i>\n\t\t\t\t<br>\n'.format(journal))
                csvfile.write('\t\t\t\t<b>{}</b>\n'.format(status))
                csvfile.write('\t\t\t\t<br><br>\n')
                csvfile.write('\t\t\t\t<div style="display: flex">\n')
                csvfile.write(
                    '\t\t\t\t\t<a class="button" target="_blank" style="width: 100%; text-align: center" href="https://doi.org/{}">Web View</a>\n'.format(
                        doi))
                csvfile.write(
                    '\t\t\t\t\t<a class="button" target="_blank" style="width: 100%; text-align: center" href="{}">PDF View</a>\n'.format(
                        pdf))
                csvfile.write('\t\t\t\t</div>\n\t\t\t</div>\n\t\t</div>\n\t</div>\n')
        csvfile.write('</div>\n')
        csvfile.write('<div class="collapsible" id="posters">\n')
        csvfile.write('\t<div class="collapsible-header">\n\t\t<h2>Posters</h2>\n\t</div>\n')
        csvfile.write('\t<div class="panel">\n')
        for i in list(my_dict):
            if 'poster' in i:
                title = i['poster']['title']
                author = i['poster']['author']
                conference = i['poster']['conference']
                abstract = i['poster']['abstract']
                image = i['poster']['image']

                csvfile.write('\t\t<div class="collapsible_1">\n\t\t\t<div class="panel">\n')
                csvfile.write('\t\t\t\t<img src="https://github.com/physicsgoddess1972/Precipitable-Water-Model/blob/docs/docs/assets/img/poster/{}?raw=true" width="100%">\n'.format(image))
                csvfile.write('\t\t\t\t<h2 style="text-align: center; font-size: 15px">{}</h2>\n'.format(title))
                csvfile.write('\t\t\t\t<b>{}</b>\n\t\t\t\t<br>\n'.format(author))
                csvfile.write('\t\t\t\t<i>{}</i>\n\t\t\t\t<hr>\n'.format(conference))
                csvfile.write('\t\t\t\t<p>{}</p>\n'.format(abstract))
                csvfile.write('\t\t\t</div>\n\t\t</div>\n')
        csvfile.write('\t</div>\n</div>')


def dash():
    with open("./dash.yml") as f:
        my_dict = list(yaml.load_all(f, Loader=yaml.FullLoader))
    with open("../docs/assets/external/results.html", 'w', newline='') as csvfile:
        csvfile.write('<script type="text/javascript">$(document).ready(function(){$(".tabs").tabs();});</script>\n')
        csvfile.write("<script>function getChcked(){var chks = document.querySelectorAll('input[type=\"checkbox\"]');var checked = [];for(var i = 0; i < chks.length; i++){if(chks[i].checked){checked.push(chks[i].name)}}return checked;}</script>\n")
        csvfile.write("<script>var arr = [];$('#button').click(function(){{for (var i = 0; i < getChcked().length; i++){{arr.push('https://github.com/physicsgoddess1972/Precipitable-Water-Model/raw/pmat-'+ $('.tabs .active').attr('href').split('#').pop() + '/figs/results/' + getChcked()[i] + '.pdf')}}download_files(arr);}})</script>\n")
        csvfile.write('<div class="mdl-grid demo-content">\n')
        csvfile.write('<div class="mdl-shadow--2dp mdl-cell mdl-cell--stretch mdl-cell--12-col" style="background-color: white">\n')
        csvfile.write('\t<div class="row">\n\t\t<div class="col s12">\n')
        csvfile.write('\t\t\t<ul class="tabs">\n')
        csvfile.write('\t\t\t\t<li class="tab col s3" style="padding-top: 10px; padding-left: 10px; padding-right: 10px"><i style="color: #4C69EC" id="button" name="action" type="submit"><i class="material-icons">save</i></i></li>\n')
        for i in list(my_dict):
            if i['active'] == True:
                csvfile.write('\t\t\t\t<li class="tab col s3"><a href="#{0}-{1}">{2}, {3}</a></li>\n'.format((i['city']).lower().replace(" ", ""), (i['state']).lower(), i['city'],i['state']))
            else:
                csvfile.write('\t\t\t\t<li class="tab col s3 disabled"><a href="#{0}-{1}">{2}, {3}</a></li>\n'.format((i['city']).lower().replace(" ", ""), (i['state']).lower(), i['city'], i['state']))
        csvfile.write('\t\t\t</ul>\n\t\t</div>\n')
        for i in list(my_dict):
            state = (i['state']).lower()
            city = (i['city']).lower().replace(" ", "")
            img = "https://github.com/physicsgoddess1972/Precipitable-Water-Model/blob/docs/docs/assets/img/dash/{}?raw=true/".format(i['img'])
            csvfile.write('\t\t<div class="col s12" id="{0}-{1}">\n'.format(city, state))
            csvfile.write('\t\t\t<div style="display: flex;">\n')
            csvfile.write('\t\t\t\t<div class="data-nav" style="height: 250px">\n')
            csvfile.write('\t\t\t\t\t<div class="mdl-card__actions mdl-card--border">\n')
            csvfile.write('\t\t\t\t\t\t<div class="row">\n')
            csvfile.write('\t\t\t\t\t\t\t<div class="col s12">\n')
            csvfile.write('\t\t\t\t\t\t\t\t<ul class="tabs">\n')
            csvfile.write('\t\t\t\t\t\t\t\t\t<li class="tab col s3"><a href="#clear-{}"><i class="material-icons">brightness_5</i></a></li>\n'.format(state))
            csvfile.write('\t\t\t\t\t\t\t\t\t<li class="tab col s3"><a href="#over-{}"><i class="material-icons">cloud</i></a></li>\n'.format(state))
            csvfile.write('\t\t\t\t\t\t\t\t\t<li class="tab col s3"><a href="#chart-{}"><i class="material-icons">insert_chart_outlined</i></a></li>\n'.format(state))
            csvfile.write('\t\t\t\t\t\t\t\t\t<li class="tab col s3"><a href="#poster-{}"><i class="material-icons">table_chart</i></a></li>\n'.format(state))
            csvfile.write('\t\t\t\t\t\t\t\t</ul>\n\t\t\t\t\t\t\t</div>\n\t\t\t\t\t\t</div>\n')
            csvfile.write('\t\t\t\t\t\t<div id="clear-{}" class="col s12">\n'.format(state))
            csvfile.write('\t\t\t\t\t\t\t<h5>Plot Set: Clear Sky</h5>\n')
            csvfile.write('\t\t\t\t\t\t\t<form action="#">\n')
            csvfile.write('\t\t\t\t\t\t\t\t<a><label><input type="checkbox" name="time_series"><span>Time Series</span></label></a><br>\n')
            csvfile.write('\t\t\t\t\t\t\t\t<a><label><input type="checkbox" name="analytics"><span>Analytics</span></label></a><br>\n')
            csvfile.write('\t\t\t\t\t\t\t\t<a><label><input type="checkbox" name="sensor"><span>Sensor Time Series</span></label></a><br>\n')
            csvfile.write('\t\t\t\t\t\t\t\t<a><label><input type="checkbox" name="sensor"><span>Sensor Time Series</span></label></a><br>\n')
            csvfile.write('\t\t\t\t\t\t\t</form>\n\t\t\t\t\t\t</div>\n')
            csvfile.write('\t\t\t\t\t\t<div id="over-{}" class="col s12">\n'.format(state))
            csvfile.write('\t\t\t\t\t\t\t<h5>Plot Set: Overcast</h5>\n')
            csvfile.write('\t\t\t\t\t\t\t<form action="#">\n')
            csvfile.write('\t\t\t\t\t\t\t\t<a><label><input type="checkbox" name="time_series_overcast"><span>Time Series</span></label></a><br>\n')
            csvfile.write('\t\t\t\t\t\t\t\t<a><label><input type="checkbox" name="analytics_overcast"><span>Analytics</span></label></a><br>\n')
            csvfile.write('\t\t\t\t\t\t\t\t<a><label><input type="checkbox" name="sensor_overcast"><span>Sensor Time Series</span></label></a><br>\n')
            csvfile.write('\t\t\t\t\t\t\t\t<a><label><input type="checkbox" name="pacman_overcast"><span>Pacman</span></label></a><br>\n')

            csvfile.write('\t\t\t\t\t\t\t</form>\n\t\t\t\t\t\t</div>\n')
            csvfile.write('\t\t\t\t\t\t<div id="chart-{}" class="col s12">\n'.format(state))
            csvfile.write('\t\t\t\t\t\t\t<h5>Plot Set: Charts</h5>\n')
            csvfile.write('\t\t\t\t\t\t\t<form action="#">\n')
            csvfile.write('\t\t\t\t\t\t\t\t<a><label><input type="checkbox" name="charts"><span>Charts</span></label></a><br>')
            csvfile.write('\t\t\t\t\t\t\t</form>\n\t\t\t\t\t\t</div>\n')
            csvfile.write('\t\t\t\t\t\t<div id="poster-{}" class="col s12">\n'.format(state))
            csvfile.write('\t\t\t\t\t\t\t<h5>Plot Set: Poster</h5>\n')
            csvfile.write('\t\t\t\t\t\t\t<form action="#">\n')
            csvfile.write('\t\t\t\t\t\t\t\t<a><label><input type="checkbox" name="poster"><span>Poster</span></label></a><br>')
            csvfile.write('\t\t\t\t\t\t\t</form>\n\t\t\t\t\t\t</div>\n')
            csvfile.write('\t\t\t\t\t</div>\n\t\t\t\t</div>\n')
            csvfile.write('\t\t\t<div class="img-small-{}" style="width: 50%; padding-top: 15px; padding-right: 5px"></div>\n'.format(state, img))
            csvfile.write('\t\t\t</div>\n\t\t</div>\n')
        csvfile.write('\t</div>\t\n</div>\n</div>')


if args.t == 'dash':
    dash()

if args.t == 'changelog':
    changelog()

if args.t == 'research':
    research()
