# Import required libraries
import os, re
from random import randint

import flask
from flask import Flask, render_template

import dash
#from dash.dependencies import Input, Output
import dash_core_components as dcc
import dash_html_components as html

import chart_studio
import chart_studio.plotly as py
import plotly.graph_objects as go
import plotly.figure_factory as ff

from sklearn import *
from sklearn import svm
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.metrics import confusion_matrix, classification_report, \
                            precision_score, jaccard_score, matthews_corrcoef, f1_score

import pandas as pd
from numpy import *
import datetime
import itertools
from datetime import datetime as dt

layout="""
<!doctype html>
<!--suppress ALL -->
<html>
<head>
	<title>Precipitable Water Model</title>
	<link rel="icon" href="./assets/icon.png">

    {%favicon%}
    <script src="./assets/jquery.min.js"></script>
	<script src='./assets/legacy.js'></script>
	<script src='./assets/script.js'></script>
	<script src="./assets/materialize.min.js"></script>

    <link rel='stylesheet' href='./assets/material.cyan-light_blue.min.css'>
    <link rel='stylesheet' href='./assets/style.css'>
    <link rel='stylesheet' href='./assets/materialize.min.css'>
	<link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.6.0/css/all.css">
	<link rel="stylesheet" href="https://fonts.googleapis.com/icon?family=Material+Icons" >
	<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Source+Code+Pro&display=swap">
    <link rel='stylesheet' href='./assets/dash.css'>
    {%css%}
</head>
<body role='flatdoc'>
<div class="demo-layout mdl-layout mdl-js-layout mdl-layout--fixed-drawer mdl-layout--fixed-header">
	<header class="demo-header mdl-layout__header mdl-color--grey-100 mdl-color-text--grey-600">
		<div class="mdl-layout__header-row">
			<span class="mdl-layout-title">Precipitable Water Model</span>
			<div class="mdl-layout-spacer"></div>
		</div>
	</header>
	<div class="demo-drawer mdl-layout__drawer mdl-color--blue-grey-900 mdl-color-text--blue-grey-50">
		<nav class="demo-navigation mdl-navigation mdl-color--blue-grey-900">
			<a class="mdl-navigation__link" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/dash.html"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">dashboard</i>Home</a>
			<a class="mdl-navigation__link" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">chrome_reader_mode</i>Documentation</a>
			<a class="mdl-navigation__link" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/contrib.html"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">people</i>Contribute</a>
			<a class="mdl-navigation__link" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/code.html"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">code</i>R Features</a>
			<a class="mdl-navigation__link" onclick="$('#maintainers').modal('open');"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">face</i>The Maintainers</a>
			<hr>
			<a class="mdl-navigation__link" href="https://github.com/physicsgoddess1972/Precipitable-Water-Model"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">people</i>View on Github</a>
			<a class="mdl-navigation__link" href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/archive/master.zip"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">cloud_download</i>Download the Repo</a>
			<a class="mdl-navigation__link" href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/issues"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">bug_report</i>Bug Report</a>
			<a class="mdl-navigation__link" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/changelog.html"><i class="mdl-color-text--blue-grey-400 material-icons" role="presentation">new_releases</i>Changelog</a>

		</nav>
	</div>
	<main class="mdl-layout__content">
		<div class="modal" id="maintainers">
			<div class="modal-content">
				<h4>The Maintainers</h4>
				<table>
					<tr>
						<td><i class="material-icons">face</i></td>
						<td>Spencer Riley</td>
						<td><i class="material-icons">face</i></td>
						<td>Vicki Kelsey</td>
					</tr>
					<tr>
						<td><i class="material-icons">public</i></td>
						<td><a target="_blank" href="http://pharaohcola13.github.io">pharaohcola13.github.io</a></td>
						<td><i class="material-icons">public</i></td>
						<td><a target="_blank" href="http://physicsgoddess1972.github.io">physicsgoddess1972.github.io</a></td>
					</tr>
					<tr>
						<td><i class="material-icons">alternate_email</i></td>
						<td>spencer.riley@student.nmt.edu</td>
						<td><i class="material-icons">alternate_email</i></td>
						<td>vicki.kelsey@student.nmt.edu</td>
					</tr>
				</table>
			</div>
			<div class="modal-footer">
				<a href="#!" class="modal-close waves-effect waves-teal btn-flat" data-dismiss="maintainers">Continue</a>
			</div>

		</div>
		<div class="modal" id="initial">
			<div class="modal-content">
				<h4 style="text-align: center">Welcome to the Precipitable Water Model Documentation Page</h4>
				<p>
					If you are interested in conducting this experiment in your
					location read through this page and follow the process.
				</p>
				<p>
					It should be noted that you have no obligation to assimilate your data into this project.
					However, we would appreciate it greatly.
				</p>
				<p>
					If there are any concerns or inquiries about the research or the process, feel free to
					contact us.
				</p>
				<table>
					<tr>
						<td><i class="material-icons">face</i></td>
						<td>Spencer Riley</td>
						<td><i class="material-icons">face</i></td>
						<td>Vicki Kelsey</td>
					</tr>
					<tr>
						<td><i class="material-icons">public</i></td>
						<td><a target="_blank" href="http://pharaohcola13.github.io">pharaohcola13.github.io</a></td>
						<td><i class="material-icons">public</i></td>
						<td><a target="_blank" href="http://physicsgoddess1972.github.io">physicsgoddess1972.github.io</a></td>
					</tr>
					<tr>
						<td><i class="material-icons">alternate_email</i></td>
						<td>spencer.riley@student.nmt.edu</td>
						<td><i class="material-icons">alternate_email</i></td>
						<td>vicki.kelsey@student.nmt.edu</td>
					</tr>
				</table>
			</div>
			<div class="modal-footer">
				<a href="#!" class="modal-close waves-effect waves-teal btn-flat" data-dismiss="modal1">Continue</a>
			</div>
		</div>
		<div class="menubar" style="padding-right: -100%;"></div>
				<div class='content'>
                    <a id="top"></a>
                    <div class="collapsible">
                        <div class="collapsible-header">
                            <h2>Data Dashboard</h2>
                        </div>
                        <div class="panel">
                            {%app_entry%}
                        </div>
                    </div>
                </div>
		<nav class="bottom-nav" style="width: 100%;">
			<a class="bottom-nav__action" href="#top">
				<svg class="bottom-nav__icon" viewBox="0 0 24 24">
					<path d="M4 12l1.41 1.41L11 7.83V20h2V7.83l5.58 5.59L20 12l-8-8-8 8z"/>
				</svg>
				<span class="bottom-nav__label">Back to Top</span>
			</a>
			<a class="bottom-nav__action" href="https://pw-ml-dash.uc.r.appspot.com/">
				<i class="bottom-nav__icon material-icons" role="presentation" style="margin-bottom: -10px; margin-top: -18px">memory</i>
				<span class="bottom-nav__label">Machine Learning</span>
			</a>
			<a class="bottom-nav__action" href="https://pw-data-dash.uc.r.appspot.com/">
				<i class="bottom-nav__icon material-icons" role="presentation" style="margin-bottom: -10px; margin-top: -18px">insights</i>
				<span class="bottom-nav__label">Data Dashboard</span>
			</a>
		</nav>
	</main>
</div>
{%config%}
<script>
	$(document).ready(function(){
		$('.modal').modal({});
	});
</script>
<script src="https://code.getmdl.io/1.3.0/material.min.js"></script>
<script src="./assets/js/modal1.js"></script>
{%scripts%}
{%renderer%}
</body>
</html>
"""
app = dash.Dash(__name__, assets_folder='assets',
			  index_string=layout, external_scripts=['https://code.getmdl.io/1.3.0/material.min.js'])

server = app.server

df = pd.read_csv("https://raw.githubusercontent.com/physicsgoddess1972/Precipitable-Water-Model/master/data/master_data.csv")

def getIndexes(dfObj, value):
    ''' Get index positions of value in dataframe i.e. dfObj.'''
    listOfPos = list()
    # Get bool dataframe with True at positions where the given value exists
    result = dfObj.isin([value])
    # Get list of columns that contains the value
    seriesObj = result.any()
    columnNames = list(seriesObj[seriesObj == True].index)
    # Iterate over list of columns and fetch the rows indexes where value exists
    for col in columnNames:
        rows = list(result[col][result[col] == True].index)
        for row in rows:
            listOfPos.append((row, col))
    # Return a list of tuples indicating the positions of value in the dataframe
    return listOfPos

app.layout = html.Div(children=[
    html.Div(children=[
        dcc.DatePickerRange(
            id='daterng',
            min_date_allowed=dt.strptime(df.Date[0], "%m/%d/%Y"),
            max_date_allowed=dt.strptime(df.Date[len(df)-1], "%m/%d/%Y"),
            start_date=dt.strptime(df.Date[0], "%m/%d/%Y").date(),
            with_portal=True,
            end_date=dt.strptime(df.Date[len(df)-1], "%m/%d/%Y").date()
        )
    ], style={'padding-bottom': 20, 'textAlign': 'center'}),
    dcc.Tabs([
        dcc.Tab(label="Time Series", children=[
            html.Div([
                html.Label("Y axis: ",
                           style={"color": "#000",
                                  'padding-top': 10,
                                  'padding-left': 10}),
                dcc.Dropdown(id='timedata',
                             options=[{'label': i, 'value': i} for i in df.columns[4:18]],
                             value=df.columns[4],
                             style={'padding-left': 10, 'width': 250}),
                 ], style={'display': 'flex', 'margin-top': 20}),
            dcc.Tabs([
                dcc.Tab(label="Scatter Plot", children=[
                    dcc.Graph(id='scatter-time')
                ]),
                dcc.Tab(label="Heatmaps", children=[
                    dcc.Graph(id='heat-time')
                ])
            ], style={'padding-top': 20})
        ]),
        dcc.Tab(label="Analytical", children=[
            html.Div([
                html.Label("X axis: ",
                           style={"color": "#000",
                                  'padding-top': 15,
                                  'margin-right': 10}),
                dcc.Dropdown(id='analydata1',
                             options=[{'label': i, 'value': i} for i in df.columns[4:18]],
                             value=df.columns[4], style={'width': 250, 'margin-right': 50}),
                html.Label("Y axis: ",
                           style={"color": "#000",
                                  'padding-top': 15,'margin-right': 10}),
                dcc.Dropdown(id='analydata2',
                             options=[{'label': i, 'value': i} for i in df.columns[4:18]],
                             value=df.columns[5], style={'width': 250}),
                     ], style={'display': 'flex', 'margin-top': 10}),
            dcc.Graph(id='scatter-analy')

        ]),
        dcc.Tab(label="Charts", children=[
            dcc.Dropdown(id='chart-data',
                         options=[{'label': i, 'value': i} for i in ['Ground Temperature', 'Sky Temperature', 'Delta Temperature']],
                         value="Ground Temperature"),
            dcc.Graph(id='chart')
        ])
    ])
])

@app.callback(
    dash.dependencies.Output('scatter-time', 'figure'),
    [dash.dependencies.Input('timedata', 'value'),
     dash.dependencies.Input('daterng', 'start_date'),
     dash.dependencies.Input('daterng', 'end_date')]
)
def time_scatter_plot(timedata, start, end):
    start_date  = dt.strptime(start, "%Y-%m-%d").strftime('%-m/%-d/%Y')
    end_date    = dt.strptime(end, "%Y-%m-%d").strftime('%-m/%-d/%Y')

    s = getIndexes(df, start_date)[0][0]
    e = getIndexes(df, end_date)[0][0]

    data = [{
        'x': df.Date[s:e],
        'y': df[timedata][s:e],
        'mode': 'markers',
        'marker': {'color': '#0897FF'},
        'name': timedata,
    }]
    return {'data': data,
            'layout': {'xaxis': {'nticks': 5,
                                 'title': 'Date'}}
            }
@app.callback(
    dash.dependencies.Output('heat-time', 'figure'),
    [dash.dependencies.Input('timedata', 'value'),
     dash.dependencies.Input('daterng', 'start_date'),
     dash.dependencies.Input('daterng', 'end_date')]
)
def time_heat_map(timedata, start, end):

    s = getIndexes(df, dt.strptime(start, "%Y-%m-%d").strftime('%-m/%-d/%Y'))[0][0]
    e = getIndexes(df, dt.strptime(end, "%Y-%m-%d").strftime('%-m/%-d/%Y'))[0][0]

    delta = pd.to_datetime(df.Date[e]) - pd.to_datetime(df.Date[s])

    dates_in_year = [pd.to_datetime(df.Date[s]) + datetime.timedelta(i) for i in range(delta.days+1)]
    yr = unique([x.year for x in dates_in_year])
    thing = [[int(x.strftime("%U"))+(52 * list(yr).index(x.year))] for x in dates_in_year]


    data = [go.Heatmap(
                     x=list(itertools.chain(*thing)),
                     y=[i.strftime('%w') for i in dates_in_year],
                     z=df[timedata][s:e],
                     colorscale='Jet',
                     text=df.Date[s:e],
                     # hoverinfo=['text'],
                     hovertemplate='Z: %{z:.2f}'
                     )]

    return {'data': data,
            'layout': {'height': 500,
                       'xaxis': {'showline': False,
                                 'showgrid': False,
                                 'zeroline': False,
                                 'visible': False},
                       'yaxis': {'dtick': 1,
                                 'tickvals': [0,1,2,3,4,5,6,7],
                                 'ticktext': ['Sun', 'Mon', "Tues", "Wend", 'Thurs', 'Fri', 'Sat','Sun'],
                                 'autorange': True,
                                 'showline': False,
                                 'showgrid': False,
                                 'zeroline': False},
                       'title': "Time Series Heatmap"}}
@app.callback(
    dash.dependencies.Output('scatter-analy', 'figure'),
    [dash.dependencies.Input('analydata1', 'value'),
     dash.dependencies.Input('analydata2', 'value'),
     dash.dependencies.Input('daterng', 'start_date'),
     dash.dependencies.Input('daterng', 'end_date')]
)
def analy_scatter_plot(analydata1, analydata2, start, end):
    start_date  = dt.strptime(start, "%Y-%m-%d").strftime('%-m/%-d/%Y')
    end_date    = dt.strptime(end, "%Y-%m-%d").strftime('%-m/%-d/%Y')

    s = getIndexes(df, start_date)[0][0]
    e = getIndexes(df, end_date)[0][0]

    data = [{
        'x': df[analydata1][s:e],
        'y': df[analydata2][s:e],
        'mode': 'markers',
        'marker': {'color': '#0897FF'},
    }]
    return {'data': data}

# Run the Dash app
if __name__ == '__main__':
    app.server.run(host='0.0.0.0', port=8080, threaded=True, debug=True)
