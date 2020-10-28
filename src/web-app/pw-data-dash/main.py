# Import required libraries
import os, re
from random import randint

import flask
from flask import Flask, render_template

import base64, io

import dash
#from dash.dependencies import Input, Output
import dash_core_components as dcc
import dash_html_components as html

import chart_studio
import chart_studio.plotly as py
import plotly.graph_objects as go
import plotly.figure_factory as ff

from flask import Flask, send_from_directory

import pandas as pd
from numpy import *
import datetime
import itertools
from datetime import datetime as dt
#    <link rel='stylesheet' href='./assets/material.cyan-light_blue.min.css'>
#    <link rel='stylesheet' href='./assets/style.css'>
#    <link rel='stylesheet' href='./assets/materialize.min.css'>
#    <link rel='stylesheet' href='./assets/dash.css'>


layout="""
<!doctype html>
<!--suppress ALL -->
<html>
<head>
	<title>Precipitable Water Model</title>
	<link rel="icon" href="./assets/icon.png">
    <link rel="shortcut icon" type="image/png" href="./assets/icon.png">

    <script src="./assets/jquery.min.js"></script>
	<script src='./assets/legacy.js'></script>
	<script src='./assets/script.js'></script>
	<script src="./assets/materialize.min.js"></script>
	<link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.6.0/css/all.css">
	<link rel="stylesheet" href="https://fonts.googleapis.com/icon?family=Material+Icons" >
	<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Source+Code+Pro&display=swap">
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
			<a class="mdl-navigation__link" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/dash.html"><i class="material-icons" role="presentation">dashboard</i>Home</a>
			<a class="mdl-navigation__link" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/"><i class="material-icons" role="presentation">chrome_reader_mode</i>Documentation</a>
			<a class="mdl-navigation__link" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/contrib.html"><i class="material-icons" role="presentation">people</i>Contribute</a>
			<a class="mdl-navigation__link" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/code.html"><i class="material-icons" role="presentation">code</i>R Features</a>
			<a class="mdl-navigation__link" onclick="$('#maintainers').modal('open');"><i class="material-icons" role="presentation">face</i>The Maintainers</a>
			<hr>
			<a class="mdl-navigation__link" href="https://github.com/physicsgoddess1972/Precipitable-Water-Model"><i class="material-icons"><i class="fab fa-github big-icon"></i></i> View on Github</a>
			<a class="mdl-navigation__link" href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/archive/master.zip"><i class="material-icons" role="presentation">cloud_download</i>Download the Repo</a>
			<a class="mdl-navigation__link" href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/issues"><i class="material-icons" role="presentation">bug_report</i>Bug Report</a>
			<a class="mdl-navigation__link" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/changelog.html"><i class="material-icons" role="presentation">new_releases</i>Changelog</a>

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
						<td><a target="_blank" href="https://spencerriley.me">spencerriley.me</a></td>
						<td><i class="material-icons">public</i></td>
						<td><a target="_blank" href="http://physicsgoddess1972.github.io">physicsgoddess1972.github.io</a></td>
					</tr>
					<tr>
						<td><i class="material-icons">alternate_email</i></td>
						<td>spencer.riley@student.nmt.edu</td>
						<td><i class="material-icons">alternate_email</i></td>
						<td>vicki.kelsey@mines.sdsmt.edu</td>
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
						<td><a target="_blank" href="https://spencerriley.me">spencerriley.me</a></td>
						<td><i class="material-icons">public</i></td>
						<td><a target="_blank" href="http://physicsgoddess1972.github.io">physicsgoddess1972.github.io</a></td>
					</tr>
					<tr>
						<td><i class="material-icons">alternate_email</i></td>
						<td>spencer.riley@student.nmt.edu</td>
						<td><i class="material-icons">alternate_email</i></td>
						<td>vicki.kelsey@mines.sdsmt.edu</td>
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
			<a class="bottom-nav__action--current" href="https://pw-data-dash.uc.r.appspot.com/">
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

def parse_data(contents, filename):
	try:
		content_type, content_string = contents.split(',')
		decoded = base64.b64decode(content_string)
		df = pd.read_csv(io.StringIO(decoded.decode('utf-8')))
	except AttributeError:
		df = pd.read_csv("https://raw.githubusercontent.com/physicsgoddess1972/Precipitable-Water-Model/master/data/master_data.csv")
	return df

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

def layout():
	return html.Div(children=[
	    		html.Div(children=[
					html.Div(children=[
						dcc.Upload(
							id='upload-data',
							children=[html.Button("add", className="bottom-nav__icon material-icons",
															style={'display': 'block', 'width': '100%', 'height': '100%', 'background-color': '#FFF', 'border-color': '#DDD','border-width': '2px'})],
						),
						html.Button("clear", id='clear', className="bottom-nav__icon material-icons",
														style={'display': 'block', 'height': '50%', 'width': '100%', 'background-color': '#FFF', 'border-color': '#DDD','border-width': '2px'})],
						style={'margin-left': '15px', 'margin-right': '20em'}),
			        dcc.DatePickerRange(
			            id='daterng',
						style={'textAlign': 'right'}
	        ),
	    ], style={'padding-bottom': 20, 'display': 'flex'}),
	    dcc.Tabs([
	        dcc.Tab(label="Time Series", children=[
	            html.Div([
	                html.Label("Y axis: ",
	                           style={"color": "#000",
	                                  'padding-top': 10,
	                                  'padding-left': 10}),
	                dcc.Dropdown(id='timedata', style={'padding-left': 10, 'width': 250}),
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
	                dcc.Dropdown(id='analydata1', style={'width': 250, 'margin-right': 50}),
	                html.Label("Y axis: ",
	                           style={"color": "#000",
	                                  'padding-top': 15,'margin-right': 10}),
	                dcc.Dropdown(id='analydata2', style={'width': 250}),
	                     ], style={'display': 'flex', 'margin-top': 10}),
	            dcc.Graph(id='scatter-analy')

	        ]),
	        # dcc.Tab(label="Charts", children=[
	        #     dcc.Dropdown(id='chart-data',
	        #                  options=[{'label': i, 'value': i} for i in ['Ground Temperature', 'Sky Temperature', 'Delta Temperature']],
	        #                  value="Ground Temperature"),
	        #     dcc.Graph(id='chart')
	        # ])
	    ])
	])
app.layout = layout()

@app.callback(
    [dash.dependencies.Output('timedata', 'value'),
	 dash.dependencies.Output('timedata', 'options'),
	 dash.dependencies.Output('analydata1', 'value'),
 	 dash.dependencies.Output('analydata1', 'options'),
	 dash.dependencies.Output('analydata2', 'value'),
 	 dash.dependencies.Output('analydata2', 'options')],
	[dash.dependencies.Input('upload-data', 'contents'),
	 dash.dependencies.Input('upload-data', 'filename')])
def update_dropdown(data, fname):
	df = parse_data(data, fname)
	options=[{'label': i.replace("_", " "), 'value': i} for i in df.columns[4:18]]
	value=df.columns[4]

	return value, options, value, options, value, options,

@app.callback(
    [dash.dependencies.Output('daterng', 'min_date_allowed'),
	 dash.dependencies.Output('daterng', 'max_date_allowed'),
	 dash.dependencies.Output('daterng', 'start_date'),
	 dash.dependencies.Output('daterng', 'end_date'),
	 dash.dependencies.Output('daterng', 'with_portal')],
	[dash.dependencies.Input('upload-data', 'contents'),
	 dash.dependencies.Input('upload-data', 'filename')]
)
def update_timerng(data, fname):
	df = parse_data(data, fname)
	thing = [dt.strptime(df.Date[0], "%m/%d/%Y"),
		 dt.strptime(df.Date[len(df)-1], "%m/%d/%Y"),
		 dt.strptime(df.Date[0], "%m/%d/%Y").date(),
		 dt.strptime(df.Date[len(df)-1], "%m/%d/%Y").date(),
		 True]
	return thing[0], thing[1], thing[2], thing[3], thing[4]

@app.callback(
    dash.dependencies.Output('scatter-time', 'figure'),
    [dash.dependencies.Input('timedata', 'value'),
     dash.dependencies.Input('daterng', 'start_date'),
     dash.dependencies.Input('daterng', 'end_date'),
	 dash.dependencies.Input('upload-data', 'contents'),
	 dash.dependencies.Input('upload-data', 'filename')]
)
def time_scatter_plot(timedata, start, end, data, fname):
	df = parse_data(data, fname)
	start_date  = dt.strptime(start, "%Y-%m-%d").strftime('%-m/%-d/%Y')
	end_date    = dt.strptime(end, "%Y-%m-%d").strftime('%-m/%-d/%Y')

	s = getIndexes(df, start_date)[0][0]
	e = getIndexes(df, end_date)[0][0]

	hovertext = list()
	for yi, xi in zip(df[timedata][s:e],df.Date[s:e]):
	    hovertext.append('{}: {}<br />Date: {}'.format(timedata.replace("_", " "), yi, xi))

	data = [{
	    'x': df.Date[s:e],
	    'y': df[timedata][s:e],
	    'mode': 'markers',
	    'marker': {'color': '#0897FF'},
	     'text':hovertext,
	     'hoverinfo':'text',
	}]

	return {'data': data,
	        'layout': {'xaxis': {'nticks': 5,
	                             'tickfont': {'size': 10, 'color': 'black'},
	                             'title': "Date"},
	                   'yaxis': {'title': timedata.replace("_", " "),
	                             'tickfont': {'size': 10, 'color': 'black'}},
	                   'title': "Time Series of {}".format(timedata.replace("_", " ")),
	                 }
	        }
@app.callback(
    dash.dependencies.Output('heat-time', 'figure'),
    [dash.dependencies.Input('timedata', 'value'),
     dash.dependencies.Input('daterng', 'start_date'),
     dash.dependencies.Input('daterng', 'end_date'),
 	 dash.dependencies.Input('upload-data', 'contents'),
 	 dash.dependencies.Input('upload-data', 'filename')]
)
def time_heat_map(timedata, start, end, data, fname):
	df = parse_data(data, fname)
	s = getIndexes(df, dt.strptime(start, "%Y-%m-%d").strftime('%-m/%-d/%Y'))[0][0]
	e = getIndexes(df, dt.strptime(end, "%Y-%m-%d").strftime('%-m/%-d/%Y'))[0][0]

	delta = pd.to_datetime(df.Date[e]) - pd.to_datetime(df.Date[s])
	dates_in_year = [pd.to_datetime(df.Date[s]) + datetime.timedelta(i) for i in range(delta.days+1)]

	hovertext = list()
	for yi, xi in zip(df[timedata][s:e],df.Date[s:e]):
	    hovertext.append('{}: {}<br />Date: {}'.format(timedata.replace("_", " "), yi, xi))

	data = [go.Heatmap(
	                 x=df.Date[s:e],
	                 y=ones(len(dates_in_year)),
	                 z=df[timedata][s:e],
	                 colorscale='Jet',
	                 text=hovertext,
	                 hoverinfo='text',
	                 )]

	return {'data': data,
	        'layout': {'height': 500,
	                   'width': 700,
	                   'margin': {'l':30, 'r':0, 't':100, 'b':50},
	                   'xaxis': {'nticks': 5,
	                             'tickfont': {'size': 10, 'color': 'black'},
	                             'title': "Date",
	                             'showline': False,
	                             'showgrid': False,
	                             'zeroline': False},
	                   'yaxis': {'tickvals': False,
	                             'showline': False,
	                             'showgrid': False,
	                             'zeroline': False,
	                             'visible': False},
	                   'title': "Time Series Heatmap of {}".format(timedata.replace("_", " "))}}
@app.callback(
    dash.dependencies.Output('scatter-analy', 'figure'),
    [dash.dependencies.Input('analydata1', 'value'),
     dash.dependencies.Input('analydata2', 'value'),
     dash.dependencies.Input('daterng', 'start_date'),
     dash.dependencies.Input('daterng', 'end_date'),
  	 dash.dependencies.Input('upload-data', 'contents'),
  	 dash.dependencies.Input('upload-data', 'filename')]
)
def analy_scatter_plot(analydata1, analydata2, start, end, data, fname):
	df = parse_data(data, fname)
	start_date  = dt.strptime(start, "%Y-%m-%d").strftime('%-m/%-d/%Y')
	end_date    = dt.strptime(end, "%Y-%m-%d").strftime('%-m/%-d/%Y')

	s = getIndexes(df, start_date)[0][0]
	e = getIndexes(df, end_date)[0][0]

	hovertext = list()
	for yi, xi,zi in zip(df[analydata1][s:e],df[analydata2][s:e],df.Date[s:e]):
	    hovertext.append('{}: {}<br />{}: {}<br />Date: {}'.format(analydata1.replace("_", " "), yi, analydata2.replace("_", " "), xi, zi))

	data = [{
	    'x': df[analydata1][s:e],
	    'y': df[analydata2][s:e],
	    'mode': 'markers',
	    'marker': {'color': '#0897FF'},
	    'text':hovertext,
	    'hoverinfo':'text',
	}]
	return {'data': data,
	        'layout': {'xaxis': {'tickfont': {'size': 10, 'color': 'black'},
	                             'title': analydata1.replace("_", " ")},
	                   'yaxis': {'title': analydata2.replace("_", " "),
	                             'tickfont': {'size': 10, 'color': 'black'}},
	                    'title': "Comparison between {} and {}".format(analydata1.replace("_", " "), analydata2.replace("_", " "))}
            }
# def charts():
#     start_date  = dt.strptime(start, "%Y-%m-%d").strftime('%-m/%-d/%Y')
#     end_date    = dt.strptime(end, "%Y-%m-%d").strftime('%-m/%-d/%Y')
#
#     s = getIndexes(df, start_date)[0][0]
#     e = getIndexes(df, end_date)[0][0]
#
#     data = [{
#         'x': df[analydata1][s:e],
#         'y': df[analydata2][s:e],
#         'mode': 'markers',
#         'marker': {'color': '#0897FF'},
#     }]
# Run the Dash app
if __name__ == '__main__':
    app.server.run(host='0.0.0.0', port=8080, threaded=True, debug=True)
