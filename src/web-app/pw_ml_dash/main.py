# Import required libraries
import os, re
from random import randint

import flask
from flask import Flask, render_template

import dash
import base64, io
#from dash.dependencies import Input, Output
import dash_core_components as dcc
import dash_html_components as html
from dash_table import DataTable

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

layout = """
<!doctype html>
<!--suppress ALL -->
<html>
<head>
    <title>Precipitable Water Model</title>
    <link rel="icon" href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/blob/docs/docs/assets/img/icon.png?raw=true">
    <link rel="shortcut icon" type="image/png" href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/blob/docs/docs/assets/img/icon.png?raw=true">
    {%scripts%}
    <script type='text/javascript' src='assets/js/jquery.min.js'></script>
    <script type='text/javascript' href='assets/js/materialize.min.js'></script>
    <script type='text/javascript' href='assets/js/sidenav.js'></script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/devicons/devicon@v2.12.0/devicon.min.css">
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
            <div class="mdl-layout-spacer" style="padding-right: 50%;"></div>
            <a href="changelog.html#v2">
                <div class="chip" style="height: 67%">
                    <div style="display: flex">
                        <div style="width: 10%;">
                            <i class="material-icons" style="padding-top: 8px">new_releases</i>
                        </div>
                        <div style="width: 90%; padding-left: 20px; padding-top: 6px">
                            <b>Version 2 is available</b>
                        </div>
                    </div>
                </div>
            </a>
        </div>
    </header>
    <div class="demo-drawer mdl-layout__drawer mdl-color--blue-grey-900 mdl-color-text--blue-grey-50">
        <nav class="demo-navigation mdl-navigation mdl-color--blue-grey-900 sidebar">
            <ul class="nav flex-column" id="nav_accordion">
            <a class="mdl-navigation__link" id="dash-html" href="dash.html"><i class="material-icons" role="presentation">dashboard</i>Home</a>
            <li class="nav-item has-submenu">
                <a  class="mdl-navigation__link nav-link" href="#">Main Project<i class="material-icons" role="presentation">expand_more</i></a>
                <ul class="submenu collapse">
                    <a class="mdl-navigation__link" id="index-html" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/index.html"><i class="material-icons" role="presentation">chrome_reader_mode</i>Documentation</a>
                    <a class="mdl-navigation__link" id="contrib-html" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/contrib.html"><i class="material-icons" role="presentation">group_add</i>Contribute</a>
                    <a class="mdl-navigation__link" id="code-html" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/code.html"><i class="devicon-r-plain material-icons"></i>R Features</a>
                    <a class="mdl-navigation__link" id="deployment-html" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/deployment.html"><i class="material-icons" role="presentation">build</i>Deployment</a>
                    <a class="mdl-navigation__link" id="changelog-html" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/changelog.html"><i class="material-icons" role="presentation">new_releases</i>Changelog</a>
                </ul>
            </li>
            <li class="nav-item has-submenu">
                <a class="mdl-navigation__link nav-link" data-toggle="dropdown" href="#">Side Projects <i class="material-icons" role="presentation">expand_more</i></a>
                <ul class="submenu collapse">
                    <a class="mdl-navigation__link" id="machine_learning-html" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/machine_learning.html"><i class="material-icons">memory</i>Machine Learning</a>
                    <a class="mdl-navigation__link" id="automation-html" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/automation.html"><i class="material-icons">smart_toy</i>Automation</a>

                </ul>
            </li>
            <a class="mdl-navigation__link" id="research-html" href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/research.html"><i class="material-icons">science</i>Research</a>
            <a class="mdl-navigation__link" onclick="$('#maintainers').modal('open');"><i class="material-icons" role="presentation">face</i>The Maintainers</a>
            <hr>
            <a class="mdl-navigation__link" href="https://github.com/physicsgoddess1972/Precipitable-Water-Model"><i class="material-icons"><i class="fab fa-github big-icon"></i></i> View on Github</a>
            <a class="mdl-navigation__link" href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/archive/master.zip"><i class="material-icons" role="presentation">cloud_download</i>Download the Repo</a>
            <a class="mdl-navigation__link" href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/issues"><i class="material-icons" role="presentation">bug_report</i>Bug Report</a>
            </ul>
        </nav>
    </div>
    <main class="mdl-layout__content">
        <div id="modal-maintainers"></div>
        <div id="modal-introduction"></div>
        <div class="menubar" style="padding-right: -100%;"></div>
        <div class='content'>
            <a id="top"></a>
            <div class="collapsible">
                <div class="collapsible-header">
                    <h2>Machine Learning Dashboard</h2>
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
            <a class="bottom-nav__action--current" href="https://pw-ml-dash.uc.r.appspot.com/">
                <i class="bottom-nav__icon material-icons" role="presentation" style="margin-bottom: -10px; margin-top: -18px">memory</i>
                <span class="bottom-nav__label">Machine Learning</span>
            </a>
            <a class="bottom-nav__action" href="https://pw-data-dash.uc.r.appspot.com/">
                <i class="bottom-nav__icon material-icons" role="presentation" style="margin-bottom: -10px; margin-top: -18px">insights</i>
                <span class="bottom-nav__label">Data Dashboard</span>
            </a>
            <a class="bottom-nav__action" href="https://pw-map-dash.uc.r.appspot.com/">
                <i class="bottom-nav__icon material-icons" role="presentation" style="margin-bottom: -10px; margin-top: -18px">travel_explore</i>
                <span class="bottom-nav__label">Import Config</span>
            </a>
        </nav>
    </main>
</div>
{%config%}
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

df = pd.read_csv("https://raw.githubusercontent.com/physicsgoddess1972/Precipitable-Water-Model/pmat-socorro-nm/data/ml_data.csv")

def parse_data(contents, filename, clear):
	if clear == 0:
		try:
			content_type, content_string = contents.split(',')
			decoded = base64.b64decode(content_string)
			df = pd.read_csv(io.StringIO(decoded.decode('utf-8')))
		except AttributeError:
			df = pd.read_csv("https://raw.githubusercontent.com/physicsgoddess1972/Precipitable-Water-Model/pmat-socorro-nm/data/ml_data.csv")
	elif clear > 0:
		df = pd.read_csv("https://raw.githubusercontent.com/physicsgoddess1972/Precipitable-Water-Model/pmat-socorro-nm/data/ml_data.csv")
	return df

def analysis(randstate, setting, checkopt, trainsize, data):
    ## Shoving data and labels into an array
    X = pd.DataFrame(array(data[data.columns[1:3]]))
    Y = pd.DataFrame(array(data[data.columns[-1]]))
    ## Redefining data labels to be -1 or 1
    Y[Y == "clear sky"] = -1
    Y[Y == "overcast"] = 1

    X_train, X_test, y_train, y_test = train_test_split(X, Y, train_size=trainsize, random_state=randstate)
    svc = svm.SVC(kernel='linear', degree=5, C=2).fit(X_train.to_numpy().tolist(), y_train.to_numpy().tolist())

    # # Minimum and Maximum values for the testing data
    x_min, x_max = X[0].min() - 1, X[0].max() + 1
    y_min, y_max = 0, X[1].max() + 1

    # # Analysis coefficients
    w = svc.coef_[0]
    a = -w[0] / w[1]

    xx, yy = meshgrid(arange(x_min, x_max, 0.2),
    arange(y_min, y_max, 0.2))
    # # X-components of the support vectors and decision boundary
    db_x    = linspace(x_min, x_max)
    # # X-component of the decision boundary
    db_y    = a * db_x - svc.intercept_[0] / w[1]
    # # Y-components of the support vectors
    sv1_y   = a * db_x - (svc.intercept_[0] - 1) / w[1]
    sv2_y   = a * db_x - (svc.intercept_[0] + 1) / w[1]

    df_x1 = pd.DataFrame({'SV1': db_x})
    df_y1 = pd.DataFrame({'SV1': sv1_y})
    df_l1 = pd.DataFrame({'SV1': ["rgb(0, 0, 0)"]})

    df_x2 = pd.DataFrame({'SV2': db_x})
    df_y2 = pd.DataFrame({'SV2': sv2_y})
    df_l2 = pd.DataFrame({'SV2': ["rgb(0, 0, 0)"]})

    df_x3 = pd.DataFrame({'DB': db_x})
    df_y3 = pd.DataFrame({'DB': db_y})
    df_l3 = pd.DataFrame({'DB': ["rgb(202, 8, 205)"]})

    if setting == "Training":
        title = "Training Dataset Temperature vs TPW"
        df_x = pd.DataFrame({'Training': X_train[0]})
        df_y = pd.DataFrame({'Training': X_train[1]})
        df_l = pd.DataFrame({'Training': y_train[0]})
    elif setting == "Testing":
        title = "Testing Dataset Temperature vs TPW"
        df_x = pd.DataFrame({'Testing': X_test[0]})
        df_y = pd.DataFrame({'Testing': X_test[1]})
        df_l = pd.DataFrame({'Testing': y_test[0]})
    elif setting == "All":
        title = "Full Dataset Temperature vs TPW"
        df_x = pd.DataFrame({'All': X[0]})
        df_y = pd.DataFrame({'All': X[1]})
        df_l = pd.DataFrame({'All': Y[0]})

    return [df_x, df_y, df_l], [df_x1, df_y1, df_l1], [df_x2, df_y2, df_l2], [df_x3, df_y3, df_l3], [x_min,x_max, y_min,y_max], title

def result(randstate, trainsize, data):
    ## Shoving data and labels into an array
    X = array(data[data.columns[1:4]])
    Y = array(data[data.columns[-1]])
    ## Redefining data labels to be -1 or 1
    Y[Y == "clear sky"] = -1
    Y[Y == "overcast"] = 1
    Y = (ones(len(X)) * Y).astype('int')
    X_train, X_test, y_train, y_test = train_test_split(X, Y,
    train_size=trainsize,
    random_state=randstate)

    svc = svm.SVC(kernel='linear', degree=5, C=2).fit(X_train, y_train)

    # # Minimum and Maximum values for the testing data
    x_min, x_max = X[:, 0].min() - 1, X[:, 0].max() + 1
    y_min, y_max = 0, X[:, 1].max() + 1
    # # Analysis coefficients
    w = svc.coef_[0]
    a = -w[0] / w[1]

    xx, yy = meshgrid(arange(x_min, x_max, 0.2),
    arange(y_min, y_max, 0.2))

    # # X-components of the support vectors and decision boundary
    db_x    = linspace(x_min, x_max)
    # # X-component of the decision boundary
    db_y    = a * db_x - svc.intercept_[0] / w[1]
    # # Y-components of the support vectors
    sv1_y   = a * db_x - (svc.intercept_[0] - 1) / w[1]
    sv2_y   = a * db_x - (svc.intercept_[0] + 1) / w[1]

    y_pred = svc.predict(X_test)

    con_mat = array(confusion_matrix(y_test, y_pred))
    confusion = pd.DataFrame(con_mat, index=['clear sky', 'overcast'],
    columns=['predicted clear', 'predicted clouds'])

    acc         = pd.Series([round(svc.score(X_test, y_test) * 100, 2)], name="Accuracy")
    scores      = cross_val_score(svc, X, Y, cv=5)
    precision   = pd.Series([round(precision_score(y_test, y_pred),2)], name="Precision")
    jaccard     = pd.Series([round(jaccard_score(y_test, y_pred),2)], name="Jaccard Score")
    matt_corr   = pd.Series([round(matthews_corrcoef(y_test, y_pred),2)], name="Matthews Coefficient")
    fscore      = pd.Series([round(f1_score(y_test, y_pred, average='binary'), 2)], name="F1-Score")
    mean_acc    = pd.Series([round(scores.mean(),2)], name="Mean Accuracy")
    cv_std      = pd.Series([round(scores.std() * 2,2)], name="Standard Deviation")

    metrics = pd.concat([acc, precision, jaccard, matt_corr, fscore, mean_acc, cv_std], axis=1)
    return confusion, metrics.T

app.layout = html.Div(children=[
html.Div([
	html.Div(children=[
		html.Div(children=[
			dcc.Upload(
				id='upload-data',
				children=[html.Button("add",
					className="bottom-nav__icon material-icons",
					style={'display': 'block', 'width': '40px', 'height': '100%', 'background-color': '#FFF', 'border-color': '#DDD','border-width': '2px','margin-left': '14px', 'margin-right': '8px'})],
			),
			html.Label("Upload", style={'color': 'black', 'padding-left': '0px', 'textAlign': 'left'})
			], style={'display': 'flex'}),
		html.Div([
			html.Button("clear",
				id='clear',
				className="bottom-nav__icon material-icons",
				n_clicks=0,
				style={'display': 'block', 'height': '100%', 'width': '40px', 'background-color': '#FFF', 'border-color': '#DDD','border-width': '2px', 'margin-left': '13px','margin-right': '10px'}
			),
			html.Label("Clear", style={'color': 'black', 'textAlign': 'left'})
			], style={'display': 'flex'}),
		],
		style={'margin-right': '4em'}
		),
        html.Label("Input random state: ",
                    style={"color": "#000",
                        'vertical-align': 'middle',
                        "margin-right": 10,
                        'padding-top': 15}),
        dcc.Input(id="randstate", type="number", placeholder="1", value=1,
                    style={'width': 100,
                            'color': '#000',
                            'margin-right': 50}),

    html.Label("Input training size: ",
    style={"color": "#000",
    'vertical-align': 'middle',
    "margin-right": 10,
    'padding-top': 15}),
    dcc.Input(
    id="trainsize",
    type="number",
    placeholder="0.7", value=0.7,
    min=0.1, max=0.9, step=0.1,
    style={'width': 100,
    'color': '#000',
    'margin-right': 50}),
    ], style={'display': 'flex'}),
    dcc.Tabs([
    dcc.Tab(label="Data", children=[
    html.Div(children=[
    dcc.Dropdown(
    id='dataset',
    options=[{'label': i, 'value': i} for i in ["Training", "Testing", "All"]],
    value="Training",
    searchable=False,
    style={'width': 150,
    'margin-right': 40}),
    # dcc.Button('Decision Hyperplane', id='decision', value="DB"),
    # dcc.Button('Support Vectors', id='support', value="SV"),
    dcc.Checklist(
    id='checkopt',
    options=[
    {'label': 'Decision Hyperplane', 'value': 'DB'},
    {'label': 'Support Vectors', 'value': 'SV'}],
    labelStyle={'display':'flex',
    'color': 'rgb(0,0,0)',
    'padding-left': 10,
    'border-width': '1px',
    'border-color': '#000'})
    ],
    style={'display': 'flex', 'margin-top': 10}
    ),

    dcc.Graph(id='alldata'),
    ]),
    dcc.Tab(label="Results and Evaluation", children=[
    html.Div([
    dcc.Graph(id='conmat'),
    DataTable(id='table',
    style_as_list_view=True,
    style_table={'margin-top': 45, 'margin-left': 0},
    style_cell_conditional=[{'if': {'column_id': c},
    'textAlign': 'left'
    } for c in ['Metric']])
    ], style={'display': 'flex', 'margin-top': 10})
    ]),
    ]),
])


@app.callback(
dash.dependencies.Output('alldata', 'figure'),
[dash.dependencies.Input('upload-data', 'contents'),
 dash.dependencies.Input('upload-data', 'filename'),
 dash.dependencies.Input('clear', 'n_clicks'),
 dash.dependencies.Input('dataset','value'),
 dash.dependencies.Input('randstate', 'value'),
 dash.dependencies.Input('checkopt', 'value'),
 dash.dependencies.Input('trainsize', 'value')])
def display_graph(data, fname, clear, dataset, randstate, checkopt, trainsize):
    df = parse_data(data, fname, clear)
    df_0, df_1, df_2, df_3, axes_rng, title = analysis(randstate, dataset, checkopt, trainsize, df)
    hovertext = list()

    for xi, yi, zi, ai in zip(df['date'][df_0[0][dataset].index.tolist()],df_0[0][dataset], df_0[1][dataset], df['condition'][df_0[0][dataset].index.tolist()]):
        hovertext.append('Date: {date}<br>Temperature: {x:.2f} C <br>TPW: {y:.2f} mm <br>Condition: {a}'.format(date=xi, x=yi, y=zi,a=ai))

    data = [{
        'x': df_0[0][dataset],
        'y': df_0[1][dataset],
        'mode': 'markers',
        'marker': {'color': df_0[2][dataset].map({-1: 'rgb(0,0,255)', 1: 'rgb(255,0,0)'})},
        'text': hovertext,
        'hoverinfo': 'text',
    }]
    if checkopt == ["SV"]:
        data.append({'x': df_1[0]["SV1"],
                     'y': df_1[1]["SV1"],
                     'mode': 'lines',
                     'marker': {'color': "rgb(0,0,0)"},
                     'hoverinfo': 'name',
                     'name': "Support Vector"})
        data.append({'x': df_2[0]["SV2"],
                     'y': df_2[1]["SV2"],
                     'mode': 'lines',
                     'marker': {'color': "rgb(0,0,0)"},
                     'hoverinfo': 'name',
                     'name': "Support Vector"})
    elif checkopt == ["DB"]:
        data.append({'x': df_3[0]["DB"],
                     'y': df_3[1]["DB"],
                     'mode': 'lines',
                     'marker': {'color': "rgb(202, 8, 205)"},
                     'hoverinfo': 'name',
                     'name': "Decision Hyperplane"})
    elif checkopt == ["DB", "SV"] or checkopt == ["SV", "DB"]:
        data.append({'x': df_1[0]["SV1"],
                     'y': df_1[1]["SV1"],
                     'mode': 'lines',
                     'marker': {'color': "rgb(0,0,0)"},
                     'hoverinfo': 'name',
                     'name': "Support Vector"})
        data.append({'x': df_2[0]["SV2"],
                     'y': df_2[1]["SV2"],
                     'mode': 'lines',
                     'marker': {'color': "rgb(0,0,0)"},
                     'hoverinfo': 'name',
                     'name': "Support Vector"})
        data.append({'x': df_3[0]["DB"],
                     'y': df_3[1]["DB"],
                     'mode': 'lines',
                     'marker': {'color': "rgb(202, 8, 205)"},
                     'hoverinfo': 'name',
                     'name': "Decision Hyperplane"})


    return {'data': data,
            'layout': {'uirevision': "{}{}".format(randstate,trainsize),
                       'title': title,
                       'showlegend': False,
                       'xaxis': {'title': 'Temperature [C]',
                                 'range': axes_rng[0:2],
                                 'color': 'rgb(0,0,0)',
                                 'showgrid': False,
                                 'gridwidth': 1,
                                 'gridcolor': 'Black'},
                       'yaxis': {'title': "TPW [mm]",
                                 'color': 'rgb(0,0,0)',
                                 'range': axes_rng[2:4],
                                 'showgrid': False,
                                 'gridwidth': 1,
                                 'gridcolor': 'Black'}}}

@app.callback(
dash.dependencies.Output('conmat', 'figure'),
[dash.dependencies.Input('randstate', 'value'),
dash.dependencies.Input('trainsize', 'value'),
dash.dependencies.Input('upload-data', 'contents'),
 dash.dependencies.Input('upload-data', 'filename'),
 dash.dependencies.Input('clear', 'n_clicks')])
def display_heatmap(randstate, trainsize, data, fname, clear):
    df = parse_data(data, fname, clear)
    confusion, scores = result(randstate, trainsize, df)
    data = [go.Heatmap(
    x=['predicted clear', 'predicted overcast'],
    y=['overcast','clear sky'],
    z=confusion.iloc[::-1],
    colorscale='Jet',
    showscale=False)]
    return {'data': data,
            'layout': {'uirevision': "{}{}".format(randstate,trainsize),
            'width': 500,
            'height': 500,
            'color': 'rgb(0,0,0)',
            'title': "Confusion Matrix"}}

@app.callback(
[dash.dependencies.Output('table', 'data'),
dash.dependencies.Output('table', 'columns')],
[dash.dependencies.Input('randstate', 'value'),
dash.dependencies.Input('trainsize', 'value'),
dash.dependencies.Input('upload-data', 'contents'),
 dash.dependencies.Input('upload-data', 'filename'),
 dash.dependencies.Input('clear', 'n_clicks')])
def display_table(randstate, trainsize, data, fname, clear):
    df = parse_data(data, fname, clear)
    confusion, scores = result(randstate, trainsize, df)
    scores.reset_index(level=0, inplace=True)
    scores.columns = ["Metric", "Values"]

    column = [{"name": i, "id": i} for i in scores.columns]
    return scores.to_dict('records'), column

# Run the Dash app
if __name__ == '__main__':
    app.server.run(host='0.0.0.0', port=8080, threaded=True, debug=True)
