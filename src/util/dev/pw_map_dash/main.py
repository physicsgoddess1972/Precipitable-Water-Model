import dash
import dash_core_components as dcc
import dash_html_components as html

from utah import Utah
import pandas as pd

layout = """
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
                            <h2>Import Configuration Module</h2>
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
state_sensors = Utah.request_data()


def layout():
    return html.Div(children=[
        html.Div([
            html.Label("State: ",
                       style={"color": "#000",
                              'padding-top': 15,
                              'margin-right': 10}),
            dcc.Dropdown(id='state', style={'width': 250, 'margin-right': 50}, value=list(state_sensors.keys())[0],
                         options=[{'label': i, 'value': i} for i in list(state_sensors.keys())]),
        ], style={'display': 'flex', 'margin-top': 10}),
        dcc.Graph(id='state-map')
    ])


app.layout = layout()


@app.callback(
    dash.dependencies.Output('state-map', 'figure'),
    [dash.dependencies.Input('state', 'value')],
)
def state_maps(state):
    df = pd.read_csv('./data/sensors_{}.csv'.format(state))
    data = [dict(
        type='scattergeo',
        lon=df['3'],
        lat=df['2'],
        text=df['0'],
        mode='markers',
        locationmode='USA-states')]
    return {'data': data,
            'layout': {
                'geo': {
                    'scope': 'north america',
                    'showlakes': True,
                    'resolution': 50,
                    'showcountries': True,
                    'countrycolor': "Black",
                    'showsubunits': True,
                    'subunitcolor': "Black"
                },
                'title': 'MesoWest Station Map'
            }}

# Run the Dash app
if __name__ == '__main__':
    app.server.run(host='0.0.0.0', port=8080, threaded=True, debug=True)
