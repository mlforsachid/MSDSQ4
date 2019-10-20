import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output

import pandas as pd
import plotly.graph_objs as go


external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)



soql_url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=boroname,count(tree_id)' +\
        '&$group=boroname').replace(' ', '%20')
df_borrows = pd.read_json(soql_url)

borrows = df_borrows['boroname'].unique()

soql_url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,count(tree_id)' +\
        '&$group=spc_common').replace(' ', '%20')
df_species = pd.read_json(soql_url)

species = df_species['spc_common'].unique()
species = species[pd.notnull(species)]

df = pd.read_csv(
    'https://gist.githubusercontent.com/chriddyp/'
    'cb5392c35661370d95f300086accea51/raw/'
    '8e0768211f6b747c0db42a9ce9a0937dafcbd8b2/'
    'indicators.csv')



app.layout = html.Div([
        html.H1(children='Data 608: HW-4'),

    html.Div(children='''
        Health analysis of various trees of different species in New York city
    '''),
             
    html.Div([

        html.Div([
             html.H6(children='Select Borrow'),
            dcc.Dropdown(
                id='borrows',
                options=[{'label': i, 'value': i} for i in borrows],
                value='Bronx'
            ),
        ],
        style={'width': '48%', 'display': 'inline-block'}),

        html.Div([
            html.H6(children='Select Species'),
            dcc.Dropdown(
                id='species',
                options=[{'label': i, 'value': i} for i in species],
                value='American beech'
            ),
        ],style={'width': '48%', 'float': 'right', 'display': 'inline-block'})
    ]),

    html.Div([
    html.H6(children='Health proportions of trees'),
    dcc.Graph(id='indicator-graphic1')
    ],style={'width': '48%', 'float': 'left', 'display': 'inline-block'}),
    
    html.Div([
    html.H6(children='Stewards impact on trees health'),
    dcc.Graph(id='indicator-graphic2')
    ],style={'width': '48%', 'float': 'right', 'display': 'inline-block'}),

    
])

@app.callback(
    Output('indicator-graphic1', 'figure'),
    [Input('borrows', 'value'),
     Input('species', 'value')])
def update_graph1(borrowname, speciesname):
    #dff = df[df['Year'] == year_value]
    
    boro = borrowname
    species = speciesname
    soql_url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=health,count(tree_id)' +\
        '&$where=boroname=\'{0}\' and spc_common=\'{1}\'' +\
        '&$group=health').replace('{0}', boro).replace('{1}', species).replace(' ', '%20')


    df_health = pd.read_json(soql_url)
    df_health.head()

    return {
    'data': [go.Pie(
            labels = df_health['health'],
            values = df_health['count_tree_id'],
            hole=.3
        )]
    }

@app.callback(
    Output('indicator-graphic2', 'figure'),
    [Input('borrows', 'value'),
     Input('species', 'value')])
def update_graph2(borrowname, speciesname):
    #dff = df[df['Year'] == year_value]
    
    boro = borrowname
    species = speciesname
    soql_url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=health,steward,count(tree_id)' +\
        '&$where=boroname=\'{0}\' and spc_common=\'{1}\'' +\
        '&$group=health,steward').replace('{0}', boro).replace('{1}', species).replace(' ', '%20')

    df_steward = pd.read_json(soql_url)
    df_steward.loc[df_steward.steward != 'None', 'steward'] = 'Yes'
    df_steward = df_steward.groupby(['health', 'steward']).sum().reset_index()
    
    return {
    'data': [
            go.Bar(name='With Steward', x=df_steward[df_steward.steward == 'Yes']['health'], y=df_steward[df_steward.steward == 'Yes']['count_tree_id'], text=df_steward[df_steward.steward == 'Yes']['count_tree_id'], textposition='auto'),
            go.Bar(name='No Steward', x=df_steward[df_steward.steward == 'None']['health'], y=df_steward[df_steward.steward == 'None']['count_tree_id'], text=df_steward[df_steward.steward == 'Yes']['count_tree_id'], textposition='auto'),
            
        ]
    }

if __name__ == '__main__':
    app.run_server(debug=True)