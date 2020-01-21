import pandas as pd
import numpy as np
import plotly.plotly as py
import plotly.graph_objs as go

events_2010_present = pd.read_csv('../data/csv/2010-present-event.csv')

events_2010_present["home_team"] = events_2010_present.game_id.str[0:3]
events_2010_present["year"] = events_2010_present.game_id.str[3:7].astype(int)
events_2010_present["month"] = events_2010_present.game_id.str[7:9].astype(int)
events_2010_present["day"] = events_2010_present.game_id.str[9:11].astype(int)
events_2010_present["SH_flag"] = np.where(events_2010_present["SH_flag"] == "T", 1, 0)

# hits (0 = no hit; 1 = single; 2 = double; 3 = triple; 4 = home run)
events_2010_present["H"] = np.where(events_2010_present["hit_value"] > 0, 1, 0)
# base on balls (walks are event type 14)
events_2010_present["BB"] = np.where(events_2010_present["event_type"] == 14, 1, 0)
# hit by pitch (event type 16)
events_2010_present["HBP"] = np.where(events_2010_present["event_type"] == 16, 1, 0)
# at bat
events_2010_present["AB"] = 1
# sacrifice fly (denoted from SH flag: T = yes; F = no)
events_2010_present.rename(columns={'SH_flag': 'SF'}, inplace=True)

years = [2010, 2011, 2012, 2013, 2014, 2015, 2016]
c = ['hsl('+str(h)+',50%'+',50%)' for h in np.linspace(0, 360, len(years))]

data = []
for y in years:
    events_for_year = events_2010_present[events_2010_present['year'] == y]
    events_for_year_gb_batter = events_for_year.groupby('res_batter')
    data.append(
        [
            (
                player_df["H"].sum() + player_df["BB"].sum() + player_df["HBP"].sum()
            )/(
                player_df["AB"].sum() + player_df["BB"].sum() + player_df["HBP"].sum() + player_df["SF"].sum()
            )
            for player_id, player_df in events_for_year_gb_batter
        ]
    )

colors = [
    'rgba(93, 164, 214, 0.5)',
    'rgba(255, 144, 14, 0.5)',
    'rgba(44, 160, 101, 0.5)',
    'rgba(255, 65, 54, 0.5)',
    'rgba(207, 114, 255, 0.5)',
    'rgba(127, 96, 0, 0.5)',
    'rgba(66, 160, 198, 0.5)'
]

traces = []

for xd, yd, cls in zip(years, data, colors):
        traces.append(go.Box(
            y=yd,
            name=xd,
            boxpoints='all',
            jitter=0.5,
            whiskerwidth=0.2,
            fillcolor=cls,
            marker=dict(
                size=2,
            ),
            line=dict(width=1),
        ))

layout = go.Layout(
    title='On Base Percentage from 2010 to 2016',
    yaxis=dict(
        autorange=True,
        showgrid=True,
        zeroline=True,
        dtick=5,
        gridcolor='rgb(255, 255, 255)',
        gridwidth=1,
        zerolinecolor='rgb(255, 255, 255)',
        zerolinewidth=2,
    ),
    margin=dict(
        l=40,
        r=30,
        b=80,
        t=100,
    ),
    paper_bgcolor='rgb(243, 243, 243)',
    plot_bgcolor='rgb(243, 243, 243)',
    showlegend=False
)

fig = go.Figure(data=traces, layout=layout)
py.iplot(fig)

# TB = 1B + 2*2B + 3*3B + 4*HR
# SLG = TB/AB
events_2010_present["TB"] = events_2010_present["batter_dest"]
# don't know how far advance was earned, so 5 (scores and unearned) and 6 (team unearned) are 4
events_2010_present["TB"] = np.where(
    events_2010_present["batter_dest"] == 5,
    4,
    events_2010_present["batter_dest"]
)
events_2010_present["TB"] = np.where(
    events_2010_present["batter_dest"] == 6,
    4,
    events_2010_present["batter_dest"]
)
data = []
for y in years:
    events_for_year = events_2010_present[events_2010_present['year'] == y]
    events_for_year_gb_batter = events_for_year.groupby('res_batter')
    data.append(
        [
            player_df["TB"].sum()/player_df["AB"].sum()
            for player_id, player_df in events_for_year_gb_batter
        ]
    )

colors = [
    'rgba(93, 164, 214, 0.5)',
    'rgba(255, 144, 14, 0.5)',
    'rgba(44, 160, 101, 0.5)',
    'rgba(255, 65, 54, 0.5)',
    'rgba(207, 114, 255, 0.5)',
    'rgba(127, 96, 0, 0.5)',
    'rgba(66, 160, 198, 0.5)'
]

traces = []

for xd, yd, cls in zip(years, data, colors):
        traces.append(go.Box(
            y=yd,
            name=xd,
            boxpoints='all',
            jitter=0.5,
            whiskerwidth=0.2,
            fillcolor=cls,
            marker=dict(
                size=2,
            ),
            line=dict(width=1),
        ))

layout = go.Layout(
    title='Slugging average from 2010 to 2016',
    yaxis=dict(
        autorange=True,
        showgrid=True,
        zeroline=True,
        dtick=5,
        gridcolor='rgb(255, 255, 255)',
        gridwidth=1,
        zerolinecolor='rgb(255, 255, 255)',
        zerolinewidth=2,
    ),
    margin=dict(
        l=40,
        r=30,
        b=80,
        t=100,
    ),
    paper_bgcolor='rgb(243, 243, 243)',
    plot_bgcolor='rgb(243, 243, 243)',
    showlegend=False
)

fig = go.Figure(data=traces, layout=layout)
py.iplot(fig)
