"""
weather_form_app.py

Python/Dash port of avyMachine_ShinyApp.R. Run locally with:
    python weather_form_app.py
Deploy to Render by pointing it at this file; it reads $PORT and binds
0.0.0.0 automatically the same way the original app checked $RENDER (see
bottom of file).

Scope note: everything here is a direct port of the R app EXCEPT the
Ensemble Prediction tab's model internals, which come from som_predict.py
(a from-scratch numpy reimplementation of the R app's kohonen-based
prediction, since Python can't load the R models' .Rdat files directly --
see export_som_models.R for how to produce the JSON those functions expect).
"""
import io
from datetime import date

import dash
from dash import dcc, html, Input, Output, State, ALL, ctx
import dash_bootstrap_components as dbc
import pandas as pd
import plotly.graph_objects as go

from nws_snotel import (
    fetch_snotel_data, calculate_snotel_form_values,
    fetch_nws_data, calculate_nws_form_values, display_table,
)

try:
    from som_predict import generate_prediction, PredictionUnavailable
except Exception as _e:  # som_predict.py missing model data, etc.
    generate_prediction = None
    PredictionUnavailable = Exception

app = dash.Dash(__name__, external_stylesheets=[dbc.themes.BOOTSTRAP])
app.title = "Combined Weather Data Form - SNOTEL & NWS"
server = app.server


# ─── Field definitions (mirrors the R UI's fluidRow/column blocks) ────────

PROBLEM_TYPES = [
    ("none", "none"), ("C", "Cornice"), ("DL", "Dry Loose"), ("DS", "Deep Slab"),
    ("G", "Glide"), ("PS", "Persistent Slab"), ("SS", "Storm Slab"),
    ("WL", "Wet Loose"), ("WdS", "Wind Slab"), ("WtS", "Wet Slab"),
]
PROBLEM_TYPES_P1 = PROBLEM_TYPES + [("spring", "Spring")]  # p1 only, matches R exactly
LIKELIHOOD_CHOICES = [("1", "Unlikely"), ("2", "Possible"), ("3", "Likely"),
                       ("4", "Very Likely"), ("5", "Almost Certain")]
WIND_DIRS = ["N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
             "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"]
DIR_DEGREES = {"N": 0, "NNE": 22.5, "NE": 45, "ENE": 67.5, "E": 90, "ESE": 112.5,
               "SE": 135, "SSE": 157.5, "S": 180, "SSW": 202.5, "SW": 225, "WSW": 247.5,
               "W": 270, "WNW": 292.5, "NW": 315, "NNW": 337.5}
BINARY_PROBLEM_CODES = ["C", "DL", "DS", "G", "PS", "SS", "WL", "WdS", "WtS", "none", "spring"]


def problem_column(n, options):
    """One avalanche-problem input group (type/likelihood/minSize/maxSize)."""
    return dbc.Col([
        html.Label(f"Problem {n} Type:"),
        dcc.Dropdown(id=f"p{n}_used", options=[{"label": l, "value": v} for v, l in options],
                     value="none", clearable=False),
        html.Label(f"Problem {n} Likelihood:", style={"marginTop": "8px"}),
        dcc.Dropdown(id=f"p{n}_like", options=[{"label": l, "value": v} for v, l in LIKELIHOOD_CHOICES],
                     value="1", clearable=False),
        html.Label(f"Problem {n} Min D-Size:", style={"marginTop": "8px"}),
        dcc.Input(id=f"p{n}_minSize", type="number", value=0, min=0, max=5, step=0.5, style={"width": "100%"}),
        html.Label(f"Problem {n} Max D-Size:", style={"marginTop": "8px"}),
        dcc.Input(id=f"p{n}_maxSize", type="number", value=0, min=0, max=5, step=0.5, style={"width": "100%"}),
    ], width=4)


def numeric_field(field_id, label, value=0, step=0.1):
    return html.Div([
        html.Label(label),
        dcc.Input(id=field_id, type="number", value=value, step=step, style={"width": "100%"}),
    ], style={"marginBottom": "10px"})


# ─── Layout ─────────────────────────────────────────────────────────────────

form_tab = html.Div([
    html.H3("All Form Fields"),

    html.H4("Avalanche Problems"),
    dbc.Row([
        problem_column(1, PROBLEM_TYPES_P1),
        problem_column(2, PROBLEM_TYPES),
        problem_column(3, PROBLEM_TYPES),
    ]),
    html.Hr(),

    html.H4("Previous Day's Danger Ratings"),
    html.P("Enter yesterday's danger ratings for each elevation band (1-5 scale):"),
    dbc.Row([
        dbc.Col(numeric_field("alp_prev", "Alpine - Previous Day:", 0, 1), width=4),
        dbc.Col(numeric_field("tl_prev", "Treeline - Previous Day:", 0, 1), width=4),
        dbc.Col(numeric_field("btl_prev", "Below Treeline - Previous Day:", 0, 1), width=4),
    ]),
    html.Hr(),

    html.H4("SNOTEL Station 954 (Alaska)"),
    dbc.Row([
        dbc.Col([
            numeric_field("snow_depth", "Cumulative Seasonal Snow Depth (in):"),
            numeric_field("swe_in", "Cumulative Seasonal SWE (in):"),
            numeric_field("swe_increment_in", "24-hour Incremental SWE (in):"),
            numeric_field("precip_increment_in", "24-h Incremental Precip (in):"),
            numeric_field("precip_cumulative_in", "Seasonal Cumulative Precip (in):"),
        ], width=4),
        dbc.Col([
            numeric_field("temp_max_low", "Max Daily Temp at Center Ridge (\u00b0F):", 32),
            numeric_field("temp_min_low", "Min Daily Temp at Center Ridge (\u00b0F):", 32),
            numeric_field("snow_depth_3day", "3-day Snow Total (in):"),
            numeric_field("swe_increment_3day", "3-day SWE Total (in):"),
            numeric_field("precip_increment_3day", "3-day Precip Total (in):"),
        ], width=4),
        dbc.Col([
            numeric_field("snow_depth_7day", "7-day Snow Total (in):"),
            numeric_field("swe_increment_7day", "7-day SWE Total (in):"),
            numeric_field("precip_increment_7day", "7-day Precip Total (in):"),
            numeric_field("snow_depth_increment", "24-h Snow Total (in):"),
        ], width=4),
    ]),
    html.Hr(),

    html.H4("NWS Turnagain Pass - Upper Elevations (above 3000 ft)"),
    dbc.Row([
        dbc.Col(numeric_field("wind_avg_hi", "Daily Average Wind Speed (mph):"), width=4),
        dbc.Col(numeric_field("wind_max_hi", "Daily Max Wind Gust (mph):"), width=4),
        dbc.Col([
            html.Label("Average Wind Direction:"),
            dcc.Dropdown(id="dir_avg_hi", options=[{"label": d, "value": d} for d in WIND_DIRS],
                         value="N", clearable=False),
        ], width=4),
    ]),
    dbc.Row([
        dbc.Col(numeric_field("temp_min_hi", "Daily Min Temperature (\u00b0F):", 32), width=6),
        dbc.Col(numeric_field("temp_max_hi", "Daily Max Temperature (\u00b0F):", 32), width=6),
    ]),
    html.Hr(),

    html.H4("NWS Turnagain Pass - Mid Elevations (1500 to 3000 ft)"),
    dbc.Row([
        dbc.Col(numeric_field("wind_avg_mid", "Daily Average Wind Speed (mph):"), width=4),
        dbc.Col(numeric_field("wind_max_mid", "Daily Max Wind Gust (mph):"), width=4),
        dbc.Col([
            html.Label("Average Wind Direction:"),
            dcc.Dropdown(id="dir_avg_mid", options=[{"label": d, "value": d} for d in WIND_DIRS],
                         value="N", clearable=False),
        ], width=4),
    ]),
    dbc.Row([
        dbc.Col(numeric_field("temp_min_mid", "Daily Min Temperature (\u00b0F):", 32), width=6),
        dbc.Col(numeric_field("temp_max_mid", "Daily Max Temperature (\u00b0F):", 32), width=6),
    ]),
])

snotel_tab = html.Div([
    html.H4("Raw Data (Last 7 Days)"),
    html.Div(id="snotel_data_table"),
    html.Hr(),
    html.H4("30-Day Trends"),
    dbc.Row([
        dbc.Col(dcc.Graph(id="snow_depth_plot"), width=6),
        dbc.Col(dcc.Graph(id="swe_plot"), width=6),
    ]),
    dbc.Row([
        dbc.Col(dcc.Graph(id="precip_plot"), width=6),
        dbc.Col(dcc.Graph(id="temp_plot"), width=6),
    ]),
])

nws_tab = html.Div([
    html.H4("Turnagain Pass Upper Elevations (above 3000 ft)"),
    html.P("(Form fields use first 5 columns - 24-hour period)"),
    html.Div(id="nws_upper_table", style={"overflowX": "auto"}),
    html.Hr(),
    html.H4("Turnagain Pass Mid Elevations (1500 to 3000 ft)"),
    html.P("(Form fields use first 5 columns - 24-hour period)"),
    html.Div(id="nws_mid_table", style={"overflowX": "auto"}),
])

prediction_tab = html.Div([
    html.H3("Ensemble Danger Rating Prediction"),
    html.P("Click the button below to generate a prediction based on the form data."),
    html.P("Note: models load from a local file the first time you click "
           "(see export_som_models.R / som_predict.py)."),
    dbc.Button("Generate Prediction", id="generate_prediction", color="primary", size="lg"),
    html.Br(), html.Br(),
    dcc.Loading(dcc.Graph(id="prediction_plot", style={"height": "700px"})),
    html.Br(),
    html.Div(id="prediction_info"),
])

app.layout = dbc.Container([
    dcc.Store(id="snotel_store"),
    dcc.Store(id="nws_upper_store"),
    dcc.Store(id="nws_mid_store"),
    dcc.Download(id="download_data"),

    html.H2("Combined Weather Data Form - SNOTEL & NWS", style={"marginTop": "16px"}),

    dbc.Row([
        dbc.Col([
            html.H4("Data Fetch Controls"),
            dbc.Button("Fetch All Data", id="fetch_all", color="primary",
                       style={"width": "100%", "marginBottom": "10px"}),
            dbc.Button("Fetch SNOTEL Only", id="fetch_snotel", color="info",
                       style={"width": "100%", "marginBottom": "10px"}),
            dbc.Button("Fetch NWS Only", id="fetch_nws", color="info",
                       style={"width": "100%", "marginBottom": "10px"}),
            dbc.Button("Reset Form", id="reset_form", color="warning",
                       style={"width": "100%", "marginBottom": "20px"}),
            html.Hr(),
            html.H4("Data Status:"),
            html.Div(id="data_status"),
            html.Hr(),
            dbc.Button("Download All as CSV", id="download_btn", style={"width": "100%"}),
            dcc.Loading(html.Div(id="fetch_status_spinner"), style={"marginTop": "10px"}),
        ], width=3),

        dbc.Col([
            dbc.Tabs([
                dbc.Tab(form_tab, label="Form"),
                dbc.Tab(snotel_tab, label="SNOTEL Data"),
                dbc.Tab(nws_tab, label="NWS Tables"),
                dbc.Tab(prediction_tab, label="Ensemble Prediction"),
            ]),
        ], width=9),
    ]),
], fluid=True)


# ─── Helpers ────────────────────────────────────────────────────────────────

# The full set of updatable form-field ids, used by fetch/reset callbacks.
SNOTEL_FIELD_IDS = [
    "snow_depth", "swe_in", "swe_increment_in", "precip_increment_in", "precip_cumulative_in",
    "temp_max_low", "temp_min_low", "snow_depth_3day", "swe_increment_3day",
    "precip_increment_3day", "snow_depth_7day", "swe_increment_7day",
    "precip_increment_7day", "snow_depth_increment",
]
NWS_HI_FIELD_IDS = ["wind_avg_hi", "wind_max_hi", "dir_avg_hi", "temp_min_hi", "temp_max_hi"]
NWS_MID_FIELD_IDS = ["wind_avg_mid", "wind_max_mid", "dir_avg_mid", "temp_min_mid", "temp_max_mid"]


def _snotel_values_to_outputs(values):
    return [values[k] for k in [
        "snow_depth", "swe_in", "swe_increment_in", "precip_increment_in", "precip_cumulative_in",
        "temp_max_low", "temp_min_low", "snow_depth_3day", "swe_increment_3day",
        "precip_increment_3day", "snow_depth_7day", "swe_increment_7day",
        "precip_increment_7day", "snow_depth_increment",
    ]]


def _nws_values_to_outputs(values):
    return [values["wind_avg"], values["wind_max"], values["dir_avg"],
            values["temp_min"], values["temp_max"]]


def _trend_figure(dates, values, title, ylabel, color):
    fig = go.Figure()
    fig.add_trace(go.Scatter(x=dates, y=values, mode="lines", line=dict(color=color, width=2)))
    fig.update_layout(title=title, xaxis_title="Date", yaxis_title=ylabel,
                       margin=dict(l=50, r=20, t=40, b=40), height=300)
    return fig


def _empty_figure(title):
    fig = go.Figure()
    fig.update_layout(title=title, height=300, margin=dict(l=50, r=20, t=40, b=40))
    return fig


# ─── Callbacks: fetching ────────────────────────────────────────────────────

@app.callback(
    Output("snotel_store", "data"),
    Output("nws_upper_store", "data"),
    Output("nws_mid_store", "data"),
    Output("fetch_status_spinner", "children"),
    *[Output(fid, "value") for fid in SNOTEL_FIELD_IDS],
    *[Output(fid, "value") for fid in NWS_HI_FIELD_IDS],
    *[Output(fid, "value") for fid in NWS_MID_FIELD_IDS],
    Input("fetch_all", "n_clicks"),
    Input("fetch_snotel", "n_clicks"),
    Input("fetch_nws", "n_clicks"),
    State("snotel_store", "data"),
    State("nws_upper_store", "data"),
    State("nws_mid_store", "data"),
    *[State(fid, "value") for fid in SNOTEL_FIELD_IDS],
    *[State(fid, "value") for fid in NWS_HI_FIELD_IDS],
    *[State(fid, "value") for fid in NWS_MID_FIELD_IDS],
    prevent_initial_call=True,
)
def fetch_data(n_all, n_snotel, n_nws, snotel_cur, nws_upper_cur, nws_mid_cur, *field_state):
    trigger = ctx.triggered_id
    n_snotel_fields = len(SNOTEL_FIELD_IDS)
    n_nws_hi = len(NWS_HI_FIELD_IDS)
    cur_snotel_vals = list(field_state[:n_snotel_fields])
    cur_hi_vals = list(field_state[n_snotel_fields:n_snotel_fields + n_nws_hi])
    cur_mid_vals = list(field_state[n_snotel_fields + n_nws_hi:])

    snotel, nws_upper, nws_mid = snotel_cur, nws_upper_cur, nws_mid_cur

    if trigger in ("fetch_all", "fetch_snotel"):
        nws_mid = fetch_nws_data("Turnagain Pass Mid Elevations")
        snotel = fetch_snotel_data()
        if snotel is not None:
            values = calculate_snotel_form_values(snotel, nws_mid)
            cur_snotel_vals = _snotel_values_to_outputs(values)
        if nws_mid is not None:
            values = calculate_nws_form_values(nws_mid)
            cur_mid_vals = _nws_values_to_outputs(values)
        # serialize for dcc.Store (DataFrame -> dict of records; dates -> iso strings)
        snotel_json = snotel.assign(Date=snotel["Date"].dt.strftime("%Y-%m-%d")).to_dict("records") if snotel is not None else None
    else:
        snotel_json = snotel_cur

    if trigger in ("fetch_all", "fetch_nws"):
        nws_upper = fetch_nws_data("Turnagain Pass Upper Elevations")
        if nws_upper is not None:
            values = calculate_nws_form_values(nws_upper)
            cur_hi_vals = _nws_values_to_outputs(values)
        if trigger == "fetch_nws":
            nws_mid = fetch_nws_data("Turnagain Pass Mid Elevations")
            if nws_mid is not None:
                values = calculate_nws_form_values(nws_mid)
                cur_mid_vals = _nws_values_to_outputs(values)

    nws_upper_json = _nws_result_to_json(nws_upper) if trigger in ("fetch_all", "fetch_nws") else nws_upper_cur
    nws_mid_json = _nws_result_to_json(nws_mid) if trigger in ("fetch_all", "fetch_snotel", "fetch_nws") else nws_mid_cur

    return (snotel_json, nws_upper_json, nws_mid_json, "",
            *cur_snotel_vals, *cur_hi_vals, *cur_mid_vals)


def _nws_result_to_json(result):
    if result is None:
        return None
    return {
        "table": result["table"].to_dict("records"),
        "table_columns": list(result["table"].columns),
        "temps": result["temps"], "wind_dirs": result["wind_dirs"],
        "wind_speeds": result["wind_speeds"], "wind_gusts": result["wind_gusts"],
        "min_max": result["min_max"], "time_labels_raw": result["time_labels_raw"],
    }


def _nws_json_to_result(data):
    if data is None:
        return None
    table = pd.DataFrame(data["table"])[data["table_columns"]]
    return {"table": table, "temps": data["temps"], "wind_dirs": data["wind_dirs"],
            "wind_speeds": data["wind_speeds"], "wind_gusts": data["wind_gusts"],
            "min_max": data["min_max"], "time_labels_raw": data["time_labels_raw"]}


@app.callback(
    *[Output(fid, "value", allow_duplicate=True) for fid in [
        "p1_used", "p1_like", "p1_minSize", "p1_maxSize",
        "p2_used", "p2_like", "p2_minSize", "p2_maxSize",
        "p3_used", "p3_like", "p3_minSize", "p3_maxSize",
    ]],
    *[Output(fid, "value", allow_duplicate=True) for fid in SNOTEL_FIELD_IDS],
    *[Output(fid, "value", allow_duplicate=True) for fid in NWS_HI_FIELD_IDS],
    *[Output(fid, "value", allow_duplicate=True) for fid in NWS_MID_FIELD_IDS],
    Input("reset_form", "n_clicks"),
    prevent_initial_call=True,
)
def reset_form(n_clicks):
    avy_defaults = ["none", "1", 0, 0] * 3
    snotel_defaults = [0, 0, 0, 0, 0, 32, 32, 0, 0, 0, 0, 0, 0, 0]
    nws_defaults = [0, 0, "N", 32, 32] * 2
    return (*avy_defaults, *snotel_defaults, *nws_defaults)


# ─── Callbacks: display (status, tables, plots) ────────────────────────────

@app.callback(
    Output("data_status", "children"),
    Input("snotel_store", "data"), Input("nws_upper_store", "data"), Input("nws_mid_store", "data"),
)
def update_status(snotel, nws_upper, nws_mid):
    status = []
    if snotel is not None:
        status.append("SNOTEL loaded")
    if nws_upper is not None:
        status.append("NWS Upper loaded")
    if nws_mid is not None:
        status.append("NWS Mid loaded")
    return " | ".join(status) if status else "No data loaded. Click 'Fetch All Data' to begin."


@app.callback(Output("snotel_data_table", "children"), Input("snotel_store", "data"))
def update_snotel_table(data):
    if data is None:
        return html.P("No data loaded yet")
    df = pd.DataFrame(data).head(7)
    return dbc.Table.from_dataframe(df, striped=True, bordered=True, hover=True, size="sm")


@app.callback(Output("nws_upper_table", "children"), Input("nws_upper_store", "data"))
def update_nws_upper_table(data):
    result = _nws_json_to_result(data)
    df = display_table(result)
    return dbc.Table.from_dataframe(df, striped=True, bordered=True, hover=True, size="sm")


@app.callback(Output("nws_mid_table", "children"), Input("nws_mid_store", "data"))
def update_nws_mid_table(data):
    result = _nws_json_to_result(data)
    df = display_table(result)
    return dbc.Table.from_dataframe(df, striped=True, bordered=True, hover=True, size="sm")


@app.callback(
    Output("snow_depth_plot", "figure"), Output("swe_plot", "figure"),
    Output("precip_plot", "figure"), Output("temp_plot", "figure"),
    Input("snotel_store", "data"),
)
def update_snotel_plots(data):
    if data is None:
        empties = [_empty_figure(t) for t in
                   ("Snow Depth - Last 30 Days", "Snow Water Equivalent - Last 30 Days",
                    "Precipitation - Last 30 Days", "Temperature - Last 30 Days")]
        return tuple(empties)

    df = pd.DataFrame(data).head(30)
    cols = list(df.columns)

    def find(pattern):
        import re
        return next((c for c in cols if re.search(pattern, c, re.IGNORECASE)), None)

    snow_col = find(r"SNWD|Snow.*Depth")
    swe_col = find(r"WTEQ|Snow.*Water|SWE")
    precip_col = find(r"PREC|Precipitation|Precip\.")
    tmax_col = find(r"TMAX|Max.*Temp|Temperature.*Max")
    tmin_col = find(r"TMIN|Min.*Temp|Temperature.*Min")

    snow_fig = (_trend_figure(df["Date"], df[snow_col], "Snow Depth - Last 30 Days", "Snow Depth (inches)", "blue")
                if snow_col else _empty_figure("Snow Depth - Last 30 Days"))
    swe_fig = (_trend_figure(df["Date"], df[swe_col], "Snow Water Equivalent - Last 30 Days", "SWE (inches)", "darkblue")
               if swe_col else _empty_figure("Snow Water Equivalent - Last 30 Days"))
    precip_fig = (_trend_figure(df["Date"], df[precip_col], "Precipitation - Last 30 Days", "Cumulative Precipitation (inches)", "green")
                  if precip_col else _empty_figure("Precipitation - Last 30 Days"))

    if tmax_col and tmin_col:
        temp_fig = go.Figure()
        temp_fig.add_trace(go.Scatter(x=df["Date"], y=df[tmax_col], mode="lines", name="Max Temp", line=dict(color="red", width=2)))
        temp_fig.add_trace(go.Scatter(x=df["Date"], y=df[tmin_col], mode="lines", name="Min Temp", line=dict(color="blue", width=2)))
        temp_fig.update_layout(title="Temperature - Last 30 Days", xaxis_title="Date", yaxis_title="Temperature (\u00b0F)",
                                margin=dict(l=50, r=20, t=40, b=40), height=300)
    else:
        temp_fig = _empty_figure("Temperature - Last 30 Days")

    return snow_fig, swe_fig, precip_fig, temp_fig


# ─── Callback: CSV download ─────────────────────────────────────────────────

ALL_FORM_IDS = (
    ["p1_used", "p2_used", "p3_used", "p1_like", "p1_minSize", "p1_maxSize",
     "p2_like", "p2_minSize", "p2_maxSize", "p3_like", "p3_minSize", "p3_maxSize",
     "alp_prev", "tl_prev", "btl_prev"]
    + SNOTEL_FIELD_IDS
    + ["wind_avg_hi", "wind_max_hi", "dir_avg_hi", "temp_min_hi", "temp_max_hi",
       "wind_avg_mid", "wind_max_mid", "dir_avg_mid", "temp_min_mid", "temp_max_mid"]
)


def build_form_csv_row(values):
    """values: dict keyed by ALL_FORM_IDS. Returns a single-row DataFrame
    matching avyMachine_ShinyApp.R's downloadHandler column order exactly."""
    dir_hi_deg = DIR_DEGREES.get(values["dir_avg_hi"], 0)
    dir_mid_deg = DIR_DEGREES.get(values["dir_avg_mid"], 0)

    def binary_row(used_value):
        row = {code: 0 for code in BINARY_PROBLEM_CODES}
        if used_value in row:
            row[used_value] = 1
        return row

    p1b, p2b, p3b = binary_row(values["p1_used"]), binary_row(values["p2_used"]), binary_row(values["p3_used"])

    def fix_minmax(mn, mx):
        mn, mx = float(mn or 0), float(mx or 0)
        if mn == 0 and mx > 0:
            mn = mx
        if mx == 0 and mn > 0:
            mx = mn
        return mn, mx

    p1_min, p1_max = fix_minmax(values["p1_minSize"], values["p1_maxSize"])
    p2_min, p2_max = fix_minmax(values["p2_minSize"], values["p2_maxSize"])
    p3_min, p3_max = fix_minmax(values["p3_minSize"], values["p3_maxSize"])

    row = {
        "Date": date.today().strftime("%m/%d/%Y"),
        "alp.used": None, "tl.used": None, "btl.used": None,  # not collected in form
        "alp.prev": values["alp_prev"], "tl.prev": values["tl_prev"], "btl.prev": values["btl_prev"],
        "p1.used": values["p1_used"], "p2.used": values["p2_used"], "p3.used": values["p3_used"],
        "p1.like": float(values["p1_like"]), "p1.minSize": p1_min, "p1.maxSize": p1_max,
        "p2.like": float(values["p2_like"]), "p2.minSize": p2_min, "p2.maxSize": p2_max,
        "p3.like": float(values["p3_like"]), "p3.minSize": p3_min, "p3.maxSize": p3_max,
        "snow_depth_in": values["snow_depth"], "swe_in": values["swe_in"],
        "swe_increment_in": values["swe_increment_in"], "precip_increment_in": values["precip_increment_in"],
        "precip_cumulative_in": values["precip_cumulative_in"],
        "temp_max_low": values["temp_max_low"], "temp_min_low": values["temp_min_low"],
        "snow_depth_3day": values["snow_depth_3day"], "swe_increment_3day": values["swe_increment_3day"],
        "precip_increment_3day": values["precip_increment_3day"],
        "snow_depth_7day": values["snow_depth_7day"], "swe_increment_7day": values["swe_increment_7day"],
        "precip_increment_7day": values["precip_increment_7day"], "snow_depth_increment": values["snow_depth_increment"],
        "wind_avg_hi": values["wind_avg_hi"], "wind_max_hi": values["wind_max_hi"], "dir_avg_hi": dir_hi_deg,
        "temp_min_hi": values["temp_min_hi"], "temp_max_hi": values["temp_max_hi"],
        "wind_avg_mid": values["wind_avg_mid"], "wind_max_mid": values["wind_max_mid"], "dir_avg_mid": dir_mid_deg,
        "temp_min_mid": values["temp_min_mid"], "temp_max_mid": values["temp_max_mid"],
    }
    for p, b in (("p1", p1b), ("p2", p2b), ("p3", p3b)):
        for code in BINARY_PROBLEM_CODES:
            if p == "p2" and code == "spring":
                continue  # p2/p3 never had a "spring" option in the R UI
            if p == "p3" and code == "spring":
                continue
            row[f"{p}.{code}"] = b[code]

    return pd.DataFrame([row])


@app.callback(
    Output("download_data", "data"),
    Input("download_btn", "n_clicks"),
    [State(fid, "value") for fid in ALL_FORM_IDS],
    prevent_initial_call=True,
)
def download_csv(n_clicks, *field_values):
    values = dict(zip(ALL_FORM_IDS, field_values))
    df = build_form_csv_row(values)
    filename = f"combined_weather_form_{date.today().isoformat()}.csv"
    return dcc.send_data_frame(df.to_csv, filename, index=False)


# ─── Callback: Ensemble Prediction ──────────────────────────────────────────

@app.callback(
    Output("prediction_plot", "figure"),
    Output("prediction_info", "children"),
    Input("generate_prediction", "n_clicks"),
    [State(fid, "value") for fid in ALL_FORM_IDS],
    prevent_initial_call=True,
)
def run_prediction(n_clicks, *field_values):
    if generate_prediction is None:
        return _empty_figure("Prediction unavailable"), (
            "som_predict.py couldn't load its model file. Run export_som_models.R "
            "against your model_list.Rdat / Normalizer.Rdat and place the resulting "
            "som_models.json next to this app, then restart."
        )
    values = dict(zip(ALL_FORM_IDS, field_values))
    try:
        fig, info = generate_prediction(values)
        return fig, info
    except PredictionUnavailable as e:
        return _empty_figure("Prediction unavailable"), f"Prediction error: {e}"


if __name__ == "__main__":
    import os
    host = "0.0.0.0" if os.environ.get("RENDER") else "127.0.0.1"
    port = int(os.environ.get("PORT", 8060))
    app.run(debug=False, host=host, port=port)
