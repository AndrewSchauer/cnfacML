"""
nws_snotel.py

Data fetching and parsing for SNOTEL station 954 (AK) and the NWS Avalanche
Weather Guidance (AVG) product for Turnagain Pass. Ported from the R
functions of the same name in avyMachine_ShinyApp.R.

Translation notes (things that differ from the R source only because R and
Python differ, not because behavior changed):
  - R is 1-indexed, Python is 0-indexed. Every index into a fixed 9-slot
    array below has been shifted accordingly; the column each value lands
    in is unchanged.
  - R's `attr(df, "temps")` etc. (custom attributes on a data.frame) becomes
    a plain dict here: {"table": DataFrame, "temps": [...], ...}.
  - Quirks in the original parser are preserved deliberately, not "fixed":
    e.g. the AM/PM line (header_line+2) is parsed but never actually used
    in the resulting column labels in the R version either -- only the raw
    hour numbers are. Field pattern matching (e.g. "Min/Max Temp") assumes
    the exact field-name order/casing NWS uses for this specific product;
    if a station's product text ever orders it "Max/Min" instead, this
    parser won't match that field -- same as the R original wouldn't.
"""
import re
from datetime import datetime

import pandas as pd
import requests

SNOTEL_URL = (
    "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/"
    "daily/start_of_period/954:AK:SNTL%7Cid=%22%22%7Cname/CurrentWY,CurrentWYEnd/"
    "WTEQ::value,SNWD::value,PREC::value,TMAX::value,TMIN::value?fitToScreen=false"
)
NWS_URL = "https://forecast.weather.gov/product.php?site=afc&issuedby=afc&product=AVG&format=txt&version=1&glossary=0"


# ─── SNOTEL ────────────────────────────────────────────────────────────────

def fetch_snotel_data():
    """Fetch and parse the SNOTEL 954 daily CSV report. Returns a DataFrame
    sorted most-recent-first (row 0 = today), or None on failure."""
    try:
        resp = requests.get(SNOTEL_URL, timeout=30)
        resp.raise_for_status()
        lines = resp.text.splitlines()

        header_idx = next((i for i, l in enumerate(lines) if l.startswith("Date,")), None)
        if header_idx is None:
            raise ValueError("Could not find header row in CSV")

        csv_text = "\n".join(lines[header_idx:])
        data = pd.read_csv(pd.io.common.StringIO(csv_text))
        data.columns = [re.sub(r"\s+", "_", c.strip()) for c in data.columns]

        date_col = next((c for c in data.columns if "date" in c.lower()), None)
        if date_col is not None:
            data[date_col] = pd.to_datetime(data[date_col], format="%Y-%m-%d", errors="coerce")
            data = data.rename(columns={date_col: "Date"})

        data = data.dropna(subset=["Date"])
        data = data.sort_values("Date", ascending=False).reset_index(drop=True)
        return data
    except Exception as e:
        print(f"Error fetching SNOTEL data: {e}")
        return None


def _find_col(columns, pattern):
    for c in columns:
        if re.search(pattern, c, re.IGNORECASE):
            return c
    return None


def calculate_snotel_form_values(data, nws_mid_data=None):
    """Derive the SNOTEL-driven form fields. `data` is the DataFrame from
    fetch_snotel_data() (row 0 = most recent day). `nws_mid_data`, if given,
    is the dict returned by fetch_nws_data() for the Mid-elevation section --
    used to source 24h forecast increments instead of pure history."""
    if data is None or len(data) == 0:
        return {
            "snow_depth": 0, "swe_in": 0, "swe_increment_in": 0,
            "precip_increment_in": 0, "precip_cumulative_in": 0,
            "temp_max_low": 0, "temp_min_low": 0, "snow_depth_3day": 0,
            "swe_increment_3day": 0, "precip_increment_3day": 0,
            "snow_depth_7day": 0, "swe_increment_7day": 0,
            "precip_increment_7day": 0, "snow_depth_increment": 0,
        }

    cols = list(data.columns)
    swe_col = _find_col(cols, r"WTEQ|Snow.*Water|SWE")
    snow_depth_col = _find_col(cols, r"SNWD|Snow.*Depth")
    precip_col = _find_col(cols, r"PREC|Precipitation|Precip\.")
    temp_max_col = _find_col(cols, r"TMAX|Max.*Temp|Temperature.*Max")
    temp_min_col = _find_col(cols, r"TMIN|Min.*Temp|Temperature.*Min")

    def at(col, row):
        if col is None or row >= len(data):
            return 0.0
        v = data[col].iloc[row]
        return 0.0 if pd.isna(v) else float(v)

    current_snow_depth = at(snow_depth_col, 0)
    current_swe = at(swe_col, 0)
    precip_cumulative = at(precip_col, 0)

    # Use NWS Mid forecasted temps if available, otherwise fall back to SNOTEL
    temp_max, temp_min = 32.0, 32.0
    if nws_mid_data is not None:
        nws_min_max = [v for v in nws_mid_data.get("min_max", []) if v is not None]
        nws_temps = [v for v in (nws_mid_data.get("temps") or []) if v is not None]
        if len(nws_min_max) >= 2:
            temp_min, temp_max = nws_min_max[0], nws_min_max[1]
        elif len(nws_min_max) == 1:
            temp_min = temp_max = nws_min_max[0]
        elif nws_temps:
            temp_min, temp_max = min(nws_temps), max(nws_temps)
    else:
        temp_max = at(temp_max_col, 0) if temp_max_col else 32.0
        temp_min = at(temp_min_col, 0) if temp_min_col else 32.0
        if temp_max == 0:
            temp_max = 32.0
        if temp_min == 0:
            temp_min = 32.0

    # 24h forecast values from NWS Mid (6-Hour Snow / 6-Hour QPF, first 24h = cols 1-4 0-indexed)
    snow_24h_forecast = swe_24h_forecast = precip_24h_forecast = 0.0
    if nws_mid_data is not None:
        table = nws_mid_data.get("table")
        if table is not None and "Field" in table.columns:
            time_cols = [c for c in table.columns if c != "Field"][:4]  # first 24h = 4 periods

            snow_row = table[table["Field"] == "6 Hour Snow"]
            if len(snow_row) > 0:
                for c in time_cols:
                    try:
                        val = float(snow_row.iloc[0][c])
                        snow_24h_forecast += val
                    except (ValueError, TypeError):
                        pass

            qpf_row = table[table["Field"] == "6 Hour QPF"]
            if len(qpf_row) > 0:
                for c in time_cols:
                    try:
                        val = float(qpf_row.iloc[0][c])
                        precip_24h_forecast += val
                        swe_24h_forecast += val  # SWE approximated as QPF
                    except (ValueError, TypeError):
                        pass

    # Historical 2-day (rows 1,2) and 6-day (rows 1-6) increments
    def day_increment(col, row):
        if col is None or row + 1 >= len(data):
            return 0.0
        a, b = at(col, row), at(col, row + 1)
        return max(0.0, a - b)

    swe_2day_hist = precip_2day_hist = snow_2day_hist = 0.0
    if len(data) >= 3:
        swe_2day_hist = day_increment(swe_col, 1)
        precip_2day_hist = day_increment(precip_col, 1)
        snow_2day_hist = day_increment(snow_depth_col, 1)
    if len(data) >= 4:
        swe_2day_hist += day_increment(swe_col, 2)
        precip_2day_hist += day_increment(precip_col, 2)
        snow_2day_hist += day_increment(snow_depth_col, 2)

    swe_6day_hist, precip_6day_hist, snow_6day_hist = swe_2day_hist, precip_2day_hist, snow_2day_hist
    for day in range(3, 7):  # R's day=4..7 (1-indexed rows) -> 0-indexed rows 3..6
        if len(data) >= day + 2:
            swe_6day_hist += day_increment(swe_col, day)
            precip_6day_hist += day_increment(precip_col, day)
            snow_6day_hist += day_increment(snow_depth_col, day)

    swe_3day = swe_24h_forecast + swe_2day_hist
    precip_3day = precip_24h_forecast + precip_2day_hist
    snow_3day = snow_24h_forecast + snow_2day_hist
    swe_7day = swe_24h_forecast + swe_6day_hist
    precip_7day = precip_24h_forecast + precip_6day_hist
    snow_7day = snow_24h_forecast + snow_6day_hist

    return {
        "snow_depth": round(current_snow_depth, 2),
        "swe_in": round(current_swe, 2),
        "swe_increment_in": round(swe_24h_forecast, 2),
        "precip_increment_in": round(precip_24h_forecast, 2),
        "precip_cumulative_in": round(precip_cumulative, 2),
        "temp_max_low": round(temp_max, 1),
        "temp_min_low": round(temp_min, 1),
        "snow_depth_3day": round(snow_3day, 2),
        "swe_increment_3day": round(swe_3day, 2),
        "precip_increment_3day": round(precip_3day, 2),
        "snow_depth_7day": round(snow_7day, 2),
        "swe_increment_7day": round(swe_7day, 2),
        "precip_increment_7day": round(precip_7day, 2),
        "snow_depth_increment": round(snow_24h_forecast, 2),
    }


# ─── NWS ───────────────────────────────────────────────────────────────────

_FIELD_PATTERNS = [
    r"Cloud Cover[^%]", r"Cloud Cover \(%\)", r"Temperature", r"Min/Max Temp",
    r"Wind Dir", r"Wind \(mph\)", r"Wind Gust \(mph\)", r"Precip Prob \(%\)",
    r"Precip Type", r"6 Hour QPF", r"6 Hour Snow", r"12 Hour Snow", r"Snow Level \(kft\)",
]
_FIELD_NAMES = [
    "Cloud Cover", "Cloud Cover (%)", "Temperature", "Min/Max Temp",
    "Wind Dir", "Wind (mph)", "Wind Gust (mph)", "Precip Prob (%)",
    "Precip Type", "6 Hour QPF", "6 Hour Snow", "12 Hour Snow", "Snow Level (kft)",
]
_NON_NUMERIC_FIELDS = {"Cloud Cover", "Wind Dir", "Precip Type"}


def _parse_fixed_width_line(line, is_numeric=True):
    if line is None:
        return [None] * 9
    m = re.search(r"\s{2,}", line)
    if not m:
        return [None] * 9
    data_part = line[m.start():]
    values = data_part.split()
    if is_numeric:
        parsed = []
        for v in values:
            try:
                parsed.append(float(v))
            except ValueError:
                parsed.append(None)
        values = parsed
    if len(values) < 9:
        values = values + [None] * (9 - len(values))
    else:
        values = values[:9]
    return values


def _parse_sparse_line(line):
    """For fields like 'Min/Max Temp' and '12 Hour Snow' that only print a
    value every other column -- positions values by character column instead
    of by split() order, same as the R version's parse_sparse_line()."""
    result = [None] * 9
    if line is None:
        return result
    m = re.search(r"\s{2,}[0-9]", line)
    if not m:
        return result
    data_start = m.end() - 1  # index of the digit itself
    remaining = line[data_start:]
    values = []
    for v in remaining.split():
        try:
            values.append(float(v))
        except ValueError:
            pass
    value_positions = [mm.start() for mm in re.finditer(r"[0-9.]+", remaining)]
    col_width = 6
    for j, val in enumerate(values):
        if j < len(value_positions):
            col_num = value_positions[j] // col_width  # 0-indexed column
            if 0 <= col_num < 9:
                result[col_num] = val
    return result


def fetch_nws_data(section_name="Turnagain Pass Upper Elevations"):
    """Fetch and parse the NWS AVG product for a given section (e.g.
    'Turnagain Pass Upper Elevations' or 'Turnagain Pass Mid Elevations').
    Returns a dict with a display "table" (DataFrame) plus the parsed
    numeric series ("temps", "wind_dirs", "wind_speeds", "wind_gusts",
    "min_max", "time_labels_raw"), or None on failure."""
    try:
        resp = requests.get(NWS_URL, timeout=30)
        resp.raise_for_status()
        lines = resp.text.splitlines()

        section_start = next(
            (i for i, l in enumerate(lines) if section_name.lower() in l.lower()), None
        )
        if section_start is None:
            raise ValueError(f"Could not find {section_name} section")

        header_line = next(
            (i for i in range(section_start, len(lines)) if re.match(r"^Time \(LT\)", lines[i])),
            None,
        )
        if header_line is None:
            raise ValueError("Could not find Time (LT) header")

        data_lines = lines[header_line + 3:]
        next_section = next((i for i, l in enumerate(data_lines) if l.startswith("...")), None)
        if next_section is not None:
            data_lines = data_lines[:next_section]

        time_line = lines[header_line + 1]
        time_str = re.sub(r"^.*\(LT\)\s+", "", time_line).strip()
        time_labels_raw = time_str.split()

        time_labels = []
        for i in range(9):
            if i < len(time_labels_raw):
                time_labels.append(f"Col{i+1}_{time_labels_raw[i]}")
            else:
                time_labels.append(f"T{i+1}")

        all_data = {}
        for pattern, name in zip(_FIELD_PATTERNS, _FIELD_NAMES):
            line = next((l for l in data_lines if re.match("^" + pattern, l)), None)
            if line is not None:
                if name in ("Min/Max Temp", "12 Hour Snow"):
                    all_data[name] = _parse_sparse_line(line)
                else:
                    all_data[name] = _parse_fixed_width_line(line, name not in _NON_NUMERIC_FIELDS)
            else:
                all_data[name] = [None] * 9

        rows = []
        time_row = {"Field": "Time"}
        for i in range(9):
            time_row[time_labels[i]] = time_labels_raw[i] if i < len(time_labels_raw) else ""
        rows.append(time_row)
        for name in _FIELD_NAMES:
            row = {"Field": name}
            for i in range(9):
                v = all_data[name][i]
                row[time_labels[i]] = "" if v is None else str(v)
            rows.append(row)
        table = pd.DataFrame(rows)

        return {
            "table": table,
            "temps": all_data["Temperature"][:5],
            "wind_dirs": all_data["Wind Dir"][:5],
            "wind_speeds": all_data["Wind (mph)"][:5],
            "wind_gusts": all_data["Wind Gust (mph)"][:5],
            "min_max": all_data["Min/Max Temp"],
            "time_labels_raw": time_labels_raw,
        }
    except Exception as e:
        print(f"Error fetching NWS data: {e}")
        return None


def calculate_nws_form_values(data):
    """Derive the summary form fields (avg/max wind, dominant direction,
    min/max temp) from a fetch_nws_data() result dict."""
    if data is None:
        return {"wind_avg": 0, "wind_max": 0, "dir_avg": "N", "temp_min": 32, "temp_max": 32}

    temps = [v for v in data["temps"] if v is not None]
    wind_dirs = [v for v in data["wind_dirs"] if v is not None]
    wind_speeds = [v for v in data["wind_speeds"] if v is not None]
    wind_gusts = [v for v in data["wind_gusts"] if v is not None]
    min_max = [v for v in data["min_max"] if v is not None]

    wind_avg = sum(wind_speeds) / len(wind_speeds) if wind_speeds else 0.0
    wind_max = max(wind_gusts) if wind_gusts else 0.0

    if wind_dirs:
        counts = {}
        for d in wind_dirs:
            counts[d] = counts.get(d, 0) + 1
        dir_string = max(counts.items(), key=lambda kv: kv[1])[0]
    else:
        dir_string = "N"

    if len(min_max) >= 2:
        temp_min, temp_max = min_max[0], min_max[1]
    elif len(min_max) == 1:
        temp_min = temp_max = min_max[0]
    else:
        temp_min = min(temps) if temps else 32.0
        temp_max = max(temps) if temps else 32.0

    return {
        "wind_avg": round(wind_avg, 1),
        "wind_max": round(wind_max, 1),
        "dir_avg": dir_string,
        "temp_min": round(temp_min, 1),
        "temp_max": round(temp_max, 1),
    }


def display_table(nws_result):
    """Formats a fetch_nws_data() result's table for display: swaps generic
    column names for the raw time labels and drops the 'Time' row, matching
    the R app's nws_upper_table/nws_mid_table renderers."""
    if nws_result is None:
        return pd.DataFrame({"Message": ["No data loaded yet"]})
    table = nws_result["table"].copy()
    time_labels_raw = nws_result["time_labels_raw"]
    new_names = ["Field"]
    for i, c in enumerate(table.columns[1:]):
        new_names.append(time_labels_raw[i] if i < len(time_labels_raw) else c)
    table.columns = new_names
    table = table[table["Field"] != "Time"]
    return table
