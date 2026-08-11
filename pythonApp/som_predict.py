"""
som_predict.py

Pure-numpy reimplementation of:
  - kohonen::supersom's best-matching-unit (BMU) prediction, and
  - plot_ensemble_danger_detail.R's ensemble pooling + triangle/histogram plot

...driven entirely by som_models.json (produced by export_som_models.R
against your real .Rdat files), since Python can't load R's serialized
supersom objects directly.

IMPORTANT -- this needs validation before you trust it:
kohonen's exact multi-layer distance combination has a few implementation
details I can't verify without running R myself (per-layer normalization,
which dist function each layer actually used -- "sumofsquares" vs
"tanimoto" for binary layers, etc). export_som_models.R exports real
predict() results for a sample of rows specifically so this module can
check itself. Run:

    python som_predict.py --validate

after generating som_models.json. It re-runs the BMU search in Python for
every validation example and reports whether it matches R's real answer.
If anything mismatches, tell me which layer/model and I'll fix the
distance formula -- don't trust generate_prediction() until this passes.
"""
import json
import os

import numpy as np
import plotly.graph_objects as go

_MODELS_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)), "som_models.json")

DANGER_COLORS = {1: "#50B848", 2: "#FFF200", 3: "#F7941E", 4: "#ED1C24", 5: "#000000"}


class PredictionUnavailable(Exception):
    pass


def _load_models():
    if not os.path.exists(_MODELS_PATH):
        raise PredictionUnavailable(
            f"{_MODELS_PATH} not found. Run export_som_models.R against your "
            "model_list.Rdat / Normalizer.Rdat and place som_models.json here."
        )
    with open(_MODELS_PATH) as f:
        return json.load(f)


def _reshape(entry):
    """entry: {"dim": [nrow, ncol], "values": [...row-major flat...]}"""
    dim = entry["dim"]
    arr = np.array(entry["values"], dtype=float)
    if len(dim) == 2 and dim[1] > 0:
        return arr.reshape(dim[0], dim[1])
    return arr.reshape(dim[0])


def _layer_distance(newvec, codebook, dist_fct):
    """Distance from one observation to every unit's codebook row for a
    single layer. codebook: (n_units, n_features). Returns (n_units,).

    Confirmed against kohonen's actual source + docs (not guessed):
    - "sumofsquares": raw sum of squared differences, no per-feature
      normalization -- distance_weights (see find_bmu) is what calibrates
      layers of different natural scale against each other, not this.
    - "tanimoto": despite the name, this is NOT Jaccard/intersection-over-
      union. Per the kohonen docs: "the fraction of cases in which the two
      vectors disagree... basically the Hamming distance divided by n" --
      the misleading name is a known, deliberately-kept quirk for backwards
      compatibility. Values are treated as binary via a 0.5 threshold.
    """
    n_features = codebook.shape[1]
    if dist_fct == "tanimoto":
        newvec_bin = (newvec > 0.5).astype(float)
        code_bin = (codebook > 0.5).astype(float)
        disagree = (code_bin != newvec_bin.reshape(1, -1)).astype(float)
        return disagree.mean(axis=1)
    # sumofsquares (default)
    diff = codebook - newvec.reshape(1, -1)
    return np.sum(diff ** 2, axis=1)


def find_bmu(model, newdata_layers):
    """model: one entry from models_json["models"]. newdata_layers: dict of
    {layer_name: 1D np.array} -- only the layers actually available need to
    be present; this restricts to their intersection with model["whatmap"]
    and renormalizes weights among just those, matching map.kohonen's
    `weights <- user.weights * x$distance.weights[whatmap.tr]; weights <-
    weights/sum(weights)` (confirmed against the kohonen package source).
    Returns the 1-indexed winning unit (matching R's unit.classif)."""
    active_layers = [l for l in model["whatmap"] if l in newdata_layers]
    if not active_layers:
        raise PredictionUnavailable("No usable layers in newdata_layers for this model")

    layer_idx = {l: i for i, l in enumerate(model["whatmap"])}
    user_weights = model.get("user_weights") or [1.0] * len(model["whatmap"])
    distance_weights = model.get("distance_weights") or [1.0] * len(model["whatmap"])
    dist_fcts = model.get("dist_fcts") or ["sumofsquares"] * len(model["whatmap"])

    raw_weights = np.array([user_weights[layer_idx[l]] * distance_weights[layer_idx[l]] for l in active_layers])
    weights = raw_weights / raw_weights.sum()

    total_dist = None
    for i, layer in enumerate(active_layers):
        codebook = _reshape(model["codes"][layer])
        newvec = newdata_layers[layer]
        d = _layer_distance(newvec, codebook, dist_fcts[layer_idx[layer]])
        weighted = d * weights[i]
        total_dist = weighted if total_dist is None else total_dist + weighted

    winning_unit_0idx = int(np.argmin(total_dist))
    return winning_unit_0idx + 1  # 1-indexed, matches R


def _normalize(raw_numeric, normalizer, column_order):
    """raw_numeric: 1D array in the training column order. normalizer:
    models_json["normalizer"] -- confirmed (by directly reading the real
    Normalizer.Rdat's $params, bypassing its transform()/inverse_transform()
    closures which Python can't execute anyway) to be a plain dict of
    {column_name: {"min", "max", "range"}}, i.e. min-max scaling to [0,1]:
    (x - min) / range. column_order: list of column names matching
    raw_numeric's order, used to look up each value's own min/max."""
    x = np.array(raw_numeric, dtype=float)
    mins = np.array([normalizer[c]["min"] for c in column_order])
    ranges = np.array([normalizer[c]["range"] for c in column_order])
    ranges = np.where(ranges == 0, 1.0, ranges)
    return (x - mins) / ranges


# ─── Validation against R ground truth ─────────────────────────────────────

def validate(models_json=None, verbose=True):
    """Re-runs find_bmu() for every example in models_json["validation"] and
    compares against each model's own recorded unit_classif for that
    training row -- real ground truth, since a training row's unit_classif
    IS the BMU the training algorithm assigned it using this same distance
    formula (no need to run R's predict() to get this, just the training
    data kohonen already recorded). Returns True if every example matches
    for every model."""
    models_json = models_json or _load_models()
    all_ok = True
    for vi, example in enumerate(models_json["validation"]):
        expected = example["expected_winning_units"]
        inputs_per_model = example["inputs_per_model"]
        for m_idx, model in enumerate(models_json["models"]):
            if model is None:
                continue
            inputs = inputs_per_model[m_idx]
            newdata_layers = {
                layer: np.array(inputs[layer], dtype=float).reshape(-1)
                for layer in model["whatmap"] if layer in inputs
            }
            got = find_bmu(model, newdata_layers)
            want = expected[m_idx] if m_idx < len(expected) else None
            ok = (want is not None) and (got == want)
            all_ok = all_ok and ok
            if verbose and not ok:
                print(f"[val {vi} model {m_idx}] MISMATCH: python={got} R(training)={want}")
            elif verbose:
                print(f"[val {vi} model {m_idx}] OK ({got})")
    if verbose:
        print("\nALL PASSED" if all_ok else "\nSOME MISMATCHES -- do not trust generate_prediction() yet")
    return all_ok


# ─── Ensemble prediction (mirrors plot_ensemble_danger_detail.R) ──────────

def _mode(values):
    if len(values) == 0:
        return None
    vals, counts = np.unique(values, return_counts=True)
    return int(vals[np.argmax(counts)])


def _winner(votes):
    """votes: dict {rating(1-5): count}. Ties resolve to the higher rating,
    matching get_winner()'s `max(tied)` in the R original."""
    if not votes or all(v == 0 for v in votes.values()):
        return None
    max_votes = max(votes.values())
    tied = [k for k, v in votes.items() if v == max_votes]
    return max(tied)


def generate_prediction(form_values):
    """form_values: dict keyed by weather_form_app.ALL_FORM_IDS (the raw
    Dash input values). Returns (plotly Figure, info_text)."""
    models_json = _load_models()

    numeric, binary, danger_prev = _build_prediction_inputs(form_values, models_json)

    alp_votes, tl_votes, btl_votes = {1: 0, 2: 0, 3: 0, 4: 0, 5: 0}, {1: 0, 2: 0, 3: 0, 4: 0, 5: 0}, {1: 0, 2: 0, 3: 0, 4: 0, 5: 0}
    winning_nodes = []

    for model in models_json["models"]:
        if model is None:
            winning_nodes.append(None)
            continue
        newdata_layers = {"numeric": numeric, "binary": binary, "danger_prev": danger_prev}
        newdata_layers = {k: v for k, v in newdata_layers.items() if k in model["whatmap"]}
        winning_unit = find_bmu(model, newdata_layers)

        unit_classif = np.array(model["unit_classif"])
        node_indices = np.where(unit_classif == winning_unit)[0]
        winning_nodes.append(node_indices)

        danger = model["data"].get("danger")
        if danger is None:
            continue
        danger_mat = _reshape(danger)
        col_names = danger["colnames"] or []
        col_idx = {name: i for i, name in enumerate(col_names)}

        for band, votes in (("alp.used", alp_votes), ("tl.used", tl_votes), ("btl.used", btl_votes)):
            if band not in col_idx or len(node_indices) == 0:
                continue
            vals = danger_mat[node_indices, col_idx[band]]
            vals = vals[(~np.isnan(vals)) & (vals >= 1) & (vals <= 5)]
            if len(vals) > 0:
                mode = _mode(vals)
                votes[mode] += 1

    ensemble_alp, ensemble_tl, ensemble_btl = _winner(alp_votes), _winner(tl_votes), _winner(btl_votes)

    # Pool unique training-day indices across all winning nodes (fixed to
    # dedupe -- see plot_ensemble_danger_detail.R's history in this project)
    all_indices = set()
    for idx_array in winning_nodes:
        if idx_array is not None:
            all_indices.update(idx_array.tolist())
    all_indices = sorted(all_indices)
    n_total = len(all_indices)

    reference_model = next((m for m in models_json["models"] if m is not None), None)
    pooled_alp = pooled_tl = pooled_btl = np.array([])
    if reference_model is not None and "danger" in reference_model["data"] and all_indices:
        danger = reference_model["data"]["danger"]
        danger_mat = _reshape(danger)
        col_idx = {name: i for i, name in enumerate(danger["colnames"] or [])}
        idx_arr = np.array(all_indices)
        if "alp.used" in col_idx:
            pooled_alp = danger_mat[idx_arr, col_idx["alp.used"]]
        if "tl.used" in col_idx:
            pooled_tl = danger_mat[idx_arr, col_idx["tl.used"]]
        if "btl.used" in col_idx:
            pooled_btl = danger_mat[idx_arr, col_idx["btl.used"]]

    def valid(vals):
        vals = np.asarray(vals, dtype=float)
        return vals[(~np.isnan(vals)) & (vals >= 1) & (vals <= 5)]

    fig = _build_triangle_figure(
        ensemble_alp, ensemble_tl, ensemble_btl,
        valid(pooled_alp), valid(pooled_tl), valid(pooled_btl), n_total,
    )
    info = (
        "Prediction generated. The triangle shows the ensemble's predicted danger levels "
        "for each elevation band. The histograms show the distribution of observed danger "
        f"ratings from {n_total} unique historical days across all models in the ensemble."
    )
    return fig, info


def _build_prediction_inputs(values, models_json):
    """Mirrors create_prediction_data_for_ensemble() in the R app: builds
    the normalized numeric vector, binary problem-type vector, and previous-
    day danger vector for one observation."""
    dir_map = {"N": 0, "NNE": 22.5, "NE": 45, "ENE": 67.5, "E": 90, "ESE": 112.5,
               "SE": 135, "SSE": 157.5, "S": 180, "SSW": 202.5, "SW": 225, "WSW": 247.5,
               "W": 270, "WNW": 292.5, "NW": 315, "NNW": 337.5}
    dir_hi_deg = dir_map.get(values["dir_avg_hi"], 0)
    dir_mid_deg = dir_map.get(values["dir_avg_mid"], 0)

    problem_types = ["C", "DL", "DS", "G", "PS", "SS", "WL", "WdS", "WtS", "none", "spring"]

    def binary_row(used):
        row = {c: 0 for c in problem_types}
        if used in row:
            row[used] = 1
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

    # Order matches R's rawNumeric <- form_data[, 1:33] exactly
    raw_numeric = [
        float(values["p1_like"]), p1_min, p1_max,
        float(values["p2_like"]), p2_min, p2_max,
        float(values["p3_like"]), p3_min, p3_max,
        values["snow_depth"], values["swe_in"], values["swe_increment_in"],
        values["precip_increment_in"], values["precip_cumulative_in"],
        values["temp_max_low"], values["temp_min_low"],
        values["snow_depth_3day"], values["swe_increment_3day"], values["precip_increment_3day"],
        values["snow_depth_7day"], values["swe_increment_7day"], values["precip_increment_7day"],
        values["snow_depth_increment"],
        values["wind_avg_hi"], values["wind_max_hi"], dir_hi_deg, values["temp_min_hi"], values["temp_max_hi"],
        values["wind_avg_mid"], values["wind_max_mid"], dir_mid_deg, values["temp_min_mid"], values["temp_max_mid"],
    ]
    raw_numeric = [0.0 if v is None else float(v) for v in raw_numeric]

    # Order matches R's predBinary <- form_data[, 34:64] exactly (p1 has
    # 11 codes incl. spring; p2/p3 have 10 each, no spring)
    binary_vec = (
        [p1b[c] for c in problem_types]
        + [p2b[c] for c in problem_types if c != "spring"]
        + [p3b[c] for c in problem_types if c != "spring"]
    )

    # IMPORTANT: the normalizer's own column names (e.g. "temp_max_low",
    # "wind_avg_hi") and the SOM model's numeric-layer colnames (e.g.
    # "temp_max_CR", "wind_avg_SB") use DIFFERENT naming for the same 33
    # positions -- confirmed by inspecting the real files directly, they're
    # from different points in this project's naming history. The R app
    # only ever relies on positional order (form_data[, 1:33]), never name
    # matching between the two, so we do the same: use the normalizer's own
    # key order, which already matches raw_numeric's construction order
    # above (both start p1.like, p1.minSize, ... in the same sequence).
    training_col_order = list(models_json["normalizer"].keys())
    numeric = _normalize(raw_numeric, models_json["normalizer"], training_col_order)
    binary = np.array(binary_vec, dtype=float)
    danger_prev = np.array([values["alp_prev"], values["tl_prev"], values["btl_prev"]], dtype=float)
    return numeric, binary, danger_prev


def _tri_polygon(fig, x, y, color, name):
    fig.add_trace(go.Scatter(x=x + [x[0]], y=y + [y[0]], fill="toself",
                              fillcolor=color, line=dict(color="gray", width=1.5),
                              mode="lines", name=name, showlegend=False, hoverinfo="skip"))


def _hist_bars(fig, values, x0, y_baseline, max_height=0.35, width=0.15):
    if len(values) == 0:
        return
    counts = {lvl: int(np.sum(np.round(values) == lvl)) for lvl in range(1, 6)}
    max_count = max(counts.values())
    if max_count == 0:
        return
    for i, lvl in enumerate(range(1, 6)):
        c = counts[lvl]
        if c == 0:
            continue
        h = (c / max_count) * max_height
        fig.add_shape(type="rect", x0=x0 + i * width, x1=x0 + (i + 1) * width,
                      y0=y_baseline, y1=y_baseline + h,
                      fillcolor=DANGER_COLORS[lvl], line=dict(color="gray", width=0.5))


def _color_for(val):
    if val is None or val < 1 or val > 5:
        return "#787878"
    return DANGER_COLORS[int(round(val))]


def _build_triangle_figure(ensemble_alp, ensemble_tl, ensemble_btl,
                            pooled_alp, pooled_tl, pooled_btl, n_total):
    tw, th = 1.5, 1.3
    top_y = th / 2
    bottom_y = -th / 2
    band_h = th / 3

    fig = go.Figure()

    alp_bottom_y = top_y - band_h
    alp_half_w = (tw * (1 - 2 * band_h / th)) / 2
    _tri_polygon(fig, [0, alp_half_w, -alp_half_w], [top_y, alp_bottom_y, alp_bottom_y],
                 _color_for(ensemble_alp), "Alpine")

    tl_bottom_y = alp_bottom_y - band_h
    tl_half_w = (tw * (1 - band_h / th)) / 2
    _tri_polygon(fig, [-alp_half_w, alp_half_w, tl_half_w, -tl_half_w],
                 [alp_bottom_y, alp_bottom_y, tl_bottom_y, tl_bottom_y],
                 _color_for(ensemble_tl), "Treeline")

    btl_bottom_y = bottom_y
    _tri_polygon(fig, [-tl_half_w, tl_half_w, tw / 2, -tw / 2],
                 [tl_bottom_y, tl_bottom_y, btl_bottom_y, btl_bottom_y],
                 _color_for(ensemble_btl), "Below Treeline")

    hist_x0 = -1.5
    _hist_bars(fig, pooled_alp, hist_x0, alp_bottom_y)
    _hist_bars(fig, pooled_tl, hist_x0, tl_bottom_y)
    _hist_bars(fig, pooled_btl, hist_x0, btl_bottom_y)

    fig.add_annotation(x=0, y=bottom_y - 0.3, text=f"n = {n_total}", showarrow=False, font=dict(size=16))
    fig.update_layout(
        title="Ensemble Predicted Danger Rating Distribution",
        xaxis=dict(visible=False, range=[-1.8, 1.0]),
        yaxis=dict(visible=False, range=[bottom_y - 0.5, top_y + 0.2], scaleanchor="x"),
        showlegend=False, margin=dict(l=20, r=20, t=60, b=20),
    )
    return fig


if __name__ == "__main__":
    import sys
    if "--validate" in sys.argv:
        validate()
    else:
        print("Usage: python som_predict.py --validate")
