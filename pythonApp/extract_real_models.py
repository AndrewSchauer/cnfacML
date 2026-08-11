"""
extract_real_models.py

One-off script: reads the real model_list_20260126.Rdat and Normalizer.Rdat
(downloaded from AndrewSchauer/cnfacML on GitHub) directly in Python via the
`rdata` package -- no R installation needed -- and writes som_models.json
in the exact format som_predict.py expects.

Run once: python extract_real_models.py
"""
import json
import warnings

import numpy as np
import rdata

warnings.filterwarnings("ignore")

MODEL_PATH = "model_list_20260126.Rdat"
NORMALIZER_PATH = "Normalizer.Rdat"
OUT_PATH = "som_models.json"


def to_list(arr):
    return np.asarray(arr).tolist()


def export_model(m):
    data_layers = list(m["data"].keys())          # real layer order in this model
    codes_layers = list(m["codes"].keys())
    whatmap_idx = np.asarray(m["whatmap"]).astype(int)  # 1-indexed positions into data_layers
    whatmap_names = [data_layers[i - 1] for i in whatmap_idx]

    codes_export = {}
    for layer in codes_layers:
        arr = np.asarray(m["codes"][layer])
        codes_export[layer] = {"dim": list(arr.shape), "values": to_list(arr.reshape(-1))}

    data_export = {}
    for layer in data_layers:
        arr = np.asarray(m["data"][layer])
        colnames = None
        # xarray DataArrays carry column names as a coordinate; try to recover them
        raw = m["data"][layer]
        if hasattr(raw, "coords"):
            for coord_name in raw.coords:
                vals = raw.coords[coord_name].values
                if vals.dtype.kind in ("U", "S", "O") and len(vals) == arr.shape[-1]:
                    colnames = [str(v) for v in vals]
                    break
        data_export[layer] = {"dim": list(arr.shape), "colnames": colnames, "values": to_list(arr.reshape(-1))}

    return {
        "layer_names": data_layers,
        "codes": codes_export,
        "grid": {
            "xdim": int(np.asarray(m["grid"]["xdim"]).item()),
            "ydim": int(np.asarray(m["grid"]["ydim"]).item()),
            "topo": str(np.asarray(m["grid"]["topo"]).item()),
        },
        "unit_classif": [int(v) for v in np.asarray(m["unit.classif"])],
        "data": data_export,
        "whatmap": whatmap_names,
        "user_weights": to_list(m["user.weights"]),
        "distance_weights": to_list(m["distance.weights"]),
        "dist_fcts": [str(v) for v in np.asarray(m["dist.fcts"])],
    }


def export_normalizer():
    full = rdata.parser.parse_file(NORMALIZER_PATH)
    params_obj = full.object.value[0].value[0]  # normalizer$params, skips the transform()/inverse_transform() closures
    wrapped = rdata.parser._parser.RData(versions=full.versions, extra=full.extra, object=params_obj)
    params = rdata.conversion.convert(wrapped)
    return {
        str(col): {"min": float(np.asarray(v["min"]).item()),
                    "max": float(np.asarray(v["max"]).item()),
                    "range": float(np.asarray(v["range"]).item())}
        for col, v in params.items()
    }


def build_validation_examples(models_export, n_val=15, seed=42):
    """Self-consistency check: for a sample of real training rows, feed that
    row's own data (across ALL layers the model was trained on) back through
    find_bmu() and see if we recover the row's own recorded unit_classif.
    This doesn't require running R's predict() -- during training, a row's
    unit_classif IS the BMU the training algorithm assigned it using this
    exact distance formula, so it's real ground truth we already have."""
    rng = np.random.default_rng(seed)
    n_train = len(models_export[0]["unit_classif"])
    row_idxs = rng.choice(n_train, size=min(n_val, n_train), replace=False)

    examples = []
    for row_idx in row_idxs:
        example = {"row_index": int(row_idx), "inputs": {}, "expected_winning_units": []}
        for m in models_export:
            layer_inputs = {}
            for layer in m["whatmap"]:
                d = m["data"][layer]
                arr = np.array(d["values"]).reshape(d["dim"])
                layer_inputs[layer] = arr[row_idx].tolist()
            example["expected_winning_units"].append(m["unit_classif"][row_idx])
        # inputs are the same across models only if layers/order match; store per-model to be safe
        example["inputs_per_model"] = [
            {layer: np.array(m["data"][layer]["values"]).reshape(m["data"][layer]["dim"])[row_idx].tolist()
             for layer in m["whatmap"]}
            for m in models_export
        ]
        examples.append(example)
    return examples


print(f"Loading {MODEL_PATH} ...")
parsed = rdata.parser.parse_file(MODEL_PATH)
converted = rdata.conversion.convert(parsed)
model_list = converted["model_list"]
print(f"Found {len(model_list)} models")

print("Exporting model internals ...")
models_export = [export_model(m) for m in model_list]

print(f"Loading {NORMALIZER_PATH} ...")
normalizer_export = export_normalizer()
print(f"Normalizer covers {len(normalizer_export)} columns")

print("Building self-consistency validation examples ...")
validation = build_validation_examples(models_export)

output = {
    "n_models": len(models_export),
    "models": models_export,
    "normalizer": normalizer_export,
    "validation": validation,
}

with open(OUT_PATH, "w") as f:
    json.dump(output, f)

import os
print(f"Wrote {OUT_PATH} ({os.path.getsize(OUT_PATH) / 1e6:.1f} MB)")
