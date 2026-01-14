"""
########################################################
SISAL_monv1 flat-CSV query cookbook (Python / pandas)
- Loads all CSVs from one folder
- Builds common "queries" using merges (no SQL)
########################################################
"""

# 0) Packages
# pip install pandas numpy matplotlib
import os
import glob
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


# ========================================================
# 1) CSV loader
# Important: put ONLY the SISAL_monv1 CSV tables in this folder.
# ========================================================

folder_path = r"E:/Google Drive/flat_csv_db_v8.0"   # <-- change

csv_files = glob.glob(os.path.join(folder_path, "*.csv"))
if len(csv_files) == 0:
    raise FileNotFoundError("No .csv files found in folder_path. Check the path and contents.")

# Read + store by file name (without .csv) into a dict called `tables`
tables = {}
for file in csv_files:
    name = os.path.splitext(os.path.basename(file))[0]
    print(f"Reading: {name}")
    tables[name] = pd.read_csv(file, low_memory=False)

# Optional: also expose as variables (like R GlobalEnv). Use with care.
globals().update(tables)


# ========================================================
# 2) Sanity checks: required tables present?
# ========================================================

required_tables = [
    "site", "notes", "reference",
    "site_link_precip", "site_link_reference", "entity_link_reference",
    "precip_site", "precip_entity", "precip_sample",
    "cave_entity",
    "drip_entity", "drip_iso_sample", "drip_rate_sample", "mod_carb_sample"
]

missing = sorted(set(required_tables) - set(tables.keys()))
if missing:
    raise KeyError(f"Missing required table(s): {', '.join(missing)}")


# ========================================================
# 3) Helpers
# ========================================================

def clean_empty_to_na(df: pd.DataFrame) -> pd.DataFrame:
    """Trim string columns; convert empty strings to NaN."""
    df = df.copy()
    obj_cols = df.select_dtypes(include=["object"]).columns
    for c in obj_cols:
        df[c] = df[c].astype("string").str.strip()
        df[c] = df[c].replace("", pd.NA)
    return df

def collapse_unique(values, sep=" ; "):
    """Collapse unique non-null values into a single string; return NA if none."""
    ser = pd.Series(values).dropna().astype(str)
    if ser.empty:
        return pd.NA
    return sep.join(pd.unique(ser))

def make_dt(yyyy, mm=None, dd=None, hhmm=None, tz="UTC",
            default_mm=6, default_dd=15, default_hhmm=1200) -> pd.Series:
    """
    SAFE datetime maker (vectorized).
    - If year is missing -> NaT
    - Missing month/day/hhmm are filled with defaults
    """
    y = pd.to_numeric(pd.Series(yyyy), errors="coerce")
    n = len(y)

    if mm is None:
        m = pd.Series([np.nan] * n)
    else:
        m = pd.to_numeric(pd.Series(mm), errors="coerce")

    if dd is None:
        d = pd.Series([np.nan] * n)
    else:
        d = pd.to_numeric(pd.Series(dd), errors="coerce")

    if hhmm is None:
        hm = pd.Series([np.nan] * n)
    else:
        hm = pd.to_numeric(pd.Series(hhmm), errors="coerce")

    m2 = m.fillna(default_mm).astype(int)
    d2 = d.fillna(default_dd).astype(int)
    hm2 = hm.fillna(default_hhmm).astype(int)

    hh = (hm2 // 100).astype(int)
    mi = (hm2 % 100).astype(int)

    # build ISO string
    iso = (
        y.astype("Int64").astype(str).str.zfill(4) + "-" +
        m2.astype(str).str.zfill(2) + "-" +
        d2.astype(str).str.zfill(2) + " " +
        hh.astype(str).str.zfill(2) + ":" +
        mi.astype(str).str.zfill(2) + ":00"
    )

    out = pd.to_datetime(iso, errors="coerce", utc=True)
    # If you want naive timestamps instead of UTC-aware:
    # out = out.dt.tz_convert(None)
    return out

def classify_freq(unit, time):
    unit = pd.Series(unit).astype("string").str.lower()
    time = pd.to_numeric(pd.Series(time), errors="coerce")

    out = pd.Series(["other/unknown"] * len(unit), index=unit.index, dtype="string")

    out[(unit == "hours")   & (time == 1)] = "hourly"
    out[(unit == "minutes") & (time == 60)] = "hourly"

    out[(unit == "days")    & (time == 1)] = "daily"
    out[(unit == "hours")   & (time == 24)] = "daily"

    out[(unit == "months")  & (time == 1)] = "monthly"
    out[(unit == "days")    & (time.between(28, 31, inclusive="both"))] = "monthly"
    return out


# ========================================================
# 4) Clean tables (convert "" -> NA in char cols)
# ========================================================

site                  = clean_empty_to_na(site)
notes                 = clean_empty_to_na(notes)
reference             = clean_empty_to_na(reference)
site_link_precip      = clean_empty_to_na(site_link_precip)
site_link_reference   = clean_empty_to_na(site_link_reference)
entity_link_reference = clean_empty_to_na(entity_link_reference)

precip_site    = clean_empty_to_na(precip_site)
precip_entity  = clean_empty_to_na(precip_entity)
precip_sample  = clean_empty_to_na(precip_sample)

cave_entity    = clean_empty_to_na(cave_entity)
drip_entity    = clean_empty_to_na(drip_entity)
drip_iso_sample  = clean_empty_to_na(drip_iso_sample)
drip_rate_sample = clean_empty_to_na(drip_rate_sample)
mod_carb_sample  = clean_empty_to_na(mod_carb_sample)


# ========================================================
# 5) SITE SUMMARY: how many entities per site
# ========================================================

cave_entity_counts = (
    cave_entity.groupby("site_id", dropna=False)["cave_entity_id"]
    .nunique()
    .reset_index(name="cave_entity_count")
)

drip_counts = (
    drip_entity.groupby("site_id", dropna=False)["drip_entity_id"]
    .nunique()
    .reset_index(name="drip_entity_count")
)

precip_counts = (
    site_link_precip.groupby("site_id", dropna=False)["precip_entity_id"]
    .nunique()
    .reset_index(name="precip_entity_count")
)

site_summary = (
    site.merge(cave_entity_counts, on="site_id", how="left")
        .merge(drip_counts, on="site_id", how="left")
        .merge(precip_counts, on="site_id", how="left")
)

site_summary["cave_entity_count"]   = site_summary["cave_entity_count"].fillna(0).astype(int)
site_summary["drip_entity_count"]   = site_summary["drip_entity_count"].fillna(0).astype(int)
site_summary["precip_entity_count"] = site_summary["precip_entity_count"].fillna(0).astype(int)

# If you also want total entity count:
site_summary["entity_count"] = (
    site_summary["cave_entity_count"] +
    site_summary["drip_entity_count"] +
    site_summary["precip_entity_count"]
)

site_summary = site_summary.sort_values("site_id").reset_index(drop=True)


# ========================================================
# 6) “Do we have entries?” checks for required measurement fields
# ========================================================

drip_iso_presence_global = pd.DataFrame([{
    "n_rows": len(drip_iso_sample),
    "n_d18O": drip_iso_sample["drip_iso_d18O_measurement"].notna().sum(),
    "n_d2H":  drip_iso_sample["drip_iso_d2H_measurement"].notna().sum(),
    "n_both": (drip_iso_sample["drip_iso_d18O_measurement"].notna() &
              drip_iso_sample["drip_iso_d2H_measurement"].notna()).sum()
}])

mod_carb_presence_global = pd.DataFrame([{
    "n_rows": len(mod_carb_sample),
    "n_d18O": mod_carb_sample["mod_carb_d18O_measurement"].notna().sum(),
    "n_d13C": mod_carb_sample["mod_carb_d13C_measurement"].notna().sum(),
    "n_both": (mod_carb_sample["mod_carb_d18O_measurement"].notna() &
              mod_carb_sample["mod_carb_d13C_measurement"].notna()).sum()
}])

precip_presence_global = pd.DataFrame([{
    "n_rows": len(precip_sample),
    "n_amount": precip_sample["precip_amount"].notna().sum(),
    "n_d18O":   precip_sample["precip_d18O_measurement"].notna().sum(),
    "n_d2H":    precip_sample["precip_d2H_measurement"].notna().sum(),
}])


# ========================================================
# 7) Per-site flags: which sites have which data?
# ========================================================

# Drip iso flags
tmp_iso = drip_entity[["site_id", "drip_entity_id"]].merge(drip_iso_sample, on="drip_entity_id", how="inner")
drip_iso_site_flags = (
    tmp_iso.groupby("site_id", dropna=False)
    .agg(
        has_drip_iso_d18O=("drip_iso_d18O_measurement", lambda s: s.notna().any()),
        has_drip_iso_d2H =("drip_iso_d2H_measurement",  lambda s: s.notna().any()),
        has_drip_iso_any =("drip_iso_d18O_measurement", lambda s: s.notna().any())  # will overwrite below
    )
    .reset_index()
)
# correct "any" as d18O OR d2H
drip_iso_site_flags["has_drip_iso_any"] = (
    tmp_iso.groupby("site_id")["drip_iso_d18O_measurement"].apply(lambda s: s.notna().any()).values |
    tmp_iso.groupby("site_id")["drip_iso_d2H_measurement"].apply(lambda s: s.notna().any()).values
)

# Drip rate flags
tmp_rate = drip_entity[["site_id", "drip_entity_id"]].merge(drip_rate_sample, on="drip_entity_id", how="inner")
drip_rate_site_flags = (
    tmp_rate.groupby("site_id", dropna=False)
    .agg(has_drip_rate=("drip_rate_measurement", lambda s: s.notna().any()))
    .reset_index()
)

# Mod carb flags
tmp_mc = drip_entity[["site_id", "drip_entity_id"]].merge(mod_carb_sample, on="drip_entity_id", how="inner")
mod_carb_site_flags = (
    tmp_mc.groupby("site_id", dropna=False)
    .agg(
        has_mod_carb_d18O=("mod_carb_d18O_measurement", lambda s: s.notna().any()),
        has_mod_carb_d13C=("mod_carb_d13C_measurement", lambda s: s.notna().any()),
        has_mod_carb_any =("mod_carb_d18O_measurement", lambda s: s.notna().any())  # will overwrite below
    )
    .reset_index()
)
mod_carb_site_flags["has_mod_carb_any"] = (
    tmp_mc.groupby("site_id")["mod_carb_d18O_measurement"].apply(lambda s: s.notna().any()).values |
    tmp_mc.groupby("site_id")["mod_carb_d13C_measurement"].apply(lambda s: s.notna().any()).values
)

# Precip flags (site_link_precip joins precip_sample on precip_entity_id)
tmp_p = site_link_precip.merge(precip_sample, on="precip_entity_id", how="inner")
precip_site_flags = (
    tmp_p.groupby("site_id", dropna=False)
    .agg(
        has_precip_amount=("precip_amount", lambda s: s.notna().any()),
        has_precip_d18O  =("precip_d18O_measurement", lambda s: s.notna().any()),
        has_precip_d2H   =("precip_d2H_measurement", lambda s: s.notna().any()),
        has_precip_anyiso=("precip_d18O_measurement", lambda s: s.notna().any())  # overwrite below
    )
    .reset_index()
)
precip_site_flags["has_precip_anyiso"] = (
    tmp_p.groupby("site_id")["precip_d18O_measurement"].apply(lambda s: s.notna().any()).values |
    tmp_p.groupby("site_id")["precip_d2H_measurement"].apply(lambda s: s.notna().any()).values
)

# Counts across sites
site_data_counts = (
    site[["site_id"]]
    .merge(drip_iso_site_flags, on="site_id", how="left")
    .merge(drip_rate_site_flags, on="site_id", how="left")
    .merge(mod_carb_site_flags, on="site_id", how="left")
    .merge(precip_site_flags, on="site_id", how="left")
)

site_data_counts_summary = pd.DataFrame([{
    "n_sites_total": site["site_id"].nunique(),
    "n_sites_precip_iso": site_data_counts["has_precip_anyiso"].fillna(False).sum(),
    "n_sites_drip_iso":   site_data_counts["has_drip_iso_any"].fillna(False).sum(),
    "n_sites_drip_rate":  site_data_counts["has_drip_rate"].fillna(False).sum(),
    "n_sites_mod_carb":   site_data_counts["has_mod_carb_any"].fillna(False).sum(),
}])


# ========================================================
# 8) “Map tables”: all sites with each data type
# ========================================================

sites_drip_iso_map = (
    site.merge(drip_iso_site_flags.query("has_drip_iso_any == True")[["site_id"]], on="site_id", how="inner")
        [["site_id", "site_name", "latitude", "longitude", "elevation"]]
)

sites_drip_rate_map = (
    site.merge(drip_rate_site_flags.query("has_drip_rate == True")[["site_id"]], on="site_id", how="inner")
        [["site_id", "site_name", "latitude", "longitude", "elevation"]]
)

sites_mod_carb_map = (
    site.merge(mod_carb_site_flags.query("has_mod_carb_any == True")[["site_id"]], on="site_id", how="inner")
        [["site_id", "site_name", "latitude", "longitude", "elevation"]]
)

site_precip_map = (
    site.merge(site_link_precip, on="site_id", how="inner")
        .merge(precip_site, on="precip_site_id", how="left")
        .merge(precip_entity, on="precip_entity_id", how="left")
        [[
            "site_id", "site_name", "latitude", "longitude", "elevation",
            "precip_site_id", "precip_site_name", "precip_latitude", "precip_longitude", "precip_elevation",
            "precip_entity_id", "precip_entity_name", "precip_method"
        ]]
        .drop_duplicates()
)


# ========================================================
# 8.1) Actually “Map tables”: global + 3 zoomed maps
# - Drip Iso: hollow BLUE triangle
# - Drip Rate: hollow ORANGE square
# - Mod Carb: smaller FILLED red dot
# - Precip: BLACK cross
# - Legends on ALL plots
# ========================================================

# Combine (deduplicate within each layer)
combined_sites = pd.concat([
    sites_drip_iso_map.drop_duplicates("site_id").assign(data_type="Drip Iso"),
    sites_drip_rate_map.drop_duplicates("site_id").assign(data_type="Drip Rate"),
    sites_mod_carb_map.drop_duplicates("site_id").assign(data_type="Mod Carb"),
    site_precip_map.drop_duplicates("site_id").assign(data_type="Precip"),
], ignore_index=True)

# Split for layered plotting
combined_hollow  = combined_sites[combined_sites["data_type"].isin(["Drip Iso", "Drip Rate"])].copy()
combined_modcarb = combined_sites[combined_sites["data_type"].eq("Mod Carb")].copy()
combined_precip  = combined_sites[combined_sites["data_type"].eq("Precip")].copy()

# Aesthetics
type_colors = {
    "Drip Iso":  "blue",
    "Drip Rate": "orange",
    "Mod Carb":  "red",
    "Precip":    "black",
}
# Matplotlib marker codes:
# triangle up: '^', square: 's', filled circle: 'o', cross: 'x'
type_markers = {
    "Drip Iso":  "^",
    "Drip Rate": "s",
    "Mod Carb":  "o",
    "Precip":    "x",
}

marker_lw = 0.5
size_default = 22   # points^2 (scatter uses area)
size_modcarb = 16

def plot_world_sites(xlim=(-180, 180), ylim=(-60, 85), title=""):
    fig, ax = plt.subplots(figsize=(12, 6))

    # quick background: world coastlines from built-in dataset if available
    # Fallback: no basemap; just plot points.
    ax.set_xlim(xlim)
    ax.set_ylim(ylim)

    # Hollow layers
    for dtype in ["Drip Iso", "Drip Rate"]:
        df = combined_hollow[combined_hollow["data_type"].eq(dtype)]
        ax.scatter(
            df["longitude"], df["latitude"],
            s=size_default,
            marker=type_markers[dtype],
            facecolors="none",                # hollow
            edgecolors=type_colors[dtype],
            linewidths=marker_lw,
            label=dtype,
            alpha=0.9
        )

    # Mod Carb: filled smaller dot
    df = combined_modcarb
    ax.scatter(
        df["longitude"], df["latitude"],
        s=size_modcarb,
        marker=type_markers["Mod Carb"],
        c=type_colors["Mod Carb"],
        edgecolors=type_colors["Mod Carb"],
        linewidths=0,
        label="Mod Carb",
        alpha=0.9
    )

    # Precip: black cross
    df = combined_precip
    ax.scatter(
        df["longitude"], df["latitude"],
        s=size_default,
        marker=type_markers["Precip"],
        c=type_colors["Precip"],
        linewidths=marker_lw,
        label="Precip",
        alpha=0.9
    )

    ax.set_title(title)
    ax.set_xlabel("Longitude [deg]")
    ax.set_ylabel("Latitude [deg]")
    ax.grid(True, alpha=0.2)
    ax.legend(loc="best", frameon=True)
    return fig, ax

# Global
plot_world_sites(
    xlim=(-180, 180), ylim=(-60, 85),
    title="Site locations with different data types (flat CSV)\nHollow: Drip Iso / Drip Rate; Filled: Mod Carb; Cross: Precip"
)

# North America
plot_world_sites(
    xlim=(-140, -50), ylim=(0, 60),
    title="North America"
)

# Europe
plot_world_sites(
    xlim=(-10, 30), ylim=(30, 70),
    title="Europe"
)

# East Asia
plot_world_sites(
    xlim=(95, 130), ylim=(15, 45),
    title="East Asia"
)

plt.show()


# ========================================================
# 9) References + notes (site-level and entity-level)
# ========================================================

site_citations = (
    site_link_reference.merge(reference, on="ref_id", how="left")
    .groupby("site_id", dropna=False)
    .agg(
        site_citations=("citation", collapse_unique),
        site_DOI=("publication_DOI", collapse_unique),
    )
    .reset_index()
)

drip_entity_citations = (
    entity_link_reference[entity_link_reference["drip_entity_id"].notna()]
    .merge(reference, on="ref_id", how="left")
    .groupby("drip_entity_id", dropna=False)
    .agg(
        drip_citations=("citation", collapse_unique),
        drip_DOI=("publication_DOI", collapse_unique),
    )
    .reset_index()
)

precip_entity_citations = (
    entity_link_reference[entity_link_reference["precip_entity_id"].notna()]
    .merge(reference, on="ref_id", how="left")
    .groupby("precip_entity_id", dropna=False)
    .agg(
        precip_citations=("citation", collapse_unique),
        precip_DOI=("publication_DOI", collapse_unique),
    )
    .reset_index()
)

site_notes = (
    notes.groupby("site_id", dropna=False)
    .agg(notes=("notes", collapse_unique))
    .reset_index()
)


# ========================================================
# 10) Modern carbonate records (all)
# ========================================================

mod_carb_records = (
    drip_entity[["site_id", "drip_entity_id", "drip_entity_name", "entity_id"]]
    .merge(mod_carb_sample, on="drip_entity_id", how="inner")
    .merge(site, on="site_id", how="left")
    .merge(site_notes, on="site_id", how="left")
    .merge(site_citations, on="site_id", how="left")
    .merge(drip_entity_citations, on="drip_entity_id", how="left")
)

# Interpret year-only as full-year coverage for mod_carb
mod_carb_records["start_dt"] = make_dt(
    mod_carb_records["mod_carb_start_yyyy"],
    mod_carb_records.get("mod_carb_start_mm"),
    mod_carb_records.get("mod_carb_start_dd"),
    default_mm=1, default_dd=1, default_hhmm=0
)
mod_carb_records["end_dt"] = make_dt(
    mod_carb_records["mod_carb_end_yyyy"],
    mod_carb_records.get("mod_carb_end_mm"),
    mod_carb_records.get("mod_carb_end_dd"),
    default_mm=12, default_dd=31, default_hhmm=2359
)
mod_carb_records["end_dt"] = mod_carb_records["end_dt"].fillna(mod_carb_records["start_dt"])

mod_carb_records["freq_class"] = classify_freq(
    mod_carb_records["mod_carb_accumulation_unit"],
    mod_carb_records["mod_carb_accumulation_time"]
)

mod_carb_records = mod_carb_records.sort_values(["site_id", "drip_entity_id", "start_dt"]).reset_index(drop=True)


# ========================================================
# 11) Filter records with SISAL entities (site_id < 1000)
# ========================================================

sisal_sites = site[site["site_id"] < 1000].copy()


# ========================================================
# 12) >= 1 year of BOTH drip_rate and drip_iso (same drip_entity_id)
# ========================================================

iso_cov = drip_entity[["site_id", "drip_entity_id", "drip_entity_name"]].merge(
    drip_iso_sample, on="drip_entity_id", how="inner"
)

iso_cov["start_dt"] = make_dt(
    iso_cov["drip_iso_start_yyyy"], iso_cov.get("drip_iso_start_mm"), iso_cov.get("drip_iso_start_dd"), iso_cov.get("drip_iso_start_hhmm")
)
iso_cov["end_dt"] = make_dt(
    iso_cov["drip_iso_end_yyyy"], iso_cov.get("drip_iso_end_mm"), iso_cov.get("drip_iso_end_dd"), iso_cov.get("drip_iso_end_hhmm")
)
iso_cov["end_dt"] = iso_cov["end_dt"].fillna(iso_cov["start_dt"])

iso_cov = iso_cov.dropna(subset=["start_dt", "end_dt"])

drip_iso_coverage = (
    iso_cov.groupby(["site_id", "drip_entity_id", "drip_entity_name"])
    .agg(
        iso_start=("start_dt", "min"),
        iso_end=("end_dt", "max"),
    )
    .reset_index()
)
drip_iso_coverage["iso_days"] = (drip_iso_coverage["iso_end"] - drip_iso_coverage["iso_start"]).dt.days + 1


rate_cov = drip_entity[["site_id", "drip_entity_id", "drip_entity_name"]].merge(
    drip_rate_sample, on="drip_entity_id", how="inner"
)

rate_cov["start_dt"] = make_dt(
    rate_cov["drip_rate_start_yyyy"], rate_cov.get("drip_rate_start_mm"), rate_cov.get("drip_rate_start_dd"), rate_cov.get("drip_rate_start_hhmm")
)
rate_cov["end_dt"] = make_dt(
    rate_cov["drip_rate_end_yyyy"], rate_cov.get("drip_rate_end_mm"), rate_cov.get("drip_rate_end_dd"), rate_cov.get("drip_rate_end_hhmm")
)
rate_cov["end_dt"] = rate_cov["end_dt"].fillna(rate_cov["start_dt"])

rate_cov = rate_cov.dropna(subset=["start_dt", "end_dt"])

drip_rate_coverage = (
    rate_cov.groupby(["site_id", "drip_entity_id", "drip_entity_name"])
    .agg(
        rate_start=("start_dt", "min"),
        rate_end=("end_dt", "max"),
    )
    .reset_index()
)
drip_rate_coverage["rate_days"] = (drip_rate_coverage["rate_end"] - drip_rate_coverage["rate_start"]).dt.days + 1


full_year_both = (
    drip_iso_coverage.merge(drip_rate_coverage, on=["site_id", "drip_entity_id", "drip_entity_name"], how="inner")
)
full_year_both = full_year_both[(full_year_both["iso_days"] >= 365) & (full_year_both["rate_days"] >= 365)]
full_year_both = full_year_both.merge(site, on="site_id", how="left")
full_year_both["min_days"] = full_year_both[["iso_days", "rate_days"]].min(axis=1)
full_year_both = full_year_both.sort_values("min_days", ascending=False).reset_index(drop=True)


# ========================================================
# 13) Lat–lon bounding box filter (sites)
# ========================================================

def filter_sites_bbox(lat_min, lat_max, lon_min, lon_max):
    return site[
        (site["latitude"] >= lat_min) & (site["latitude"] <= lat_max) &
        (site["longitude"] >= lon_min) & (site["longitude"] <= lon_max)
    ].copy()


# ========================================================
# 14) Frequency tables: hourly/daily/monthly
# (Most common freq_class per entity)
# ========================================================

drip_iso_freq_by_entity = (
    drip_iso_sample.assign(freq_class=classify_freq(drip_iso_sample["drip_iso_accumulation_unit"],
                                                   drip_iso_sample["drip_iso_accumulation_time"]))
    .groupby(["drip_entity_id", "freq_class"])
    .size()
    .reset_index(name="n")
    .sort_values(["drip_entity_id", "n"], ascending=[True, False])
)
drip_iso_freq_by_entity = drip_iso_freq_by_entity.drop_duplicates("drip_entity_id", keep="first").reset_index(drop=True)

drip_rate_freq_by_entity = (
    drip_rate_sample.assign(freq_class=classify_freq(drip_rate_sample["drip_rate_accumulation_unit"],
                                                    drip_rate_sample["drip_rate_accumulation_time"]))
    .groupby(["drip_entity_id", "freq_class"])
    .size()
    .reset_index(name="n")
    .sort_values(["drip_entity_id", "n"], ascending=[True, False])
)
drip_rate_freq_by_entity = drip_rate_freq_by_entity.drop_duplicates("drip_entity_id", keep="first").reset_index(drop=True)

precip_freq_by_entity = (
    precip_sample.assign(freq_class=classify_freq(precip_sample["precip_accumulation_unit"],
                                                 precip_sample["precip_accumulation_time"]))
    .groupby(["precip_entity_id", "freq_class"])
    .size()
    .reset_index(name="n")
    .sort_values(["precip_entity_id", "n"], ascending=[True, False])
)
precip_freq_by_entity = precip_freq_by_entity.drop_duplicates("precip_entity_id", keep="first").reset_index(drop=True)

print("DONE.")
