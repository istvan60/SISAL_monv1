# ============================================================
# SISAL_monv1: MySQL -> Python -> Excel
# (translation of the provided R script)
# ============================================================

# pip install pandas sqlalchemy pymysql openpyxl

import pandas as pd
from sqlalchemy import create_engine, text

# -------------------------
# 1) CONNECTION SETTINGS
# -------------------------
MYSQL_HOST = "localhost"
MYSQL_PORT = 3306
MYSQL_USER = "root"
MYSQL_PASSWORD = "password"   # <- change this
MYSQL_DB = "sisal_monv1"      # <- important

# SQLAlchemy engine (PyMySQL driver)
engine = create_engine(
    f"mysql+pymysql://{MYSQL_USER}:{MYSQL_PASSWORD}@{MYSQL_HOST}:{MYSQL_PORT}/{MYSQL_DB}"
    "?charset=utf8mb4"
)

# Optional: allow long GROUP_CONCAT results for citations
with engine.begin() as conn:
    conn.execute(text("SET SESSION group_concat_max_len = 100000;"))

# Quick smoke test
with engine.begin() as conn:
    n_sites = pd.read_sql("SELECT COUNT(*) AS n_sites FROM site;", conn)
print(n_sites)

# -------------------------
# 2) PARAMETERS
# -------------------------
site_name_example = "REPLACE_WITH_SITE_NAME"  # <- set this

# -------------------------
# 3) SQL QUERIES
# -------------------------

# 3.0) Site entity counts
site_entity_counts_sql = """
SELECT
  s.site_id, s.site_name, s.latitude, s.longitude, s.elevation,
  COUNT(DISTINCT cem.cave_entity_id)   AS cave_entity_count,
  COUNT(DISTINCT dem.drip_entity_id)   AS drip_entity_count,
  COUNT(DISTINCT pem.precip_entity_id) AS precip_entity_count,
  ( COUNT(DISTINCT cem.cave_entity_id)
  + COUNT(DISTINCT dem.drip_entity_id)
  + COUNT(DISTINCT pem.precip_entity_id)
  ) AS entity_count
FROM site s
LEFT JOIN cave_entity cem  ON cem.site_id = s.site_id
LEFT JOIN drip_entity dem  ON dem.site_id = s.site_id
LEFT JOIN site_link_precip slp      ON slp.site_id = s.site_id
LEFT JOIN precip_entity pem ON pem.precip_entity_id = slp.precip_entity_id
GROUP BY s.site_id, s.site_name, s.latitude, s.longitude, s.elevation
ORDER BY s.site_id;
"""

# 3.1) Drip isotope sample for a site_name
drip_iso_sample_sql = """
SELECT
  s.site_name,
  d.drip_entity_id,
  d.drip_entity_name,
  di.drip_iso_start_yyyy, di.drip_iso_start_mm, di.drip_iso_start_dd,
  di.drip_iso_end_yyyy,   di.drip_iso_end_mm,   di.drip_iso_end_dd,
  di.drip_iso_d18O_measurement,
  di.drip_iso_d2H_measurement,
  (di.drip_iso_d2H_measurement - 8 * di.drip_iso_d18O_measurement) AS d_excess
FROM site s
JOIN drip_entity d ON d.site_id = s.site_id
JOIN drip_iso_sample di ON di.drip_entity_id = d.drip_entity_id
WHERE s.site_name = :site_name
ORDER BY d.drip_entity_id, di.drip_iso_start_yyyy, di.drip_iso_start_mm, di.drip_iso_start_dd;
"""

# 3.2) Drip rate sample for a site_name
drip_rate_sample_sql = """
SELECT
  s.site_name,
  d.drip_entity_id,
  d.drip_entity_name,
  dr.drip_rate_start_yyyy, dr.drip_rate_start_mm, dr.drip_rate_start_dd,
  dr.drip_rate_end_yyyy,   dr.drip_rate_end_mm,   dr.drip_rate_end_dd,
  dr.drip_rate_measurement,
  dr.drip_rate_precision
FROM site s
JOIN drip_entity d
  ON d.site_id = s.site_id
JOIN drip_rate_sample dr
  ON dr.drip_entity_id = d.drip_entity_id
WHERE s.site_name = :site_name
ORDER BY
  d.drip_entity_id,
  dr.drip_rate_start_yyyy,
  dr.drip_rate_start_mm,
  dr.drip_rate_start_dd;
"""

# 3.3) Precip sample for a site_name (using site_link_precip logic)
precip_sample_sql = """
SELECT
  s.site_name,
  psm.precip_site_name,
  pem.precip_entity_id,
  pem.precip_entity_name,
  p.precip_start_yyyy, p.precip_start_mm, p.precip_start_dd,
  p.precip_end_yyyy,   p.precip_end_mm,   p.precip_end_dd,
  p.precip_amount,
  p.precip_d18O_measurement,
  p.precip_d2H_measurement,
  (p.precip_d2H_measurement - 8 * p.precip_d18O_measurement) AS d_excess
FROM site s
JOIN site_link_precip slp
  ON slp.site_id = s.site_id
JOIN precip_site psm
  ON psm.precip_site_id = slp.precip_site_id
JOIN precip_entity pem
  ON pem.precip_entity_id = slp.precip_entity_id
JOIN precip_sample p
  ON p.precip_entity_id = pem.precip_entity_id
WHERE s.site_name = :site_name
ORDER BY
  pem.precip_entity_id,
  p.precip_start_yyyy,
  p.precip_start_mm,
  p.precip_start_dd;
"""


# 3.6) Site-only references for ALL sites
site_refs_siteonly_sql = """
SELECT
  s.site_id,
  s.site_name,
  s.latitude,
  s.longitude,
  s.elevation,
  GROUP_CONCAT(DISTINCT r.citation ORDER BY r.citation SEPARATOR ' ; ')        AS citations,
  GROUP_CONCAT(DISTINCT r.publication_DOI ORDER BY r.publication_DOI SEPARATOR ' ; ') AS publication_DOI
FROM site s
LEFT JOIN site_link_reference slr ON slr.site_id = s.site_id
LEFT JOIN reference r             ON r.ref_id   = slr.ref_id
GROUP BY s.site_id, s.site_name, s.latitude, s.longitude, s.elevation
ORDER BY s.site_id;
"""

# 3.5) Global metadata example (optional; included here because it exists in your R)
global_sql = """
SELECT DISTINCT s.site_name, s.site_id, 
precip.precip_site_id,
precip.precip_site_name, 
cave_entity.cave_entity_id,
cave_entity.cave_entity_name,
cave_entity.cave_entity_location,
cave_entity.cave_entity_contact,
drip_entity.*, s.latitude, s.longitude
FROM site s
LEFT JOIN site_link_precip sp_link ON s.site_id = sp_link.site_id
LEFT JOIN precip_site precip ON precip.precip_site_id = sp_link.precip_site_id
LEFT JOIN cave_entity ON s.site_id = cave_entity.site_id
LEFT JOIN drip_entity ON s.site_id = drip_entity.site_id
WHERE 1 = 1  and s.latitude between -90 and 90
and s.longitude between -180 and 180
"""

# -------------------------
# 4) RUN QUERIES
# -------------------------
params = {"site_name": site_name_example}

with engine.begin() as conn:
    site_summary = pd.read_sql(site_entity_counts_sql, conn)
    drip_iso_df = pd.read_sql(text(drip_iso_sample_sql), conn, params=params)
    drip_rate_df = pd.read_sql(text(drip_rate_sample_sql), conn, params=params)
    precip_df = pd.read_sql(text(precip_sample_sql), conn, params=params)
    site_refs_siteonly = pd.read_sql(site_refs_siteonly_sql, conn)
    global_df = pd.read_sql(global_sql, conn)  # optional

# -------------------------
# 5) EXPORT TO EXCEL
# -------------------------
out_xlsx = "SISAL_monv1_export_examples_PY.xlsx"

with pd.ExcelWriter(out_xlsx, engine="openpyxl") as writer:
    site_summary.to_excel(writer, sheet_name="site_summary", index=False)
    drip_iso_df.to_excel(writer, sheet_name="drip_iso_example", index=False)
    drip_rate_df.to_excel(writer, sheet_name="drip_rate_example", index=False)
    precip_df.to_excel(writer, sheet_name="precip_example", index=False)
    site_refs_siteonly.to_excel(writer, sheet_name="refs_siteonly", index=False)
    global_df.to_excel(writer, sheet_name="global", index=False)  # optional

print("Wrote Excel file:", out_xlsx)
