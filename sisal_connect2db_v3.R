#0) What you need installed (once)

#In R (Console):
install.packages(c("DBI", "RMariaDB", "openxlsx"))

#What these are:
#  DBI: standard database interface in R
#RMariaDB: modern MySQL/MariaDB connector (works great with MySQL too)
#openxlsx: writes .xlsx without needing Excel installed

#1) R script (copy into e.g. sisal_monv1_export.R)

#Change only the connection settings at the top.

# ============================================================
# SISAL_monv1: MySQL -> R -> Excel (modern R / DBI)
# ============================================================

library(DBI)
library(RMariaDB)
library(openxlsx)

# -------------------------
# 1) CONNECTION SETTINGS
# -------------------------
MYSQL_HOST <- "localhost"
MYSQL_PORT <- 3306
MYSQL_USER <- "root"
MYSQL_PASSWORD <- "password"      # <- change this
MYSQL_DB <- "sisal_monv1"         # <- important

# -------------------------
# 2) CONNECT
# -------------------------
con <- dbConnect(
  RMariaDB::MariaDB(),
  host = MYSQL_HOST,
  port = MYSQL_PORT,
  user = MYSQL_USER,
  password = MYSQL_PASSWORD,
  dbname = MYSQL_DB
)

# Optional: allow long GROUP_CONCAT results for citations
dbExecute(con, "SET SESSION group_concat_max_len = 100000;")

# Quick smoke test (should return a number)
print(dbGetQuery(con, "SELECT COUNT(*) AS n_sites FROM site;"))

# -------------------------
# 3) EXAMPLE QUERIES
# -------------------------

#3.0 # See which site has - and how many - entites (drip entity, cave entity, precip entity)
site_entity_counts_sql <- " 
SELECT 
s.site_id, s.site_name, s.latitude, s.longitude, s.elevation, 
COUNT(DISTINCT cem.cave_entity_id) AS cave_entity_count, 
COUNT(DISTINCT dem.drip_entity_id) AS drip_entity_count, 
COUNT(DISTINCT pem.precip_entity_id) AS precip_entity_count, 
( COUNT(DISTINCT cem.cave_entity_id) + 
COUNT(DISTINCT dem.drip_entity_id) + 
COUNT(DISTINCT pem.precip_entity_id) 
) AS entity_count 
FROM site s 
LEFT JOIN cave_entity cem ON cem.site_id = s.site_id 
LEFT JOIN drip_entity dem ON dem.site_id = s.site_id 
LEFT JOIN site_link_precip slp ON slp.site_id = s.site_id -- Correct join 
LEFT JOIN precip_entity pem ON pem.precip_entity_id = 
slp.precip_entity_id -- Correct join 
GROUP BY s.site_id, s.site_name, s.latitude, s.longitude, s.elevation 
ORDER BY s.site_id; "

site_summary <- dbGetQuery(con, site_entity_counts_sql)


# Query to fetch a specific site_name based on your condition
site_name_example <- "REPLACE_WITH_SITE_NAME"


#3.1 # SQL query to fetch drip isotope sample data for a specific site name
drip_iso_sample_sql <- "
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
WHERE s.site_name = ?site_name
ORDER BY d.drip_entity_id, di.drip_iso_start_yyyy, di.drip_iso_start_mm, di.drip_iso_start_dd;
"

# Execute the query with site_name parameter
drip_iso_df <- dbGetQuery(
  con,
  sqlInterpolate(con, drip_iso_sample_sql, site_name = site_name_example)
)


#3.2 # SQL query to fetch drip rate sample data for a specific site name
drip_rate_sample_sql <- "
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
WHERE s.site_name = ?site_name
ORDER BY
  d.drip_entity_id,
  dr.drip_rate_start_yyyy,
  dr.drip_rate_start_mm,
  dr.drip_rate_start_dd;
"


drip_rate_df <- dbGetQuery(
  con,
  sqlInterpolate(con, drip_rate_sample_sql, site_name = site_name_example)
)



#3.3 # SQL query to fetch precip sample data for a specific site name
precip_sample_sql <- "
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
WHERE s.site_name = ?site_name
ORDER BY
  pem.precip_entity_id,
  p.precip_start_yyyy,
  p.precip_start_mm,
  p.precip_start_dd;
"

precip_df <- dbGetQuery(
  con,
  sqlInterpolate(con, precip_sample_sql, site_name = site_name_example)
)



#3.4 # SQL query to fetch references for a given site.
site_entity_counts_sql <- " 
SELECT s.site_id, s.site_name, s.latitude, 
s.longitude, s.elevation, 
COUNT(DISTINCT cem.cave_entity_id) AS cave_entity_count, 
COUNT(DISTINCT dem.drip_entity_id) AS drip_entity_count, 
COUNT(DISTINCT pem.precip_entity_id) AS precip_entity_count, 
( COUNT(DISTINCT cem.cave_entity_id) + 
COUNT(DISTINCT dem.drip_entity_id) + 
COUNT(DISTINCT pem.precip_entity_id) ) 
AS entity_count FROM site s 
LEFT JOIN cave_entity cem ON cem.site_id = s.site_id 
LEFT JOIN drip_entity dem ON dem.site_id = s.site_id 
LEFT JOIN site_link_precip slp ON slp.site_id = s.site_id -- 
Correct join LEFT JOIN precip_entity pem ON pem.precip_entity_id = slp.precip_entity_id -- 
Correct join GROUP BY s.site_id, s.site_name, s.latitude, s.longitude, s.elevation ORDER BY s.site_id; 
"


site_summary <- dbGetQuery(con, site_entity_counts_sql)


#3.5 # SQL query to fetch all metadata for a given bounding box given in lat and lon
#Given variables can be left out.

Global_metadata_sql <- "
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
"


Global_metadata_df <- dbGetQuery(
  con,
  sqlInterpolate(con, Global_metadata_sql)
)




#3.6 # SQL query to fetch all metadata for a given bounding box given in lat and lon
site_refs_siteonly_sql <- "
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
"
site_refs_siteonly <- dbGetQuery(con, site_refs_siteonly_sql)



# -------------------------
# 4) EXPORT TO EXCEL
# -------------------------
out_xlsx <- "SISAL_monv1_export_examples_R_drip.xlsx"


openxlsx::write.xlsx(
  x = list(
    site_summary       = site_summary,
    drip_iso_example   = drip_iso_df,
    drip_rate_example  = drip_rate_df,
    refs = site_refs_siteonly
  ),
  file = out_xlsx,
  overwrite = TRUE
)

cat("Wrote Excel file:", normalizePath(out_xlsx), "\n")

# -------------------------
# 6) DISCONNECT
# -------------------------
dbDisconnect(con)
