Scripts to query the SISAL_monv1 database in one sql format and 15 flat csv files available here: https://repo.researchdata.hu/privateurl.xhtml?token=43a43257-f06e-4dc5-9e96-6603eabe775f

MS figures -> code used to derive the figures shown in the database paper to be published in Earth System Science Data (Treble et al., 2026) QC script_v5.3.R -> Script to quality check the input workbooks

GlobalProductsScripts_SISAL_monv1.7z -> Original MATLAB scripts (ERA5_Temp.m, GLEAM_PET_AET.m, MSWEP_Precip.m) used to extract ERA5 temperature, GLEAM PET/AET, and MSWEP precipitation for SISAL_monv1 sites from their respective gridded products

sisal_monv1_db_schema_final.sql -> the schema the published SISAL_monv1 database was built from. Run it to create an empty database with the same tables, keys and indexes.

R and Python scripts to connect to the database loaded into MySQL and some query examples
sisal_connect2db_v3.R sisal_connect2db_v3.py

R and Python scripts to connect to the database in the form of 15 csv files and an extensive set of query examples
sisal_monv1_extractCSVdata.R sisal_monv1_extractCSVdata.py
