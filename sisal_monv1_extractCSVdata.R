########################################################
# SISAL_monv1 flat-CSV query cookbook (R / dplyr)
# - Loads all CSVs from one folder
# - Builds common "queries" using joins (no SQL)
########################################################

# 0) Packages
# install.packages(c("dplyr", "lubridate", "readr"))
library(dplyr)
library(lubridate)
library(readr)

########################################################
# 1) CSV loader
# Important: put ONLY the SISAL_monv1 CSV tables in this folder.
########################################################

folder_path <- "E:/Google Drive/flat_csv_db_v8.0"  # <-- change

csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  stop("No .csv files found in folder_path. Check the path and contents.")
}

# Read + assign into GlobalEnv by file name (without .csv)
for (file in csv_files) {
  file_name <- tools::file_path_sans_ext(basename(file))
  message("Reading: ", file_name)
  
  df <- readr::read_csv(file, show_col_types = FALSE, progress = FALSE)
  assign(file_name, df, envir = .GlobalEnv)
}

########################################################
# 2) Sanity checks: required tables present?
########################################################

required_tables <- c(
  "site", "notes", "reference",
  "site_link_precip", "site_link_reference", "entity_link_reference",
  "precip_site", "precip_entity", "precip_sample",
  "cave_entity",
  "drip_entity", "drip_iso_sample", "drip_rate_sample", "mod_carb_sample"
)

missing_tables <- setdiff(required_tables, ls(envir = .GlobalEnv))
if (length(missing_tables) > 0) {
  stop("Missing required table(s): ", paste(missing_tables, collapse = ", "))
}

########################################################
# 3) Helpers
########################################################

clean_empty_to_na <- function(df) {
  df %>% mutate(across(where(is.character), ~ na_if(trimws(.), "")))
}

collapse_unique <- function(x, sep = " ; ") {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  paste(unique(x), collapse = sep)
}

hhmm_to_hm <- function(hhmm) {
  hhmm <- ifelse(is.na(hhmm), 0, hhmm)
  hhmm <- as.integer(hhmm)
  h <- hhmm %/% 100
  m <- hhmm %% 100
  list(h = h, m = m)
}

# --- SAFE datetime maker (vectorized) ---
make_dt <- function(yyyy, mm = NA, dd = NA, hhmm = NA,
                    tz = "UTC",
                    default_mm = 6, default_dd = 15, default_hhmm = 1200) {
  
  # if year missing -> NA
  out <- rep(as.POSIXct(NA), length(yyyy))
  ok  <- !is.na(yyyy)
  
  mm2   <- ifelse(is.na(mm),   default_mm,   mm)
  dd2   <- ifelse(is.na(dd),   default_dd,   dd)
  hhmm2 <- ifelse(is.na(hhmm), default_hhmm, hhmm)
  
  hh <- floor(hhmm2 / 100)
  mi <- hhmm2 %% 100
  
  # build ISO string
  x <- sprintf("%04d-%02d-%02d %02d:%02d:00", yyyy, mm2, dd2, hh, mi)
  
  out[ok] <- as.POSIXct(x[ok], tz = tz, format = "%Y-%m-%d %H:%M:%S")
  out
}


classify_freq <- function(unit, time) {
  unit <- tolower(unit)
  case_when(
    unit == "hours"   & time == 1  ~ "hourly",
    unit == "minutes" & time == 60 ~ "hourly",
    unit == "days"    & time == 1  ~ "daily",
    unit == "hours"   & time == 24 ~ "daily",
    unit == "months"  & time == 1  ~ "monthly",
    unit == "days"    & time %in% 28:31 ~ "monthly",
    TRUE ~ "other/unknown"
  )
}

########################################################
# 4) Clean tables (convert "" -> NA in char cols)
########################################################

site                  <- clean_empty_to_na(site)
notes                 <- clean_empty_to_na(notes)
reference             <- clean_empty_to_na(reference)
site_link_precip      <- clean_empty_to_na(site_link_precip)
site_link_reference   <- clean_empty_to_na(site_link_reference)
entity_link_reference <- clean_empty_to_na(entity_link_reference)

precip_site    <- clean_empty_to_na(precip_site)
precip_entity  <- clean_empty_to_na(precip_entity)
precip_sample  <- clean_empty_to_na(precip_sample)

cave_entity    <- clean_empty_to_na(cave_entity)
drip_entity    <- clean_empty_to_na(drip_entity)
drip_iso_sample  <- clean_empty_to_na(drip_iso_sample)
drip_rate_sample <- clean_empty_to_na(drip_rate_sample)
mod_carb_sample  <- clean_empty_to_na(mod_carb_sample)

########################################################
# 5) SITE SUMMARY: how many entities per site
########################################################

cave_entity_counts <- cave_entity %>%
  group_by(site_id) %>%
  summarise(cave_entity_count = n_distinct(cave_entity_id), .groups = "drop")

drip_counts <- drip_entity %>%
  group_by(site_id) %>%
  summarise(drip_entity_count = n_distinct(drip_entity_id), .groups = "drop")

precip_counts <- site_link_precip %>%
  group_by(site_id) %>%
  summarise(precip_entity_count = n_distinct(precip_entity_id), .groups = "drop")

site_summary <- site %>%
  left_join(cave_entity_counts, by = "site_id") %>%
  left_join(drip_counts, by = "site_id") %>%
  left_join(precip_counts, by = "site_id") %>%
  mutate(
    cave_entity_count   = ifelse(is.na(cave_entity_count), 0, cave_entity_count),
    drip_entity_count   = ifelse(is.na(drip_entity_count), 0, drip_entity_count),
    precip_entity_count = ifelse(is.na(precip_entity_count), 0, precip_entity_count),
    ) %>%
  arrange(site_id)

########################################################
# 6) “Do we have entries?” checks for required measurement fields
########################################################

drip_iso_presence_global <- drip_iso_sample %>%
  summarise(
    n_rows = n(),
    n_d18O = sum(!is.na(drip_iso_d18O_measurement)),
    n_d2H  = sum(!is.na(drip_iso_d2H_measurement)),
    n_both = sum(!is.na(drip_iso_d18O_measurement) & !is.na(drip_iso_d2H_measurement))
  )


mod_carb_presence_global <- mod_carb_sample %>%
  summarise(
    n_rows = n(),
    n_d18O = sum(!is.na(mod_carb_d18O_measurement)),
    n_d13C = sum(!is.na(mod_carb_d13C_measurement)),
    n_both = sum(!is.na(mod_carb_d18O_measurement) & !is.na(mod_carb_d13C_measurement))
  )

precip_presence_global <- precip_sample %>%
  summarise(
    n_rows   = n(),
    n_amount = sum(!is.na(precip_amount)),
    n_d18O   = sum(!is.na(precip_d18O_measurement)),
    n_d2H    = sum(!is.na(precip_d2H_measurement))
  )

########################################################
# 7) Per-site flags: which sites have which data?
########################################################

drip_iso_site_flags <- drip_entity %>%
  select(site_id, drip_entity_id) %>%
  inner_join(drip_iso_sample, by = "drip_entity_id") %>%
  group_by(site_id) %>%
  summarise(
    has_drip_iso_d18O = any(!is.na(drip_iso_d18O_measurement)),
    has_drip_iso_d2H  = any(!is.na(drip_iso_d2H_measurement)),
    has_drip_iso_any  = any(!is.na(drip_iso_d18O_measurement) | !is.na(drip_iso_d2H_measurement)),
    .groups = "drop"
  )

drip_rate_site_flags <- drip_entity %>%
  select(site_id, drip_entity_id) %>%
  inner_join(drip_rate_sample, by = "drip_entity_id") %>%
  group_by(site_id) %>%
  summarise(
    has_drip_rate = any(!is.na(drip_rate_measurement)),
    .groups = "drop"
  )

mod_carb_site_flags <- drip_entity %>%
  select(site_id, drip_entity_id) %>%
  inner_join(mod_carb_sample, by = "drip_entity_id") %>%
  group_by(site_id) %>%
  summarise(
    has_mod_carb_d18O = any(!is.na(mod_carb_d18O_measurement)),
    has_mod_carb_d13C = any(!is.na(mod_carb_d13C_measurement)),
    has_mod_carb_any  = any(!is.na(mod_carb_d18O_measurement) | !is.na(mod_carb_d13C_measurement)),
    .groups = "drop"
  )


precip_site_flags <- site_link_precip %>%
  inner_join(precip_sample, by = "precip_entity_id") %>%
  group_by(site_id) %>%
  summarise(
    has_precip_amount = any(!is.na(precip_amount)),
    has_precip_d18O   = any(!is.na(precip_d18O_measurement)),
    has_precip_d2H    = any(!is.na(precip_d2H_measurement)),
    has_precip_anyiso = any(!is.na(precip_d18O_measurement) | !is.na(precip_d2H_measurement)),
    .groups = "drop"
  ) #Warning message: In inner_join(., precip_sample, by = "precip_entity_id") :  Detected an unexpected many-to-many relationship between `x` and `y`.

site_data_counts <- site %>%
  select(site_id) %>%
  left_join(drip_iso_site_flags,  by = "site_id") %>%
  left_join(drip_rate_site_flags, by = "site_id") %>%
  left_join(mod_carb_site_flags,  by = "site_id") %>%
  left_join(precip_site_flags,    by = "site_id") %>%
  summarise(
    n_sites_total      = n_distinct(site_id),
    n_sites_precip_iso = sum(has_precip_anyiso %in% TRUE, na.rm = TRUE),
    n_sites_drip_iso   = sum(has_drip_iso_any  %in% TRUE, na.rm = TRUE),
    n_sites_drip_rate  = sum(has_drip_rate     %in% TRUE, na.rm = TRUE),
    n_sites_mod_carb   = sum(has_mod_carb_any  %in% TRUE, na.rm = TRUE)
  )

########################################################
# 8) “Map tables”: all sites with each data type
########################################################

sites_drip_iso_map <- site %>%
  inner_join(drip_iso_site_flags %>% filter(has_drip_iso_any), by = "site_id") %>%
  select(site_id, site_name, latitude, longitude, elevation)

sites_drip_rate_map <- site %>%
  inner_join(drip_rate_site_flags %>% filter(has_drip_rate), by = "site_id") %>%
  select(site_id, site_name, latitude, longitude, elevation)

sites_mod_carb_map <- site %>%
  inner_join(mod_carb_site_flags %>% filter(has_mod_carb_any), by = "site_id") %>%
  select(site_id, site_name, latitude, longitude, elevation)

site_precip_map <- site %>%
  inner_join(site_link_precip, by = "site_id") %>%
  left_join(precip_site, by = "precip_site_id") %>%
  left_join(precip_entity, by = "precip_entity_id") %>%
  select(
    site_id, site_name, latitude, longitude, elevation,
    precip_site_id, precip_site_name, precip_latitude, precip_longitude, precip_elevation,
    precip_entity_id, precip_entity_name, precip_method
  ) %>%
  distinct()




########################################################
# 8.1) Actually “Map tables”: all sites with each data type
########################################################

library(ggplot2)
library(dplyr)
library(ggmap)


########################################################
# 8.1) Actually “Map tables”: all sites with each data type
#      + Global map + 3 zoomed regional maps
#      - Drip Iso: hollow BLUE triangle
#      - Drip Rate: hollow ORANGE square
#      - Mod Carb: slightly smaller FILLED red dot
#      - Precip: BLACK cross
#      - legends on ALL plots
########################################################

library(ggplot2)
library(dplyr)
library(maps)

# ---- Build map tables ----
sites_drip_iso_map <- site %>%
  inner_join(drip_iso_site_flags %>% filter(has_drip_iso_any), by = "site_id") %>%
  select(site_id, site_name, latitude, longitude, elevation)

sites_drip_rate_map <- site %>%
  inner_join(drip_rate_site_flags %>% filter(has_drip_rate), by = "site_id") %>%
  select(site_id, site_name, latitude, longitude, elevation)

sites_mod_carb_map <- site %>%
  inner_join(mod_carb_site_flags %>% filter(has_mod_carb_any), by = "site_id") %>%
  select(site_id, site_name, latitude, longitude, elevation)

site_precip_map <- site %>%
  inner_join(site_link_precip, by = "site_id") %>%
  left_join(precip_site,  by = "precip_site_id") %>%
  left_join(precip_entity, by = "precip_entity_id") %>%
  select(
    site_id, site_name, latitude, longitude, elevation,
    precip_site_id, precip_site_name, precip_latitude, precip_longitude, precip_elevation,
    precip_entity_id, precip_entity_name, precip_method
  ) %>%
  distinct()

# ---- Combine (deduplicate within each layer) ----
combined_sites <- bind_rows(
  mutate(distinct(sites_drip_iso_map,  site_id, .keep_all = TRUE), data_type = "Drip Iso"),
  mutate(distinct(sites_drip_rate_map, site_id, .keep_all = TRUE), data_type = "Drip Rate"),
  mutate(distinct(sites_mod_carb_map,  site_id, .keep_all = TRUE), data_type = "Mod Carb"),
  mutate(distinct(site_precip_map,     site_id, .keep_all = TRUE), data_type = "Precip")
)

# Split for layered plotting (to allow Mod Carb smaller + filled)
combined_hollow <- combined_sites %>% filter(data_type %in% c("Drip Iso", "Drip Rate"))
combined_modcarb <- combined_sites %>% filter(data_type == "Mod Carb")
combined_precip <- combined_sites %>% filter(data_type == "Precip")

# ---- Aesthetics ----
type_colors <- c(
  "Drip Iso"  = "blue",
  "Drip Rate" = "green",
  "Mod Carb"  = "red",
  "Precip"    = "black"
)

type_shapes <- c(
  "Drip Iso"  = 2,   # hollow triangle
  "Drip Rate" = 0,   # hollow square
  "Mod Carb"  = 16,  # filled circle (dot)
  "Precip"    = 4    # cross
)

marker_stroke <- 0.35
size_default  <- 2.6
size_modcarb  <- 2.1   # a bit smaller than before

# ---- Helpers for formatting axes ----
lon_lab <- function(x) ifelse(x < 0, paste0(abs(x), "°W"), paste0(x, "°E"))
lat_lab <- function(x) ifelse(x < 0, paste0(abs(x), "°S"), paste0(x, "°N"))

# ---- Base map function ----
base_map <- function(xlim = c(-180, 180), ylim = c(-60, 85)) {
  ggplot() +
    borders("world", colour = "gray85", fill = "gray95") +
    coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 9)
    )
}

add_site_layers <- function(p) {
  p +
    # Hollow layers (Drip Iso, Drip Rate)
    geom_point(
      data = combined_hollow,
      aes(x = longitude, y = latitude, colour = data_type, shape = data_type),
      size = size_default,
      stroke = marker_stroke,
      alpha = 0.9
    ) +
    # Mod Carb: smaller FILLED red dot
    geom_point(
      data = combined_modcarb,
      aes(x = longitude, y = latitude, colour = data_type, shape = data_type),
      size = size_modcarb,
      stroke = 0,
      alpha = 0.9
    ) +
    # Precip: black cross
    geom_point(
      data = combined_precip,
      aes(x = longitude, y = latitude, colour = data_type, shape = data_type),
      size = size_default,
      stroke = marker_stroke,
      alpha = 0.9
    ) +
    scale_colour_manual(values = type_colors, name = "Data type") +
    scale_shape_manual(values = type_shapes, name = "Data type")
}

# ---- Global map ----
plot_global <- base_map(xlim = c(-180, 180), ylim = c(-60, 85)) |>
  add_site_layers() +
  labs(
    title = "Site locations with different data types (flat CSV)",
    subtitle = "Hollow: Drip Iso / Drip Rate; Filled: Mod Carb; Cross: Precip",
    x = "Longitude [deg]",
    y = "Latitude [deg]"
  ) +
  scale_x_continuous(breaks = seq(-180, 180, by = 60), labels = lon_lab) +
  scale_y_continuous(breaks = seq(-60,  80,  by = 20), labels = lat_lab)

# ---- Zoomed regions ----
inset_theme <- theme(legend.position = "right")

# North America
plot_NAm <- base_map(xlim = c(-140, -50), ylim = c(0, 60)) |>
  add_site_layers() +
  scale_x_continuous(
    breaks = seq(-140, -50, by = 20),
    labels = lon_lab,
    guide  = guide_axis(angle = 30)
  ) +
  scale_y_continuous(
    breaks = seq(0, 60, by = 10),
    labels = lat_lab
  ) +
  labs(x = "Longitude [deg]", y = "Latitude [deg]", title = "North America") +
  inset_theme

# Europe
plot_Europe <- base_map(xlim = c(-10, 30), ylim = c(30, 70)) |>
  add_site_layers() +
  scale_x_continuous(
    breaks = seq(-10, 30, by = 10),
    labels = lon_lab,
    guide  = guide_axis(angle = 30)
  ) +
  scale_y_continuous(
    breaks = seq(30, 70, by = 10),
    labels = lat_lab
  ) +
  labs(x = "Longitude [deg]", y = "Latitude [deg]", title = "Europe") +
  inset_theme

# East Asia
plot_EAsia <- base_map(xlim = c(95, 130), ylim = c(15, 45)) |>
  add_site_layers() +
  scale_x_continuous(
    breaks = seq(95, 130, by = 10),
    labels = lon_lab,
    guide  = guide_axis(angle = 30)
  ) +
  scale_y_continuous(
    breaks = seq(15, 45, by = 10),
    labels = lat_lab
  ) +
  labs(x = "Longitude [deg]", y = "Latitude [deg]", title = "East Asia") +
  inset_theme

# ---- Print plots ----
plot_global
plot_NAm
plot_Europe
plot_EAsia



########################################################
# 9) References + notes (site-level and entity-level)
########################################################

site_citations <- site_link_reference %>%
  left_join(reference, by = "ref_id") %>%
  group_by(site_id) %>%
  summarise(
    site_citations = collapse_unique(citation),
    site_DOI       = collapse_unique(publication_DOI),
    .groups = "drop"
  )

drip_entity_citations <- entity_link_reference %>%
  filter(!is.na(drip_entity_id)) %>%
  left_join(reference, by = "ref_id") %>%
  group_by(drip_entity_id) %>%
  summarise(
    drip_citations = collapse_unique(citation),
    drip_DOI       = collapse_unique(publication_DOI),
    .groups = "drop"
  )

precip_entity_citations <- entity_link_reference %>%
  filter(!is.na(precip_entity_id)) %>%
  left_join(reference, by = "ref_id") %>%
  group_by(precip_entity_id) %>%
  summarise(
    precip_citations = collapse_unique(citation),
    precip_DOI       = collapse_unique(publication_DOI),
    .groups = "drop"
  )

site_notes <- notes %>%
  group_by(site_id) %>%
  summarise(notes = collapse_unique(notes), .groups = "drop")

########################################################
# 10) Modern carbonate records (all)
########################################################

mod_carb_records <- drip_entity %>%
  select(site_id, drip_entity_id, drip_entity_name, entity_id) %>%
  inner_join(mod_carb_sample, by = "drip_entity_id") %>%
  left_join(site, by = "site_id") %>%
  left_join(site_notes, by = "site_id") %>%
  left_join(site_citations, by = "site_id") %>%
  left_join(drip_entity_citations, by = "drip_entity_id") %>%
  mutate(
    # If only year exists, interpret as full-year coverage:
    start_dt = make_dt(mod_carb_start_yyyy, mod_carb_start_mm, mod_carb_start_dd,
                       default_mm = 1, default_dd = 1, default_hhmm = 0),
    end_dt   = make_dt(mod_carb_end_yyyy,   mod_carb_end_mm,   mod_carb_end_dd,
                       default_mm = 12, default_dd = 31, default_hhmm = 2359),
    
    # IMPORTANT: use if_else (not ifelse) to preserve POSIXct class
    end_dt = dplyr::if_else(is.na(end_dt), start_dt, end_dt),
    
    freq_class = classify_freq(mod_carb_accumulation_unit, mod_carb_accumulation_time)
  ) %>%
  arrange(site_id, drip_entity_id, start_dt)


########################################################
# 11) # Filter records with SISAL entities (site_id < 1000)
########################################################


sisal_sites <- site %>%
  filter(site_id < 1000)  # SISAL entities have site_id < 1000

########################################################
# 12) >= 1 year of BOTH drip_rate and drip_iso (same drip_entity_id)
########################################################

drip_iso_coverage <- drip_entity %>%
  select(site_id, drip_entity_id, drip_entity_name) %>%
  inner_join(drip_iso_sample, by = "drip_entity_id") %>%
  mutate(
    start_dt = make_dt(drip_iso_start_yyyy, drip_iso_start_mm, drip_iso_start_dd, drip_iso_start_hhmm),
    end_dt   = make_dt(drip_iso_end_yyyy,   drip_iso_end_mm,   drip_iso_end_dd,   drip_iso_end_hhmm),
    end_dt   = ifelse(is.na(end_dt), start_dt, end_dt)
  ) %>%
  filter(!is.na(start_dt), !is.na(end_dt)) %>%
  group_by(site_id, drip_entity_id, drip_entity_name) %>%
  summarise(
    iso_start = min(start_dt),
    iso_end   = max(end_dt),
    iso_days  = as.numeric(difftime(max(end_dt), min(start_dt), units = "days")) + 1,
    .groups = "drop"
  )

drip_rate_coverage <- drip_entity %>%
  select(site_id, drip_entity_id, drip_entity_name) %>%
  inner_join(drip_rate_sample, by = "drip_entity_id") %>%
  mutate(
    start_dt = make_dt(drip_rate_start_yyyy, drip_rate_start_mm, drip_rate_start_dd, drip_rate_start_hhmm),
    end_dt   = make_dt(drip_rate_end_yyyy,   drip_rate_end_mm,   drip_rate_end_dd,   drip_rate_end_hhmm),
    end_dt   = ifelse(is.na(end_dt), start_dt, end_dt)
  ) %>%
  filter(!is.na(start_dt), !is.na(end_dt)) %>%
  group_by(site_id, drip_entity_id, drip_entity_name) %>%
  summarise(
    rate_start = min(start_dt),
    rate_end   = max(end_dt),
    rate_days  = as.numeric(difftime(max(end_dt), min(start_dt), units = "days")) + 1,
    .groups = "drop"
  )

full_year_both <- drip_iso_coverage %>%
  inner_join(drip_rate_coverage, by = c("site_id", "drip_entity_id", "drip_entity_name")) %>%
  filter(iso_days >= 365, rate_days >= 365) %>%
  left_join(site, by = "site_id") %>%
  arrange(desc(pmin(iso_days, rate_days)))

########################################################
# 13) Lat–lon bounding box filter (sites)
########################################################

filter_sites_bbox <- function(lat_min, lat_max, lon_min, lon_max) {
  site %>%
    filter(latitude >= lat_min, latitude <= lat_max,
           longitude >= lon_min, longitude <= lon_max)
}

########################################################
# 14) Frequency tables: hourly/daily/monthly
########################################################

drip_iso_freq_by_entity <- drip_iso_sample %>%
  mutate(freq_class = classify_freq(drip_iso_accumulation_unit, drip_iso_accumulation_time)) %>%
  count(drip_entity_id, freq_class, name = "n") %>%
  group_by(drip_entity_id) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  ungroup()

drip_rate_freq_by_entity <- drip_rate_sample %>%
  mutate(freq_class = classify_freq(drip_rate_accumulation_unit, drip_rate_accumulation_time)) %>%
  count(drip_entity_id, freq_class, name = "n") %>%
  group_by(drip_entity_id) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  ungroup()

precip_freq_by_entity <- precip_sample %>%
  mutate(freq_class = classify_freq(precip_accumulation_unit, precip_accumulation_time)) %>%
  count(precip_entity_id, freq_class, name = "n") %>%
  group_by(precip_entity_id) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  ungroup()


########################################################
# END
########################################################
