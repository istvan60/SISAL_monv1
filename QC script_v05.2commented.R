
# Load necessary libraries
library(readxl)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(maps)
library(ggmap)
library(gridExtra)
library(ggspatial)
library(broom)

file_name <-"2024-01-08_monitoringdb_nk_lcb_workbook_arcy-sur-cure_cave_yz.xlsx"  #example

site_data <- read.xlsx(file_name, sheet = "site", startRow = 2, colNames = TRUE)
reference_data <- read.xlsx(file_name, sheet = "reference", startRow = 2, colNames = TRUE)
notes_data <- read.xlsx(file_name, sheet = "notes", startRow = 2, colNames = TRUE)
precip_site_metadata_data <- read.xlsx(file_name, sheet = "precip_site_metadata", startRow = 2, colNames = TRUE)
precip_entity_metadata_data <- read.xlsx(file_name, sheet = "precip_entity_metadata", startRow = 2, colNames = TRUE)
precip_sample_data <- read.xlsx(file_name, sheet = "precip_sample", startRow = 2, colNames = TRUE)
cave_entity_metadata_data <- read.xlsx(file_name, sheet = "cave_entity_metadata", startRow = 2, colNames = TRUE)
cave_temperature_sample_data <- read.xlsx(file_name, sheet = "cave_temperature_sample", startRow = 2, colNames = TRUE)
cave_relative_humidity_sample_data <- read.xlsx(file_name, sheet = "cave_relative_humidity_sample", startRow = 2, colNames = TRUE)
cave_pCO2_sample_data <- read.xlsx(file_name, sheet = "cave_pCO2_sample", startRow = 2, colNames = TRUE)
drip_entity_metadata_data <- read.xlsx(file_name, sheet = "drip_entity_metadata", startRow = 2, colNames = TRUE)
drip_iso_sample_data <- read.xlsx(file_name, sheet = "drip_iso_sample", startRow = 2, colNames = TRUE)
drip_rate_sample_data <- read.xlsx(file_name, sheet = "drip_rate_sample", startRow = 2, colNames = TRUE)
mod_carb_sample_data <- read.xlsx(file_name, sheet = "mod_carb_sample", startRow = 2, colNames = TRUE)

cave_name <- site_data$site_name

# Remove check variables
text_variable <- ls()
chck_string <- "check"
matching_elements <- grep(chck_string, text_variable, value = TRUE)
rm(list=matching_elements)

##############################################################################x
# T01 Site table


sheet_name <- "site"
site_data <- read.xlsx(file_name, sheet = sheet_name, startRow = 2, colNames = TRUE)

if (dim(site_data)[1] != 0){
  
  # -----------------------------------------------------
  # 1. There can only be one site (i.e. one row) (***) 
  T01.01_one_site_check <- length(site_data[,1]) == 1
  
  # -----------------------------------------------------
  # 2. “site_name” cannot start/end with a space (**)
  
  ### Extract the relevant row for checks
  data_row <- site_data[1, ]
  
  ### Check if 'site_name' in cell A3 starts or ends with a space
  site_name <- data_row$site_name
  T01.02_site_name_check <- if (startsWith(site_name, " ") | endsWith(site_name, " ")) FALSE else TRUE
  
  # -----------------------------------------------------
  # 3. “latitude” and “longitude” must be within a valid range (i.e. -90 to 90 for latitude and -180 to 180 for longitude) (**) 
  
  ### Convert latitude and longitude to numeric for range checking
  latitude <- as.numeric(data_row$latitude)
  longitude <- as.numeric(data_row$longitude)
  
  # Check if 'latitude' and 'longitude' are within valid ranges
  T01.03_latitude_check <- !is.na(latitude) && latitude >= -90 && latitude <= 90
  T01.03_longitude_check <- !is.na(longitude) && longitude >= -180 && longitude <= 180
  
  # -----------------------------------------------------
  # 4.  “elevation” should not be missing (*) 
  T01.04_elevation_check <- !is.na(data_row$elevation)
  
  # Print results
  cat("One Site Only Check: ",T01.01_one_site_check, "\n")
  cat("Site Name Check: ", T01.02_site_name_check, "\n")
  cat("Latitude Check: ", T01.03_latitude_check, "\n")
  cat("Longitude Check: ", T01.03_longitude_check, "\n")
  cat("Elevation Check: ", T01.04_elevation_check, "\n")
  
  # Summarize all checks
  T01_all_checks <- T01.01_one_site_check & T01.02_site_name_check & T01.03_longitude_check &  T01.03_longitude_check & T01.04_elevation_check
  
  T01_all_checks <- paste("Site table checks passed:",T01_all_checks)
  
  cat("All T01 Site table checks passed: ", T01_all_checks, "\n")
  
} else {
  T01_all_checks <- paste("The", sheet_name, "table table contains no data.")
  cat("All checks passed: FAILED | The", sheet_name, "table contains no data.", "\n")
}

################################################################################################################
################################################################################################################
################################################################################################################

# T02 Reference table

sheet_name <- "reference"
reference_data <- read.xlsx(file_name, sheet = sheet_name, startRow = 2, colNames = TRUE)

if (dim(reference_data)[1] != 0){
  
  # -----------------------------------------------------
  # 1.  How many rows contain information, if multiple, the information in the 'site_name' column must be the same for all rows.
  ### Check the number of rows containing information
  info_rows <- reference_data %>% filter_all(any_vars(!is.na(.)))
  
  ### Check if 'site_name' column has the same value for all rows
  unique_site_names <- unique(info_rows$site_name)
  T02.01_site_name_check <- length(unique_site_names) == 1
  
  # -----------------------------------------------------
  # 2.  If a “citation” appears more than once, its “publication_doi” must be the same (**) 
  ### Check if the same 'citation' is paired with the same 'publication_doi'
  T02.02_citation_doi_consistency_check <- info_rows %>%
    group_by(citation) %>%
    summarize(unique_dois = n_distinct(publication_doi)) %>%
    ungroup() %>%
    summarise(all_unique = all(unique_dois == 1)) %>%
    pull(all_unique)
  
  # -----------------------------------------------------
  # 3.  If two DOIs are the same, their linked citation must be the same unless the DOI is “unpublished” (**)
  
  # -----------------------------------------------------
  # 4.  There should not be repeated “citations” for one entity  (**) 
  unique_citations <- unique(info_rows$citation)
  T02.04_no_duplicate_citation_check <- length(unique_citations) == length(info_rows$citation)
  
  unique_dois <- unique(info_rows$publication_doi)
  T02.04_no_duplicate_dois_check <- length(unique_dois) == length(info_rows$publication_doi)
  
  # -----------------------------------------------------
  # 5.  There should be no spaces before/after the “publication_doi” (**) 
  T02.05_doi_no_spaces_check <- all(!grepl("^\\s|\\s$", info_rows$publication_doi))
  
  
  # Check if 'publication_doi' values are 'unpublished', start with 'http', or start with '10'
  T02.05_doi_valid_check <- all(grepl("^(unpublished$|http://|10)", info_rows$publication_doi)) || all(grepl("^(unpublished$|https://|10)", info_rows$publication_doi))
  
  # Check for valid 'citation' values
  invalid_citations <- c("unknown", "N/A", "not known")
  T02.05_citation_valid_check <- all(!info_rows$citation %in% invalid_citations & 
                                       !grepl("^\\s|\\s$", info_rows$citation))
  
  # -----------------------------------------------------
  # x.   There should not be null value in site_name row
  # outp.: ERROR
  T02.0x_no_empty_sitename_or_citation_check <- sum(unlist(lapply(info_rows$site_name, is.null))) == 0 &
    sum(unlist(lapply(info_rows$citation, is.null))) == 0
  
  # -----------------------------------------------------
  # x.   
  # Is there really no reference for the data?
  T02.0x_references_filled_check <- dim(reference_data)[1] != 0
  
  # Print results
  cat("Site Name Consistency Check: ", T02.01_site_name_check, "\n")
  cat("Citation Doi Consistency Check: ", T02.02_citation_doi_consistency_check, "\n")
  cat("No Duplicate Citation Check: ", T02.04_no_duplicate_citation_check, "\n")
  cat("No Duplicate DOI Check: ", T02.04_no_duplicate_citation_check, "\n")
  cat("DOI No Spaces Check: ", T02.05_doi_no_spaces_check, "\n")
  cat("DOI Validity Check: ", T02.05_doi_valid_check, "\n")
  cat("Citation Validity Check: ", T02.05_citation_valid_check, "\n")
  cat("No Empty Sitename or Citation: ",T02.0x_no_empty_sitename_or_citation_check, "\n")
  cat("References Filled: ",T02.0x_references_filled_check, "\n")
  
  # Summarize all checks
  T02_all_checks <- T02.01_site_name_check & 
    T02.02_citation_doi_consistency_check & 
    T02.04_no_duplicate_citation_check &
    T02.05_doi_no_spaces_check &
    T02.05_doi_valid_check &
    T02.05_citation_valid_check&
    T02.0x_references_filled_check&
    T02.0x_no_empty_sitename_or_citation_check
  
  T02_all_checks <- paste("Reference table checks passed:",T02_all_checks)
  
  cat("All REFERENCE tab checks passed: ", T02_all_checks, "\n")
  
} else {
  T02_all_checks <- paste("The", sheet_name, "table contains no data.")
  cat("All checks passed: FAILED | The", sheet_name, "table contains no data.", "\n")
}

################################################################################################################
################################################################################################################
################################################################################################################

#T03 Precip site metadata table

sheet_name <- "precip_site_metadata"
precip_data <- read.xlsx(file_name, sheet = sheet_name, startRow = 2, colNames = TRUE)

if (dim(precip_data)[1] != 0){
  
  # -----------------------------------------------------
  # 1. “precip_entity_name” in this table must be unique (***) 
  T03.01_unique_precip_entity_name_check <- n_distinct(precip_data$precip_entity_name) == nrow(precip_data)
  
  # -----------------------------------------------------
  # 2. “precip_site_name” cannot start/end with a space (**)
  # Check if 'precip_site_name' does not start or end with a space
  T03.02_precip_site_name_check <- all(!grepl("^\\s|\\s$", precip_data$precip_site_name))
  
  
  # -----------------------------------------------------
  # 3. “precip_latitude” and “precip_longitude” must be within a valid range (i.e. -90 to 90 for latitude and -180 to 180 for longitude (**) 
  latitude_chk <- all(!is.na(precip_data$precip_latitude) & precip_data$precip_latitude >= -90 & precip_data$precip_latitude <= 90)
  longitude_chk <- all(!is.na(precip_data$precip_longitude) & precip_data$precip_longitude >= -180 & precip_data$precip_longitude <= 180)
  T03.03_lat_lon_check <- latitude_chk & longitude_chk
  
  # -----------------------------------------------------
  # 4. “precip_elevation” should not be missing (*)
  # Check if 'precip_elevation' is not missing
  T03.04_elevation_check <- all(!is.na(precip_data$precip_elevation))
  
  # -----------------------------------------------------
  # 5. “precip_distance_cave_entrance” should not be missing (*) 
  # Check if 'precip_distance_cave_entrance' is not missing
  T03.05_distance_cave_entrance_check <- all(!is.na(precip_data$precip_distance_cave_entrance))
  
  # Print results
  cat("Unique Precip Entity Name Check: ", T03.01_unique_precip_entity_name_check, "\n")
  cat("Precip Site Name Check: ", T03.02_precip_site_name_check, "\n")
  cat("Latitude & Longitude Check: ", T03.03_lat_lon_check, "\n")
  cat("Elevation Check: ", T03.04_elevation_check, "\n")
  cat("Distance to Cave Entrance Check: ", T03.05_distance_cave_entrance_check, "\n")
  
  # Summarize all checks
  T03_all_checks <- T03.01_unique_precip_entity_name_check & T03.02_precip_site_name_check & T03.03_lat_lon_check & T03.04_elevation_check & T03.05_distance_cave_entrance_check
  
  T03_all_checks <- paste("Precip site metadata table checks passed:",T03_all_checks)
  
  cat("All checks passed: ", T03_all_checks, "\n")
  
} else {
  T03_all_checks <- paste("The", sheet_name, "table contains no data.")
  cat("All checks passed: FAILED | The", sheet_name, "table contains no data.", "\n")
}

################################################################################################################
################################################################################################################
################################################################################################################

#T04 Precip entity metadata table


sheet_name <- "precip_entity_metadata"

precip_entity_data <- read.xlsx(file_name, sheet = sheet_name, startRow = 2, colNames = TRUE)

if (dim(precip_entity_data)[1] != 0){
  
  # -----------------------------------------------------
  #1. All drop-down menus must be filled in with pre-defined options (**) 
  # Check if 'precip_method' is valid
  valid_methods <- c("IAEA/GNIP", "other (see notes)", "unknown")
  T04.01_precip_method_check <- all(precip_entity_data$precip_method %in% valid_methods)
  
  # -----------------------------------------------------
  # 2. “precip_entity_contact“ cannot be numeric, it must contain two words (i.e. name and surname) and cannot start/end with a space. (**) 
  # Check if 'precip_entity_contact' is valid
  T04.02_precip_entity_contact_check <- all(
    !grepl("^\\s|\\s$", precip_entity_data$precip_entity_contact) & # no leading or trailing spaces
      #sapply(strsplit(precip_entity_data$precip_entity_contact, " "), length) == 2 & # contains exactly two words
      !grepl("^[0-9]+$", precip_entity_data$precip_entity_contact) # not numeric
  )
  
  # Print results
  cat("Precip Method Check: ", T04.01_precip_method_check, "\n")
  cat("Precip Entity Contact Check: ", T04.02_precip_entity_contact_check, "\n")
  
  # Summarize all checks
  T04_all_checks <- T04.01_precip_method_check & T04.02_precip_entity_contact_check
  
  T04_all_checks <- paste("Precip entity metadata table checks passed:",T04_all_checks)
  
  cat("All checks passed: ", T04_all_checks, "\n")
  
} else {
  T04_all_checks <- paste("The", sheet_name, "table contains no data.")
  cat("All checks passed: FAILED | The", sheet_name, "table contains no data.", "\n")
}

################################################################################################################
################################################################################################################
################################################################################################################

#T05 Precip_sample table


sheet_name <- "precip_sample"

precip_sample_data <- read.xlsx(file_name, sheet = sheet_name, startRow = 2, colNames = TRUE)

if (dim(precip_sample_data)[1] != 0){
  
  # -----------------------------------------------------
  # 1. All drop-down menus a must be filled in with the pre-defined options (**) 
  # Check if 'precip_accumulation_unit' is valid
  valid_units <- c("days", "hours", "minutes", "unknown")
  T05.01_accumulation_unit_check <- all(precip_sample_data$precip_accumulation_unit %in% valid_units)
  
  # -----------------------------------------------------
  # 2. “precip_start_yyyy” must be a 4 digit whole number and must be within a valid range (i.e. 1900 to 2024) (**)
  # 3. “precip_start_mm” must be a 1 or 2 digit whole number and must be within a valid range (i.e. 1 to 12) (**)
  # 4. “precip_start_dd” must be a 1 or 2 digit whole number and must be within a valid range (i.e. 1 to 31) (**)
  # 5. “precip_start_hhmm” must be a 4 digit whole number and must be within a valid range (i.e. 0001 to 2359)
  # 6. “precip_end_yyyy” must be a 4 digit whole number and must be within a valid range (i.e. 1900 to 2024) (**)
  # 7. “precip_end_mm” must be a 1 or 2 digit whole number and must be within a valid range (i.e. 1 to 12) (**)
  # 8. “precip_end_dd” must be a 1 or 2 digit whole number and must be within a valid range (i.e. 1 to 31) (**)
  # 9. “precip_end_hhmm” must be a 4 digit number and must be within a valid range (i.e. 0001 to 2359)
  
  # Function to check 4 digit whole number within a range
  check_4digit_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{4}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Function to check 1 or 2 digit whole number within a range
  check_1or2digit_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{1,2}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Function to check 4 digit whole number within hhmm range
  check_hhmm_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{4}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Checking start and end dates and times
  T05.02_start_year_check <- check_4digit_within_range(precip_sample_data$precip_start_yyyy, 1800, 2024)
  T05.03_start_month_check <- check_1or2digit_within_range(precip_sample_data$precip_start_mm, 1, 12)
  T05.04_start_day_check <- check_1or2digit_within_range(precip_sample_data$precip_start_dd, 1, 31)
  T05.05_start_hhmm_check <- check_hhmm_within_range(precip_sample_data$precip_start_hhmm, 1, 2359)
  
  T05.06_end_year_check <- check_4digit_within_range(precip_sample_data$precip_end_yyyy, 1800, 2024)
  T05.07_end_month_check <- check_1or2digit_within_range(precip_sample_data$precip_end_mm, 1, 12)
  T05.08_end_day_check <- check_1or2digit_within_range(precip_sample_data$precip_end_dd, 1, 31)
  T05.09_end_hhmm_check <- check_hhmm_within_range(precip_sample_data$precip_end_hhmm, 1, 2359)
  
  
  # -----------------------------------------------------
  # 10. “precip_accumulation_time” must be a number
  # 11. “precip_amount” must be a number
  # 12. “precip_d18O_measurement” must be a number 
  # 13. “precip_d18O_precision” must be a number 
  # 14. “precip_d2H_measurement” must be a number 
  # 15. “precip_d2H_precision” must be a number 
  
  # Check if numeric columns contain numbers excluding NA
  numeric_check_excluding_na <- function(x) {
    all(is.na(x) | !is.na(as.numeric(x)))
  }
  
  T05.10_accumulation_time_check <- numeric_check_excluding_na(precip_sample_data$precip_accumulation_time)
  T05.11_amount_check <- numeric_check_excluding_na(precip_sample_data$precip_amount)
  T05.12_d18O_measurement_check <- numeric_check_excluding_na(precip_sample_data$precip_d18O_measurement)
  T05.13_d18O_precision_check <- numeric_check_excluding_na(precip_sample_data$precip_d18O_precision)
  T05.14_d2H_measurement_check <- numeric_check_excluding_na(precip_sample_data$precip_d2H_measurement)
  T05.15_d2H_precision_check <- numeric_check_excluding_na(precip_sample_data$precip_d2H_precision)
  
  # Print results
  cat("Accumulation Unit Check: ", T05.01_accumulation_unit_check, "\n")
  cat("Start Year Check: ", T05.02_start_year_check, "\n")
  cat("Start Month Check: ", T05.03_start_month_check, "\n")
  cat("Start Day Check: ", T05.04_start_day_check, "\n")
  cat("Start HHMM Check: ", T05.05_start_hhmm_check, "\n")
  cat("End Year Check: ", T05.06_end_year_check, "\n")
  cat("End Month Check: ", T05.07_end_month_check, "\n")
  cat("End Day Check: ", T05.08_end_day_check, "\n")
  cat("End HHMM Check: ", T05.09_end_hhmm_check, "\n")
  cat("Accumulation Time Check: ", T05.10_accumulation_time_check, "\n")
  cat("Amount Check: ", T05.11_amount_check, "\n")
  cat("d18O Measurement Check: ", T05.12_d18O_measurement_check, "\n")
  cat("d18O Precision Check: ", T05.13_d18O_precision_check, "\n")
  cat("d2H Measurement Check: ", T05.14_d2H_measurement_check, "\n")
  cat("d2H Precision Check: ", T05.15_d2H_precision_check, "\n")
  
  # Summarize all checks
  T05_all_checks <- T05.01_accumulation_unit_check & T05.02_start_year_check & T05.03_start_month_check & T05.04_start_day_check & T05.05_start_hhmm_check & 
    T05.06_end_year_check & T05.07_end_month_check & T05.08_end_day_check & T05.09_end_hhmm_check & 
    T05.10_accumulation_time_check & T05.11_amount_check & T05.12_d18O_measurement_check & 
    T05.13_d18O_precision_check & T05.14_d2H_measurement_check & T05.15_d2H_precision_check
  
  T05_all_checks <- paste("Precip_sample table checks passed:",T05_all_checks)
  
  cat("All checks passed: ", T05_all_checks, "\n")
  
  
} else {
  T05_all_checks <- paste("The", sheet_name, "table contains no data.")
  cat("All checks passed: FAILED | The", sheet_name, "table contains no data.", "\n")
}

################################################################################################################
################################################################################################################
################################################################################################################

#T06 cave entity metadata


sheet_name <- "cave_entity_metadata"

cave_entity_data <- read.xlsx(file_name, sheet = sheet_name, startRow = 2, colNames = TRUE)

if (dim(cave_entity_data)[1] != 0){
  
  # -----------------------------------------------------
  # 1. “cave_entity_name” in this table must be unique (***)
  # Check if 'cave_entity_name' is unique
  T06.01_unique_cave_entity_name_check <- n_distinct(cave_entity_data$cave_entity_name) == length(cave_entity_data$cave_entity_name)
  
  
  # -----------------------------------------------------
  # 2. All drop-down menus must be filled in with pre-defined options (**)
  # Check if 'cave_temperature', 'cave_relative_humidity', 'cave_pco2' are valid
  valid_yes_no_unknown <- c("yes", "no", "unknown")
  T06.02_cave_temperature_check <- all(is.na(cave_entity_data$cave_temperature) | cave_entity_data$cave_temperature %in% valid_yes_no_unknown)
  T06.02_cave_relative_humidity_check <- all(is.na(cave_entity_data$cave_relative_humidity) | cave_entity_data$cave_relative_humidity %in% valid_yes_no_unknown)
  T06.02_cave_pco2_check <- all(is.na(cave_entity_data$cave_pco2) | cave_entity_data$cave_pco2 %in% valid_yes_no_unknown)
  
  # Check if frequency columns are valid
  valid_frequencies <- c("regular", "sporadic (see notes)", "other (see notes)", "unknown")
  T06.02_temperature_frequency_check <- all(is.na(cave_entity_data$cave_temperature_frequency) | cave_entity_data$cave_temperature_frequency %in% valid_frequencies)
  T06.02_humidity_frequency_check <- all(is.na(cave_entity_data$cave_relative_humidity_frequency) | cave_entity_data$cave_relative_humidity_frequency %in% valid_frequencies)
  T06.02_pco2_frequency_check <- all(is.na(cave_entity_data$cave_pco2_frequency) | cave_entity_data$cave_pco2_frequency %in% valid_frequencies)
  T06.02_drip_rate_frequency_check <- all(is.na(cave_entity_data$drip_rate_frequency) | cave_entity_data$drip_rate_frequency %in% valid_frequencies)
  
  # Check if instrument columns are valid
  valid_instruments <- c("logger", "hand-held", "other (see notes)", "unknown")
  T06.02_temperature_instrument_check <- all(is.na(cave_entity_data$cave_temperature_instrument) | cave_entity_data$cave_temperature_instrument %in% valid_instruments)
  T06.02_humidity_instrument_check <- all(is.na(cave_entity_data$cave_relative_humidity_instrument) | cave_entity_data$cave_relative_humidity_instrument %in% valid_instruments)
  T06.02_pco2_instrument_check <- all(is.na(cave_entity_data$cave_pco2_instrument) | cave_entity_data$cave_pco2_instrument %in% valid_instruments)
  T06.02_drip_rate_instrument_check <- all(is.na(cave_entity_data$drip_rate_instrument) | cave_entity_data$drip_rate_instrument %in% valid_instruments)
  
  # -----------------------------------------------------
  # 3. “cave_entity_contact“ cannot be numeric, it must contain two words (i.e. name and surname) and cannot start/end with a space. (**) 
  # Check if 'cave_entity_contact' is valid
  T06.03_cave_entity_contact_check <- all(
    is.na(cave_entity_data$cave_entity_contact) | 
      (!grepl("^\\s|\\s$", cave_entity_data$cave_entity_contact) & # no leading or trailing spaces
         #sapply(strsplit(cave_entity_data$cave_entity_contact, " "), length) >= 2 & # contains exactly two words
         !grepl("^[0-9]+$", cave_entity_data$cave_entity_contact)) # not numeric
  )
  
  # Print results
  cat("Unique Cave Entity Name Check: ", T06.01_unique_cave_entity_name_check, "\n")
  cat("Cave Temperature Check: ", T06.02_cave_temperature_check, "\n")
  cat("Cave Relative Humidity Check: ", T06.02_cave_relative_humidity_check, "\n")
  cat("Cave PCO2 Check: ", T06.02_cave_pco2_check, "\n")
  cat("Temperature Frequency Check: ", T06.02_temperature_frequency_check, "\n")
  cat("Humidity Frequency Check: ", T06.02_humidity_frequency_check, "\n")
  cat("PCO2 Frequency Check: ", T06.02_pco2_frequency_check, "\n")
  cat("Drip Rate Frequency Check: ", T06.02_drip_rate_frequency_check, "\n")
  cat("Temperature Instrument Check: ", T06.02_temperature_instrument_check, "\n")
  cat("Humidity Instrument Check: ", T06.02_humidity_instrument_check, "\n")
  cat("PCO2 Instrument Check: ", T06.02_pco2_instrument_check, "\n")
  cat("Drip Rate Instrument Check: ", T06.02_drip_rate_instrument_check, "\n")
  cat("Cave Entity Contact Check: ", T06.03_cave_entity_contact_check, "\n")
  
  # Summarize all checks
  T06_all_checks <- T06.01_unique_cave_entity_name_check & T06.02_cave_temperature_check & T06.02_cave_relative_humidity_check & T06.02_cave_pco2_check &
    T06.02_temperature_frequency_check & T06.02_humidity_frequency_check & T06.02_pco2_frequency_check & T06.02_drip_rate_frequency_check &
    T06.02_temperature_instrument_check & T06.02_humidity_instrument_check & T06.02_pco2_instrument_check & T06.02_drip_rate_instrument_check &
    T06.03_cave_entity_contact_check
  
  T06_all_checks <- paste("Cave entity metadata table checks passed:",T06_all_checks)
  
  cat("All checks passed: ", T06_all_checks, "\n")
  
} else {
  T06_all_checks <- paste("The", sheet_name, "table contains no data.")
  cat("All checks passed: FAILED | The", sheet_name, "table contains no data.", "\n")
}

################################################################################################################
################################################################################################################
################################################################################################################

#T07 cave_temperature_sample

sheet_name <- "cave_temperature_sample"

cave_temperature_sample_data <- read.xlsx(file_name, sheet = sheet_name, startRow = 2, colNames = TRUE)

if (dim(cave_temperature_sample_data)[1] != 0){
  
  # Function to check 4 digit whole number within a range
  #check_4digit_within_range <- function(x, min_val, max_val) {
  #  all(is.na(x) | (grepl("^\\d{4}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  #}
  # Function to check 1 or 2 digit whole number within a range
  #check_1or2digit_within_range <- function(x, min_val, max_val) {
  #  all(is.na(x) | (grepl("^\\d{1,2}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  #}
  # Function to check 4 digit whole number within hhmm range
  #check_hhmm_within_range <- function(x, min_val, max_val) {
  #  all(is.na(x) | (grepl("^\\d{4}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  #}
  
  # -----------------------------------------------------
  # 1. “cave_temperature_yyyy” must be a 4 digit whole number and must be within a valid range (i.e. 1900 to 2024) (**)
  # 2. “cave_temperature_mm” must be a 1 or 2 digit whole number and must be within a valid range (i.e. 1 to 12) (**)
  # 3. “cave_temperature _dd” must be a 1 or 2 digit whole number and must be within a valid range (i.e. 1 to 31) (**)
  # 4. “cave_temperature _hhmm” must be a 4 digit whole number and must be within a valid range (i.e. 0001 to 2359)
  
  # Checking 'cave_temperature_yyyy', 'cave_temperature_mm', 'cave_temperature_dd', and 'cave_temperature_hhmm'
  T07.01_year_check <- check_4digit_within_range(cave_temperature_sample_data$cave_temperature_yyyy, 1800, 2024)
  T07.02_month_check <- check_1or2digit_within_range(cave_temperature_sample_data$cave_temperature_mm, 1, 12)
  T07.03_day_check <- check_1or2digit_within_range(cave_temperature_sample_data$cave_temperature_dd, 1, 31)
  T07.04_day_check <- check_hhmm_within_range(cave_temperature_sample_data$cave_temperature_hhmm, 1, 2359)
  
  # -----------------------------------------------------
  # 5. “cave_temperature _number” must be must be a whole number
  # Check if 'cave_temperature_number' is a whole number
  T07.05_number_check <- all(is.na(cave_temperature_sample_data$cave_temperature_number) | (cave_temperature_sample_data$cave_temperature_number %% 1 == 0))
  
  # -----------------------------------------------------
  # 6. “cave_temperature _measurement” must be within a valid range (i.e. -60 to +50)
  # Check if 'cave_temperature_measurement' is within -60 to +50
  T07.06_measurement_check <- all(is.na(cave_temperature_sample_data$cave_temperature_measurement) | 
                                    (as.numeric(cave_temperature_sample_data$cave_temperature_measurement) >= -60 & 
                                       as.numeric(cave_temperature_sample_data$cave_temperature_measurement) <= 50))
  
  # -----------------------------------------------------
  # 7. “cave_temperature _precision” must be within a valid range (i.e. 0 to 5)
  # Check if 'cave_temperature_precision' is within 0 to 5
  T07.07_precision_check <- all(is.na(cave_temperature_sample_data$cave_temperature_precision) | 
                                  (as.numeric(cave_temperature_sample_data$cave_temperature_precision) >= 0 & 
                                     as.numeric(cave_temperature_sample_data$cave_temperature_precision) <= 5))
  
  # Print results
  cat("Year Check: ", T07.01_year_check, "\n")
  cat("Month Check: ", T07.02_month_check, "\n")
  cat("Day Check: ", T07.03_day_check, "\n")
  cat("HHMM Check: ", T07.04_day_check, "\n")
  cat("Number Check: ", T07.05_number_check, "\n")
  cat("Measurement Check: ", T07.06_measurement_check, "\n")
  cat("Precision Check: ", T07.07_precision_check, "\n")
  
  # Summarize all checks
  T07_all_checks <- T07.01_year_check & T07.02_month_check & T07.03_day_check & T07.04_day_check & T07.05_number_check & T07.06_measurement_check & T07.07_precision_check
  
  T07_all_checks <- paste("Cave temperature sample checks passed:",T07_all_checks)
  
  cat("All checks passed: ", T07_all_checks, "\n")
  
} else {
  T07_all_checks <- paste("The", sheet_name, "table contains no data.")
  cat("All checks passed: FAILED | The", sheet_name, "table contains no data.", "\n")
}




################################################################################################################
################################################################################################################
################################################################################################################
### ITT TARTOTTAM
################################################################################################################
################################################################################################################
################################################################################################################




#T08 cave_relative_humidity_sample

sheet_name <- "cave_relative_humidity_sample"

cave_relative_humidity_sample_data <- read.xlsx(file_name, sheet = sheet_name, startRow = 2, colNames = TRUE)

if (dim(cave_relative_humidity_sample_data)[1] != 0){
  
  # Function to check 4 digit whole number within a range
  check_4digit_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{4}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Function to check 1 or 2 digit whole number within a range
  check_1or2digit_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{1,2}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Function to check 4 digit whole number within hhmm range
  check_hhmm_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{4}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Checking 'cave_relative_humidity_yyyy', 'cave_relative_humidity_mm', 'cave_relative_humidity_dd', and 'cave_relative_humidity_hhmm'
  T08.01_year_check <- check_4digit_within_range(cave_relative_humidity_sample_data$cave_relative_humidity_yyyy, 1800, 2024)
  T08.02_month_check <- check_1or2digit_within_range(cave_relative_humidity_sample_data$cave_relative_humidity_mm, 1, 12)
  T08.03_day_check <- check_1or2digit_within_range(cave_relative_humidity_sample_data$cave_relative_humidity_dd, 1, 31)
  T08.04_hhmm_check <- check_hhmm_within_range(cave_relative_humidity_sample_data$cave_relative_humidity_hhmm, 1, 2359)
  
  # Check if 'cave_relative_humidity_number' is a whole number
  T08.05_number_check <- all(is.na(cave_relative_humidity_sample_data$cave_relative_humidity_number) | 
                               (cave_relative_humidity_sample_data$cave_relative_humidity_number %% 1 == 0))
  
  # Check if 'cave_relative_humidity_measurement' is within 0 to 100
  T08.06_measurement_check <- all(is.na(cave_relative_humidity_sample_data$cave_relative_humidity_measurement) | 
                                    (as.numeric(cave_relative_humidity_sample_data$cave_relative_humidity_measurement) >= 0 & 
                                       as.numeric(cave_relative_humidity_sample_data$cave_relative_humidity_measurement) <= 100))
  
  # Check if 'cave_relative_humidity_precision' is within 0 to 20
  T08.07_precision_check <- all(is.na(cave_relative_humidity_sample_data$cave_relative_humidity_precision) | 
                                  (as.numeric(cave_relative_humidity_sample_data$cave_relative_humidity_precision) >= 0 & 
                                     as.numeric(cave_relative_humidity_sample_data$cave_relative_humidity_precision) <= 20))
  
  # Print results
  cat("Year Check: ", T08.01_year_check, "\n")
  cat("Month Check: ", T08.02_month_check, "\n")
  cat("Day Check: ", T08.03_day_check, "\n")
  cat("HHMM Check: ", T08.04_hhmm_check, "\n")
  cat("Number Check: ", T08.05_number_check, "\n")
  cat("Measurement Check: ", T08.06_measurement_check, "\n")
  cat("Precision Check: ", T08.07_precision_check, "\n")
  
  # Summarize all checks
  T08_all_checks <- T08.01_year_check & T08.02_month_check & T08.03_day_check & T08.04_hhmm_check & T08.05_number_check & T08.06_measurement_check & T08.07_precision_check
  
  T08_all_checks <- paste("Cave relative humidity sample checks passed:",T08_all_checks)
  
  cat("All checks passed: ", T08_all_checks, "\n")
  
} else {
  T08_all_checks <- paste("The", sheet_name, "table contains no data.")
  cat("All checks passed: FAILED | The", sheet_name, "table contains no data.", "\n")
}



################################################################################################################
################################################################################################################
################################################################################################################

#T09 cave_pCO2_sample

sheet_name <- "cave_pCO2_sample"


cave_pCO2_sample_data <- read.xlsx(file_name, sheet = sheet_name, startRow = 2, colNames = TRUE)

if (dim(cave_pCO2_sample_data)[1] != 0){
  
  if (names(cave_pCO2_sample_data)[7] != "cave_pCO2_measurement") {names(cave_pCO2_sample_data)[7] <- "cave_pCO2_measurement"}
  
  # Function to check 4 digit whole number within a range
  check_4digit_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{4}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Function to check 1 or 2 digit whole number within a range
  check_1or2digit_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{1,2}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Function to check 4 digit whole number within hhmm range
  check_hhmm_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{4}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Checking 'cave_pCO2_sample_yyyy', 'cave_pCO2_sample_mm', 'cave_pCO2_sample_dd', and 'cave_pCO2_sample_hhmm'
  T09.01_year_check <- check_4digit_within_range(cave_pCO2_sample_data$cave_pCO2_sample_yyyy, 1800, 2024)
  T09.02_month_check <- check_1or2digit_within_range(cave_pCO2_sample_data$cave_pCO2_sample_mm, 1, 12)
  T09.03_day_check <- check_1or2digit_within_range(cave_pCO2_sample_data$cave_pCO2_sample_dd, 1, 31)
  T09.04_hhmm_check <- check_hhmm_within_range(cave_pCO2_sample_data$cave_pCO2_sample_hhmm, 1, 2359)
  
  # Check if 'cave_pCO2_sample_number' is a whole number
  T09.05_number_check <- all(is.na(cave_pCO2_sample_data$cave_pCO2_sample_number) | 
                               (cave_pCO2_sample_data$cave_pCO2_sample_number %% 1 == 0))
  
  # Check if 'cave_pCO2_sample_measurement' is within 0 to 100000
  T09.06_measurement_check <- all(is.na(cave_pCO2_sample_data$cave_pCO2_sample_measurement) | 
                                    (as.numeric(cave_pCO2_sample_data$cave_pCO2_sample_measurement) >= 0 & 
                                       as.numeric(cave_pCO2_sample_data$cave_pCO2_sample_measurement) <= 100000))
  
  # Check if 'cave_pCO2_sample_precision' is a number
  T09.07_precision_check <- all(is.na(cave_pCO2_sample_data$cave_pCO2_sample_precision) | !is.na(as.numeric(cave_pCO2_sample_data$cave_pCO2_sample_precision)))
  
  # Print results
  cat("Year Check: ", T09.01_year_check, "\n")
  cat("Month Check: ", T09.02_month_check, "\n")
  cat("Day Check: ", T09.03_day_check, "\n")
  cat("HHMM Check: ", T09.04_hhmm_check, "\n")
  cat("Number Check: ", T09.05_number_check, "\n")
  cat("Measurement Check: ", T09.06_measurement_check, "\n")
  cat("Precision Check: ", T09.07_precision_check, "\n")
  
  # Summarize all checks
  T09_all_checks <- T09.01_year_check & T09.02_month_check & T09.03_day_check & T09.04_hhmm_check & T09.05_number_check & T09.06_measurement_check & T09.07_precision_check
  
  T09_all_checks <- paste("Cave pCO2 sample checks passed:",T09_all_checks)
  
  cat("All checks passed: ", T09_all_checks, "\n")
  
} else {
  T09_all_checks <- paste("The", sheet_name, "table contains no data.")
  cat("All checks passed: FAILED | The", sheet_name, "table contains no data.", "\n")
}


################################################################################################################
################################################################################################################
################################################################################################################

#T10 drip_entity_metadata

sheet_name <- "drip_entity_metadata"

drip_entity_data <- read.xlsx(file_name, sheet = sheet_name, startRow = 2, colNames = TRUE)

if (dim(drip_entity_data)[1] != 0){
  
  # Check if 'drip_entity_name' is unique
  T10.01_unique_drip_entity_name_check <- n_distinct(drip_entity_data$drip_entity_name) == nrow(drip_entity_data)
  
  # Check if 'entity_id' is a whole number within 1 to 2000
  T10.02_entity_id_check <- all(is.na(drip_entity_data$entity_id) | 
                                  (drip_entity_data$entity_id %% 1 == 0 & 
                                     drip_entity_data$entity_id >= 1 & 
                                     drip_entity_data$entity_id <= 2000))
  
  # Check if 'drip_entity_contact' is valid
  T10.04_drip_entity_contact_check <- all(
    !is.na(drip_entity_data$drip_entity_contact) | 
      (!grepl("^\\s|\\s$", drip_entity_data$drip_entity_contact) & # no leading or trailing spaces
         #sapply(strsplit(replace(drip_entity_data$drip_entity_contact,is.na(drip_entity_data$drip_entity_contact),""), " "), length) == 2 & # contains exactly two words
         !grepl("^[0-9]+$", drip_entity_data$drip_entity_contact)) # not numeric
  )
  
  # Check if 'drip_entity_metadata_geology' is valid
  valid_geologies <- c("limestone", "dolomite", "marble", "dolomite limestone", "marly limestone", 
                       "calcarenite", "mixed (see notes)", "other (see notes)", "unknown")
  T10.03_geology_check <- all(is.na(drip_entity_data$drip_entity_metadata_geology) | 
                                drip_entity_data$drip_entity_metadata_geology %in% valid_geologies)
  
  # Check if 'drip_entity_metadata_rock_age' is valid
  valid_rock_ages <- c("Holocene", "Pleistocene", "Pliocene", "Miocene", "Oligocene", "Eocene", 
                       "Palaeocene", "Cretaceous", "Jurassic", "Triassic", "Permian", "Carboniferous", 
                       "Devonian", "Silurian", "Ordovician", "Cambrian", "Precambrian", 
                       "mixed (see notes)", "other (see notes)", "unknown")
  T10.03_rock_age_check <- all(is.na(drip_entity_data$drip_entity_metadata_rock_age) | 
                                 drip_entity_data$drip_entity_metadata_rock_age %in% valid_rock_ages)
  
  # Check if 'drip_entity_metadata_drip_iso', 'drip_entity_metadata_drip_rate', and 'drip_entity_metadata_mod_carb' are valid
  valid_yes_no_unknown <- c("yes", "no", "unknown")
  T10.03_drip_iso_check <- all(is.na(drip_entity_data$drip_iso) | 
                                 drip_entity_data$drip_iso %in% valid_yes_no_unknown)
  T10.03_drip_rate_check <- all(is.na(drip_entity_data$drip_rate) | 
                                  drip_entity_data$drip_rate %in% valid_yes_no_unknown)
  T10.03_mod_carb_check <- all(is.na(drip_entity_data$mod_carb) | 
                                 drip_entity_data$mod_carb %in% valid_yes_no_unknown)
  
  # Print results
  cat("Unique Drip Entity Name Check: ", T10.01_unique_drip_entity_name_check, "\n")
  cat("Entity ID Check: ", T10.02_entity_id_check, "\n")
  cat("Drip Entity Contact Check: ", T10.04_drip_entity_contact_check, "\n")
  cat("Geology Check: ", T10.03_geology_check, "\n")
  cat("Rock Age Check: ", T10.03_rock_age_check, "\n")
  cat("Drip Iso Check: ", T10.03_drip_iso_check, "\n")
  cat("Drip Rate Check: ", T10.03_drip_rate_check, "\n")
  cat("Mod Carb Check: ", T10.03_mod_carb_check, "\n")
  
  # Summarize all checks
  T10_all_checks <- T10.01_unique_drip_entity_name_check & T10.02_entity_id_check & T10.04_drip_entity_contact_check & T10.03_geology_check & 
    T10.03_rock_age_check & T10.03_drip_iso_check & T10.03_drip_rate_check & T10.03_mod_carb_check
  
  T10_all_checks <- paste("drip_entity_metadata checks passed:",T10_all_checks)
  
  cat("All checks passed: ", T10_all_checks, "\n")
  
} else {
  T10_all_checks <- paste("The", sheet_name, "table contains no data.")
  cat("All checks passed: FAILED | The", sheet_name, "table contains no data.", "\n")
}

################################################################################################################
################################################################################################################
################################################################################################################

#T11 drip_iso_sample

sheet_name <- "drip_iso_sample"

# Load necessary libraries
drip_iso_sample_data <- read.xlsx(file_name, sheet = sheet_name, startRow = 2, colNames = TRUE)

if (dim(drip_iso_sample_data)[1] != 0){
  
  # Function to check 4 digit whole number within a range
  check_4digit_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{4}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Function to check 1 or 2 digit whole number within a range
  check_1or2digit_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{1,2}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Function to check 4 digit whole number within hhmm range
  check_hhmm_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{4}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Check if 'drip_iso_accumulation_unit' is valid
  valid_units <- c("days", "hours", "minutes", "seconds", "unknown")
  T11.01_accumulation_unit_check <- all(is.na(drip_iso_sample_data$drip_iso_accumulation_unit) | 
                                          drip_iso_sample_data$drip_iso_accumulation_unit %in% valid_units)
  
  # Checking 'drip_iso_start_yyyy', 'drip_iso_start_mm', 'drip_iso_start_dd', 'drip_iso_start_hhmm'
  T11.02_start_year_check <- check_4digit_within_range(drip_iso_sample_data$drip_iso_start_yyyy, 1800, 2024)
  T11.03_start_month_check <- check_1or2digit_within_range(drip_iso_sample_data$drip_iso_start_mm, 1, 12)
  T11.04_start_day_check <- check_1or2digit_within_range(drip_iso_sample_data$drip_iso_start_dd, 1, 31)
  T11.05_start_hhmm_check <- check_hhmm_within_range(drip_iso_sample_data$drip_iso_start_hhmm, 1, 2359)
  
  # Checking 'drip_iso_end_yyyy', 'drip_iso_end_mm', 'drip_iso_end_dd', 'drip_iso_end_hhmm'
  T11.06_end_year_check <- check_4digit_within_range(drip_iso_sample_data$drip_iso_end_yyyy, 1800, 2024)
  T11.07_end_month_check <- check_1or2digit_within_range(drip_iso_sample_data$drip_iso_end_mm, 1, 12)
  T11.08_end_day_check <- check_1or2digit_within_range(drip_iso_sample_data$drip_iso_end_dd, 1, 31)
  T11.09_end_hhmm_check <- check_hhmm_within_range(drip_iso_sample_data$drip_iso_end_hhmm, 1, 2359)
  
  # Check if numeric columns contain numbers excluding NA
  numeric_check_excluding_na <- function(x) {
    all(is.na(x) | !is.na(as.numeric(x)))
  }
  
  T11.10_accumulation_time_check <- numeric_check_excluding_na(drip_iso_sample_data$drip_iso_accumulation_time)
  T11.11_d18O_measurement_check <- numeric_check_excluding_na(drip_iso_sample_data$drip_d18O_measurement)
  T11.12_d18O_precision_check <- numeric_check_excluding_na(drip_iso_sample_data$drip_d18O_precision)
  T11.13_d2H_measurement_check <- numeric_check_excluding_na(drip_iso_sample_data$drip_d2H_measurement)
  T11.14_d2H_precision_check <- numeric_check_excluding_na(drip_iso_sample_data$drip_d2H_precision)
  
  # Print results
  cat("Accumulation Unit Check: ", T11.01_accumulation_unit_check, "\n")
  cat("Start Year Check: ", T11.02_start_year_check, "\n")
  cat("Start Month Check: ", T11.03_start_month_check, "\n")
  cat("Start Day Check: ", T11.04_start_day_check, "\n")
  cat("Start HHMM Check: ", T11.05_start_hhmm_check, "\n")
  cat("End Year Check: ", T11.06_end_year_check, "\n")
  cat("End Month Check: ", T11.07_end_month_check, "\n")
  cat("End Day Check: ", T11.08_end_day_check, "\n")
  cat("End HHMM Check: ", T11.09_end_hhmm_check, "\n")
  cat("Accumulation Time Check: ", T11.10_accumulation_time_check, "\n")
  cat("d18O Measurement Check: ", T11.11_d18O_measurement_check, "\n")
  cat("d18O Precision Check: ", T11.12_d18O_precision_check, "\n")
  cat("d2H Measurement Check: ", T11.13_d2H_measurement_check, "\n")
  cat("d2H Precision Check: ", T11.14_d2H_precision_check, "\n")
  
  # Summarize all checks
  T11_all_checks <- T11.01_accumulation_unit_check & T11.02_start_year_check & T11.03_start_month_check & T11.04_start_day_check & T11.05_start_hhmm_check & 
    T11.06_end_year_check & T11.07_end_month_check & T11.08_end_day_check & T11.09_end_hhmm_check & 
    T11.10_accumulation_time_check & T11.11_d18O_measurement_check & T11.12_d18O_precision_check & 
    T11.13_d2H_measurement_check & T11.14_d2H_precision_check
  
  T11_all_checks <- paste("Drip iso sample checks passed:",T11_all_checks)
  
  cat("All checks passed: ", T11_all_checks, "\n")
  
} else {
  T11_all_checks <- paste("The", sheet_name, "table contains no data.")
  cat("All checks passed: FAILED | The", sheet_name, "table contains no data.", "\n")
}

################################################################################################################
################################################################################################################
################################################################################################################

#T12 drip_rate_sample

sheet_name <- "drip_rate_sample"

drip_rate_sample_data <- read.xlsx(file_name, sheet = sheet_name, startRow = 2, colNames = TRUE)

if (dim(drip_rate_sample_data)[1] != 0){
  
  # Function to check 4 digit whole number within a range
  check_4digit_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{4}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Function to check 1 or 2 digit whole number within a range
  check_1or2digit_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{1,2}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Function to check 4 digit whole number within hhmm range
  check_hhmm_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{4}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Check if 'drip_rate_accumulation_unit' is valid
  valid_units <- c("days", "hours", "minutes", "seconds", "unknown")
  T12.01_accumulation_unit_check <- all(is.na(drip_rate_sample_data$`drip_rate_accumulation_unit`) | 
                                          drip_rate_sample_data$`drip_rate_accumulation_unit` %in% valid_units)
  
  # Checking 'drip_rate_start_yyyy', 'drip_rate_start_mm', 'drip_rate_start_dd', 'drip_rate_start_hhmm'
  T12.02_start_year_check <- check_4digit_within_range(drip_rate_sample_data$drip_rate_start_yyyy, 1800, 2024)
  T12.03_start_month_check <- check_1or2digit_within_range(drip_rate_sample_data$drip_rate_start_mm, 1, 12)
  T12.04_start_day_check <- check_1or2digit_within_range(drip_rate_sample_data$drip_rate_start_dd, 1, 31)
  T12.05_start_hhmm_check <- check_hhmm_within_range(drip_rate_sample_data$drip_rate_start_hhmm, 1, 2359)
  
  # Checking 'drip_rate_end_yyyy', 'drip_rate_end_mm', 'drip_rate_end_dd', 'drip_rate_end_hhmm'
  T12.06_end_year_check <- check_4digit_within_range(drip_rate_sample_data$drip_rate_end_yyyy, 1800, 2024)
  T12.07_end_month_check <- check_1or2digit_within_range(drip_rate_sample_data$drip_rate_end_mm, 1, 12)
  T12.08_end_day_check <- check_1or2digit_within_range(drip_rate_sample_data$drip_rate_end_dd, 1, 31)
  T12.09_end_hhmm_check <- check_hhmm_within_range(drip_rate_sample_data$drip_rate_end_hhmm, 1, 2359)
  
  # Check if numeric columns contain numbers excluding NA
  numeric_check_excluding_na <- function(x) {
    all(is.na(x) | !is.na(as.numeric(x)))
  }
  
  T12.10_accumulation_time_check <- numeric_check_excluding_na(drip_rate_sample_data$drip_rate_accumulation_time)
  T12.11_measurement_check <- numeric_check_excluding_na(drip_rate_sample_data$drip_rate_measurement)
  T12.12_precision_check <- numeric_check_excluding_na(drip_rate_sample_data$drip_rate_precision)
  
  # Print results
  cat("Accumulation Unit Check: ", T12.01_accumulation_unit_check, "\n")
  cat("Start Year Check: ", T12.02_start_year_check, "\n")
  cat("Start Month Check: ", T12.03_start_month_check, "\n")
  cat("Start Day Check: ", T12.04_start_day_check, "\n")
  cat("Start HHMM Check: ", T12.05_start_hhmm_check, "\n")
  cat("End Year Check: ", T12.06_end_year_check, "\n")
  cat("End Month Check: ", T12.07_end_month_check, "\n")
  cat("End Day Check: ", T12.08_end_day_check, "\n")
  cat("End HHMM Check: ", T12.09_end_hhmm_check, "\n")
  cat("Accumulation Time Check: ", T12.10_accumulation_time_check, "\n")
  cat("Measurement Check: ", T12.11_measurement_check, "\n")
  cat("Precision Check: ", T12.12_precision_check, "\n")
  
  # Summarize all checks
  T12_all_checks <- T12.01_accumulation_unit_check & T12.02_start_year_check & T12.03_start_month_check & T12.04_start_day_check & T12.05_start_hhmm_check & 
    T12.06_end_year_check & T12.07_end_month_check & T12.08_end_day_check & T12.09_end_hhmm_check & 
    T12.10_accumulation_time_check & T12.11_measurement_check & T12.12_precision_check
  
  T12_all_checks <- paste("Drip rate sample checks passed:",T12_all_checks)
  
  cat("All checks passed: ", T12_all_checks, "\n")
  
} else {
  T12_all_checks <- paste("The", sheet_name, "table contains no data.")
  cat("All checks passed: FAILED | The", sheet_name, "table contains no data.", "\n")
}



################################################################################################################
################################################################################################################
################################################################################################################

#T13 mod_carb_sample

sheet_name <- "mod_carb_sample"
mod_carb_sample_data <- read.xlsx(file_name, sheet = sheet_name, startRow = 2, colNames = TRUE)

if (dim(mod_carb_sample_data)[1] != 0){
  
  # Function to check 4 digit whole number within a range
  check_4digit_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{4}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Function to check 1 or 2 digit whole number within a range
  check_1or2digit_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{1,2}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Function to check 4 digit whole number within hhmm range
  check_hhmm_within_range <- function(x, min_val, max_val) {
    all(is.na(x) | (grepl("^\\d{4}$", x) & as.numeric(x) >= min_val & as.numeric(x) <= max_val))
  }
  
  # Check if 'mod_carb_accumulation_unit' is valid
  valid_units <- c("years", "months", "days", "unknown")
  T13.01_accumulation_unit_check <- all(is.na(mod_carb_sample_data$mod_carb_accumulation_unit) | 
                                          mod_carb_sample_data$mod_carb_accumulation_unit %in% valid_units)
  
  # Check if 'mod_carb_surface' is valid
  valid_surfaces <- c("stalagmite", "stalagmite scar", "glass plate", "other (see notes)", "unknown")
  T13.01_surface_check <- all(is.na(mod_carb_sample_data$mod_carb_surface) | 
                                mod_carb_sample_data$mod_carb_surface %in% valid_surfaces)
  
  # Check if 'mod_carb_mineralogy' is valid
  valid_mineralogy <- c("calcite", "aragonite", "mixed (see notes)", "other (see notes)", "unknown")
  T13.01_mineralogy_check <- all(is.na(mod_carb_sample_data$mod_carb_mineralogy) | 
                                   mod_carb_sample_data$mod_carb_mineralogy %in% valid_mineralogy)
  
  # Checking 'mod_carb_start_yyyy', 'mod_carb_start_mm', 'mod_carb_start_dd', 'mod_carb_start_hhmm'
  T13.02_start_year_check <- check_4digit_within_range(mod_carb_sample_data$mod_carb_start_yyyy, 1800, 2024)
  T13.03_start_month_check <- check_1or2digit_within_range(mod_carb_sample_data$mod_carb_start_mm, 1, 12)
  T13.04_start_day_check <- check_1or2digit_within_range(mod_carb_sample_data$mod_carb_start_dd, 1, 31)
  T13.05_start_hhmm_check <- check_hhmm_within_range(mod_carb_sample_data$mod_carb_start_hhmm, 1, 2359)
  
  # Checking 'mod_carb_end_yyyy', 'mod_carb_end_mm', 'mod_carb_end_dd', 'mod_carb_end_hhmm'
  T13.06_end_year_check <- check_4digit_within_range(mod_carb_sample_data$mod_carb_end_yyyy, 1800, 2024)
  T13.07_end_month_check <- check_1or2digit_within_range(mod_carb_sample_data$mod_carb_end_mm, 1, 12)
  T13.08_end_day_check <- check_1or2digit_within_range(mod_carb_sample_data$mod_carb_end_dd, 1, 31)
  T13.09_end_hhmm_check <- check_hhmm_within_range(mod_carb_sample_data$mod_carb_end_hhmm, 1, 2359)
  
  # Check if numeric columns contain numbers excluding NA
  numeric_check_excluding_na <- function(x) {
    all(is.na(x) | !is.na(as.numeric(x)))
  }
  
  T13.10_accumulation_time_check <- numeric_check_excluding_na(mod_carb_sample_data$mod_carb_accumulation_time)
  T13.11_d18O_measurement_check <- numeric_check_excluding_na(mod_carb_sample_data$mod_carb_d18O_measurement)
  T13.12_d18O_precision_check <- numeric_check_excluding_na(mod_carb_sample_data$mod_carb_d18O_precision)
  T13.13_d2H_measurement_check <- numeric_check_excluding_na(mod_carb_sample_data$mod_carb_d2H_measurement)
  T13.14_d2H_precision_check <- numeric_check_excluding_na(mod_carb_sample_data$mod_carb_d2H_precision)
  
  # Print results
  cat("Accumulation Unit Check: ", T13.01_accumulation_unit_check, "\n")
  cat("Surface Check: ", T13.01_surface_check, "\n")
  cat("Mineralogy Check: ", T13.01_mineralogy_check, "\n")
  cat("Start Year Check: ", T13.02_start_year_check, "\n")
  cat("Start Month Check: ", T13.03_start_month_check, "\n")
  cat("Start Day Check: ", T13.04_start_day_check, "\n")
  cat("Start HHMM Check: ", T13.05_start_hhmm_check, "\n")
  cat("End Year Check: ", T13.06_end_year_check, "\n")
  cat("End Month Check: ", T13.07_end_month_check, "\n")
  cat("End Day Check: ", T13.08_end_day_check, "\n")
  cat("End HHMM Check: ", T13.09_end_hhmm_check, "\n")
  cat("Accumulation Time Check: ", T13.10_accumulation_time_check, "\n")
  cat("d18O Measurement Check: ", T13.11_d18O_measurement_check, "\n")
  cat("d18O Precision Check: ", T13.12_d18O_precision_check, "\n")
  cat("d2H Measurement Check: ", T13.13_d2H_measurement_check, "\n")
  cat("d2H Precision Check: ", T13.14_d2H_precision_check, "\n")
  
  # Summarize all checks
  T13_all_checks <- T13.01_accumulation_unit_check & T13.01_surface_check & T13.01_mineralogy_check & T13.02_start_year_check & T13.03_start_month_check & 
    T13.04_start_day_check & T13.05_start_hhmm_check & T13.06_end_year_check & T13.07_end_month_check & T13.08_end_day_check & T13.09_end_hhmm_check & 
    T13.10_accumulation_time_check & T13.11_d18O_measurement_check & T13.12_d18O_precision_check & 
    T13.13_d2H_measurement_check & T13.14_d2H_precision_check
  
  T13_all_checks <- paste("Mod carb sample checks passed:",T13_all_checks)
  
  cat("All checks passed: ", T13_all_checks, "\n")
  
} else {
  T13_all_checks <- paste("The", sheet_name, "table contains no data.")
  cat("All checks passed: FAILED | The", sheet_name, "table contains no data.", "\n")
}

################################################################################################################
################################################################################################################
################################################################################################################

# T14 Checks across cave entity (table integrity)

sheet_name <- "cave_entity_metadata"
cave_entity_data <- read.xlsx(file_name, sheet = sheet_name, startRow = 2, colNames = TRUE)



# 1. If there is a yes in the cave_entity_metadata worksheet cave_temperature column, the corresponding cave_entity_name must be shown in the cave_temperature_sample worksheet.
#sheet_name <- "cave_temperature_sample"
#cave_temperature_sample_data <- read.xlsx(file_name, sheet = sheet_name, startRow = 2, colNames = TRUE)
#cave_entity_name_temperature <- cave_entity_data[toupper(cave_entity_data$cave_temperature) == "YES", "cave_entity_name"]
#T14.01_cave_entity_name_temperature_check <- all(is.na(cave_temperature_sample_data$cave_entity_name) | 
#                                            cave_temperature_sample_data$cave_entity_name %in% cave_entity_name_temperature)

# 2. If there is a yes in the cave_entity_metadata worksheet cave_relative_humidity column, the corresponding cave_entity_name must be shown in the cave_realtive_humidity_sample worksheet. 
#sheet_name <- "cave_relative_humidity_sample"
#cave_relative_humidity_sample_data <- read.xlsx(file_name, sheet = sheet_name, startRow = 2, colNames = TRUE)
#cave_entity_name_relative_humidity <- cave_entity_data[toupper(cave_entity_data$cave_relative_humidity) == "YES", "cave_entity_name"]
#T14.02_cave_entity_name_relative_humidity_check <- all(is.na(cave_relative_humidity_sample_data$cave_entity_name) | 
#                                                  cave_relative_humidity_sample_data$cave_entity_name %in% cave_entity_name_relative_humidity)

# 3. If there is a yes in the cave_entity_metadata worksheet cave_pCO2 column, the corresponding cave_entity_name must be shown in the cave_pCO2_sample worksheet.
#sheet_name <- "cave_pCO2_sample"
#cave_pCO2_sample_data <- read.xlsx(file_name, sheet = sheet_name, startRow = 2, colNames = TRUE)
#cave_entity_name_pCO2 <- cave_entity_data[toupper(cave_entity_data$cave_pCO2) == "YES", "cave_entity_name"]
#T14.03_cave_entity_name_pCO2_check <- all(is.na(cave_pCO2_sample_data$cave_entity_name) | 
#                                     cave_pCO2_sample_data$cave_entity_name %in% cave_entity_name_pCO2)

# Print results
#cat("Cave entity name temperature check: ", T14.01_cave_entity_name_temperature_check, "\n")
#cat("Cave entity name relative humidity check: ", T14.02_cave_entity_name_relative_humidity_check, "\n")
#cat("Cave entity name pCO2 check: ", T14.03_cave_entity_name_pCO2_check, "\n")

# Summarize all checks
#T14_all_checks <- T14.01_cave_entity_name_temperature_check & T14.02_cave_entity_name_relative_humidity_check & T14.03_cave_entity_name_pCO2_check

#T14_all_checks <- paste("Checks across cave entity (table integrity) passed:",T14_all_checks)

#cat("All checks passed: ", T14_all_checks, "\n")

################################################################################################################
################################################################################################################
################################################################################################################


# T15 Checks across tables (table integrity)

# 1. Check 'site_name' matches across 'reference', 'notes', 'precip_site_metadata', and 'site'
site_names <- unique(site_data$site_name)
reference_site_names <- unique(reference_data$site_name)
notes_site_names <- unique(notes_data$site_name[notes_data$site_name != ""])  # Exclude empty values
precip_site_names <- unique(precip_site_metadata_data$site_name)

T15.01_site_name_check <- all(reference_site_names %in% site_names) &
  all(notes_site_names %in% site_names) &
  all(precip_site_names %in% site_names)

# 2. Check 'precip_entity_name' matches across 'precip_entity_metadata', 'precip_sample', and 'precip_site_metadata'
precip_site_entity_names <- unique(precip_site_metadata_data$precip_entity_name)
precip_entity_metadata_names <- unique(precip_entity_metadata_data$precip_entity_name)
precip_sample_entity_names <- unique(precip_sample_data$precip_entity_name)

T15.02_precip_entity_name_check <- all(precip_entity_metadata_names %in% precip_site_entity_names) &
  all(precip_sample_entity_names %in% precip_site_entity_names)

# 3. Check 'cave_entity_name' in 'cave_entity_metadata' exists in 'cave_temperature_sample', 'cave_relative_humidity_sample', 'cave_pCO2_sample'
cave_entity_metadata_names <- unique(cave_entity_metadata_data$cave_entity_name)
cave_temperature_names <- unique(cave_temperature_sample_data$cave_entity_name)
cave_relative_humidity_names <- unique(cave_relative_humidity_sample_data$cave_entity_name)
cave_pCO2_names <- unique(cave_pCO2_sample_data$cave_entity_name)

T15.03_cave_entity_name_check <- all(cave_entity_metadata_names %in% c(cave_temperature_names, cave_relative_humidity_names, cave_pCO2_names))

# 4. Check 'drip_entity_name' in 'drip_entity_metadata' exists in 'drip_iso_sample', 'drip_rate_sample', 'mod_carb_sample'
drip_entity_metadata_names <- unique(drip_entity_metadata_data$drip_entity_name)
drip_iso_names <- unique(drip_iso_sample_data$drip_entity_name)
drip_rate_names <- unique(drip_rate_sample_data$drip_entity_name)
mod_carb_names <- unique(mod_carb_sample_data$drip_entity_name)

T15.04_drip_entity_name_check <- all(drip_entity_metadata_names %in% c(drip_iso_names, drip_rate_names, mod_carb_names))

# 5. Check there is at least one row with information in the 'reference' worksheet
T15.05_reference_non_empty_check <- nrow(reference_data) > 0

# Print results
cat("Site Name Check: ", T15.01_site_name_check, "\n")
cat("Precip Entity Name Check: ", T15.02_precip_entity_name_check, "\n")
cat("Cave Entity Name Check: ", T15.03_cave_entity_name_check, "\n")
cat("Drip Entity Name Check: ", T15.04_drip_entity_name_check, "\n")
cat("Reference Non-Empty Check: ", T15.05_reference_non_empty_check, "\n")

# Summarize all checks
T15_all_checks <- T15.01_site_name_check & T15.02_precip_entity_name_check & T15.03_cave_entity_name_check & T15.04_drip_entity_name_check & T15.05_reference_non_empty_check

cat("All checks passed: ", T15_all_checks, "\n")

############################### PLOTTING ######################################################
# text output
write_vars_to_pdf <- function(name_vector, cave_name = "cave_name", file_name = "output.pdf", max_lines_per_page = 40) {
  # Open a PDF device
  pdf(file = file_name)
  
  # Set up initial plot layout and text parameters
  par(mar = c(1, 1, 1, 1))  # Minimal margins
  
  # Initialize line count and page number
  line_count <- 3
  page_number <- 1
  
  
  # Start 1st page
  plot.new()
  text(0, 1 - (0 / max_lines_per_page), labels = paste("Monitoring db auto qc output document < PAGE",page_number,">"), pos = 4, cex = 1.1, adj = c(0, 1),col="blue")
  text(0, 1 - (1 / max_lines_per_page), labels = cave_name, pos = 4, cex = 1.1, adj = c(0, 1),col="blue")
  text(0, 1 - (2 / max_lines_per_page), labels = "", pos = 4, cex = 1.0, adj = c(0, 1))
  
  # Loop through each variable name in the name_vector
  for (var_name in name_vector) {
    
    # Start a new page if the line count exceeds max_lines_per_page
    if (line_count >= max_lines_per_page) {
      plot.new()
      line_count <- 3
      
      # Increment page number
      page_number <- page_number + 1
      
      text(0, 1 - (0 / max_lines_per_page), labels = paste("Monitoring db auto qc output document < PAGE",page_number,">"), pos = 4, cex = 1.1, adj = c(0, 1),col="blue")
      text(0, 1 - (1 / max_lines_per_page), labels = cave_name, pos = 4, cex = 1.1, adj = c(0, 1),col="blue")
      text(0, 1 - (2 / max_lines_per_page), labels = "", pos = 4, cex = 1.0, adj = c(0, 1))
    }
    
    
    # Check if the variable exists in the environment
    if (exists(var_name, envir = .GlobalEnv)) {
      # Get the value of the variable
      var_value <- get(var_name, envir = .GlobalEnv)
      
      # Check if the value is not a function (closure)
      if (!is.function(var_value)) {
        # Convert non-text values to text
        if (var_value == TRUE) {
          var_value <- "PASSED"
        } else if (var_value == FALSE) {
          var_value <- "FAILED"
        } else {var_value}
        
        # Create the text to display
        line_text <- paste(var_name, ":", var_value)
        
        # Plot the text
        if (var_value == "FAILED" || grepl("FALSE",var_value)) {
          text(0, 1 - (line_count / max_lines_per_page), labels = line_text, pos = 4, cex = 1, adj = c(0, 1),col="red")
        } else {
          text(0, 1 - (line_count / max_lines_per_page), labels = line_text, pos = 4, cex = 1, adj = c(0, 1))
        }
        
        
        # Increment line count
        line_count <- line_count + 1
        
      } else {
        cat("Skipping", var_name, "- it is a function, not a variable.\n")
      }
    } else {
      cat("Variable", var_name, "does not exist in the environment.\n")
    }
    
    
    
  }
  
  # Close the PDF device
  dev.off()
  
  # Confirmation message
  cat("Variable values have been written to", file_name, "\n")
}

# variables
text_variable <- ls()

# Define the check string
chck_string <- "check"

# Extract elements containing the check string using grep()
matching_elements <- grep(chck_string, text_variable, value = TRUE)

write_vars_to_pdf(matching_elements, file_name, paste("text_output_",cave_name,".pdf",sep=""))

##################x
# graphics outputs



library(ggplot2)

#date vector
concat_date <- function(yyyy, mm, dd) {
  # Replace missing month or day with defaults (mid-month, mid-day)
  mm[is.na(mm)] <- 6
  dd[is.na(dd)] <- 15
  as.POSIXct(paste(yyyy, mm, dd), format = "%Y %m %d", tz = "UTC")
}


mod_carb_sample_data$date <- concat_date(mod_carb_sample_data$mod_carb_start_yyyy, mod_carb_sample_data$mod_carb_start_mm, mod_carb_sample_data$mod_carb_start_dd)

# Filter out rows with NA in the date
mod_carb_sample_data <- mod_carb_sample_data[!is.na(mod_carb_sample_data$date), ]


# Create the PDF
pdf_path <- paste("graphics_output_",cave_name,".pdf",sep="")
pdf(pdf_path)

# -----------------------------------------------------
# Plot 1.1: “site” and “precip_site” coordinates on map for visual inspection (Overview)
# Load data
site_data <- read.xlsx(file_name, sheet = "site", startRow = 2, colNames = TRUE)
precip_site_metadata <- read.xlsx(file_name, sheet = "precip_site_metadata", startRow = 2, colNames = TRUE)

# Create data frames for point A and point B
point_A <- data.frame(lon = site_data$longitude, lat = site_data$latitude, name = site_data$site_name)
point_B <- data.frame(lon = precip_site_metadata$precip_longitude, lat = precip_site_metadata$precip_latitude, name = precip_site_metadata$precip_entity_name)

# Combine the points into one data frame
points <- rbind(point_A, point_B)

# Function to plot the world map
fig_map_world <- function (points, scale){  
  world_map <- map_data("world")
  ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightblue", color = "black") +
    geom_point(data = points, aes(x = lon, y = lat), color = "red", size = 3) +
    geom_text(data = points, aes(x = lon, y = lat, label = name), vjust = -1, hjust = 0.5, color = "black") +
    coord_fixed(ratio = 1.3, xlim = scale[1:2], ylim = scale[3:4]) +
    ggtitle("Site and Precipitation Location on the World Map") +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_minimal()
}

# Plot the global map
fig_map_world(points, c(-180, 180, -90, 90))



#Plot 1.2: “site” and “precip_site” coordinates on map for visual inspection (Zoomed in)
# Load data
# Load data
site_data <- read.xlsx(file_name, sheet = "site", startRow = 2, colNames = TRUE)
precip_site_metadata <- read.xlsx(file_name, sheet = "precip_site_metadata", startRow = 2, colNames = TRUE)

# Create data frames for point A and point B
point_A <- data.frame(lon = site_data$longitude, lat = site_data$latitude, name = site_data$site_name)
point_B <- data.frame(lon = precip_site_metadata$precip_longitude, lat = precip_site_metadata$precip_latitude, name = precip_site_metadata$precip_entity_name)

# Combine the points into one data frame
points <- rbind(point_A, point_B)

# Function to plot the zoomed-in map with modified latitude and longitude ranges
fig_map_zoomed <- function (points){  
  world_map <- map_data("world")
  
  # Adjust the longitude and latitude by ±5 degrees 
  lon_range <- range(points$lon) + c(-5, 5)
  lat_range <- range(points$lat) + c(-5, 5)
  
  ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightblue", color = "black") +
    geom_point(data = points, aes(x = lon, y = lat), color = "red", size = 3) +
    geom_text(data = points, aes(x = lon, y = lat, label = name), vjust = -1, hjust = 0.5, color = "black") +
    coord_fixed(ratio = 1.3, xlim = lon_range, ylim = lat_range) +
    ggtitle("Zoomed-In Map of Site and Precipitation Locations") +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_minimal()
}

# Plot the zoomed-in map with adjusted ranges
fig_map_zoomed(points)

# -----------------------------------------------------
# Plot 2: “precip_amount” versus “precip_accumulation_time” with “precip_accumulation_unit” on a bar graph

ggplot(precip_sample_data, aes(x = precip_accumulation_time, y = precip_amount, color = precip_accumulation_unit)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "Precipitation Amount vs Accumulation Time",
       x = "Precipitation Accumulation Time",
       y = "Precipitation Amount",
       color = "Accumulation Unit") +
  theme_minimal()

if(dim(precip_sample_data)[1] == 0){
  plot.new()
  text(0, 1, labels = paste("No data available on table precip_sample_data"), pos = 4, cex = 1.1, adj = c(0, 1),col="black")
} else {
  date <- concat_date(precip_sample_data$precip_start_yyyy,precip_sample_data$precip_start_mm,precip_sample_data$precip_start_dd)
  
  ggplot(precip_sample_data, aes(x = date, y = precip_amount, color = precip_entity_name)) +
    geom_line() +    # Scatter points   
    geom_point() +
    labs(title = "Plot precip_amount vs precip_start_date",
         x = "Date", 
         y = "precip_amount") +
    theme_minimal()              
}

#___________________________________________________________
# OLD
#ggplot(precip_sample_data, aes(x = precip_accumulation_time, y = precip_amount, fill = precip_accumulation_unit)) +
#  geom_bar(stat = "identity", position = "dodge") +
#  labs(title = "Precipitation Amount vs Accumulation Time",
#       x = "Precipitation Accumulation Time",
#       y = "Precipitation Amount",
#       fill = "Accumulation Unit") +
#  theme_minimal()
#___________________________________________________________

# -----------------------------------------------------
# Plot 3: “precip_d18O_measurement” versus start-end day and time on a scatter plot
#Itt új változót tesz az eredeti táblához, tehát lehet ,hogy külön kéne elmenteni. 

#Plot 3 és 4: “precip_d18O_measurement” versus start-end day and time on a scatter plot. Wrongly entered precip d18O measurements can be identified here

#___________________________________________________________
#OLD
# Combine the date and time components into start and end date-time objects
#precip_sample_data$start_datetime <- as.POSIXct(
#  paste(precip_sample_data$precip_start_yyyy, precip_sample_data$precip_start_mm, precip_sample_data$precip_start_dd, precip_sample_data$precip_start_hhmm),
#  format = "%Y %m %d %H%M"
#)
#
#precip_sample_data$end_datetime <- as.POSIXct(
#  paste(precip_sample_data$precip_end_yyyy, precip_sample_data$precip_end_mm, precip_sample_data$precip_end_dd, precip_sample_data$precip_end_hhmm),
#  format = "%Y %m %d %H%M"
#)
# Calculate the difference in hours between start and end datetime
#precip_sample_data$precip_duration_hours <- as.numeric(difftime(precip_sample_data$end_datetime, precip_sample_data$start_datetime, units = "hours"))
#
#
#ggplot(precip_sample_data, aes(x = precip_duration_hours)) +
#  geom_point(aes(y = precip_d18O_measurement), color = "red") +
#  geom_point(aes(y = precip_d2H_measurement / 8), color = "darkblue") +  # Scale down d2H for visualization purposes
#  scale_y_continuous(
#    name = "Precipitation d18O Measurement",
#    sec.axis = sec_axis(~ . * 8, name = "Precipitation d2H Measurement")  # Scale back up d2H to original scale
#  ) +
#  labs(
#    title = "Precipitation d18O and d2H Measurements vs Precipitation Duration",
#    x = "Precipitation Duration (hours)"
#  ) +
#  theme_minimal()
#________________________________________________________


library(ggplot2)
library(broom)

# date helper
concat_date <- function(yyyy, mm, dd) {
  mm[is.na(mm)] <- 6; dd[is.na(dd)] <- 15
  as.POSIXct(paste(yyyy, mm, dd), format = "%Y %m %d", tz = "UTC")
}

# … [mod_carb date and PDF setup] …

# -----------------------------------------------------
# Precip & drip d18O vs d2H Measurement
# -----------------------------------------------------

# count valid (non‐NA) pairs in each dataset
n_precip <- sum(!is.na(precip_sample_data$precip_d18O_measurement) &
                  !is.na(precip_sample_data$precip_d2H_measurement))
n_drip   <- sum(!is.na(drip_iso_sample_data$drip_d18O_measurement) &
                  !is.na(drip_iso_sample_data$drip_d2H_measurement))

if (n_precip >= 2 && n_drip >= 2) {
  
  precip_df <- data.frame(
    d18O = precip_sample_data$precip_d18O_measurement,
    d2H  = precip_sample_data$precip_d2H_measurement,
    group = "Precipitation"
  )
  drip_df <- data.frame(
    d18O = drip_iso_sample_data$drip_d18O_measurement,
    d2H  = drip_iso_sample_data$drip_d2H_measurement,
    group = "Drip Isotope"
  )
  combined_data <- na.omit(rbind(precip_df, drip_df))
  
  # fit both models
  mod_p <- lm(d2H ~ d18O, data = subset(combined_data, group=="Precipitation"))
  mod_d <- lm(d2H ~ d18O, data = subset(combined_data, group=="Drip Isotope"))
  eq_p <- with(coef(mod_p), sprintf("Precip: y=%.2fx+%.2f, R²=%.2f", d18O, `(Intercept)`, summary(mod_p)$r.squared))
  eq_d <- with(coef(mod_d), sprintf("Drip:   y=%.2fx+%.2f, R²=%.2f", d18O, `(Intercept)`, summary(mod_d)$r.squared))
  
  ggplot(combined_data, aes(x=d18O, y=d2H, color=group)) +
    geom_point() +
    geom_smooth(method="lm", se=FALSE) +
    geom_abline(intercept=10, slope=8, linetype="dashed") +
    annotate("text", x=min(combined_data$d18O), y= max(combined_data$d2H)*1.05, label=eq_p, hjust=0, color="blue") +
    annotate("text", x=min(combined_data$d18O), y= max(combined_data$d2H)*1.00, label=eq_d, hjust=0, color="red") +
    annotate("text", x=max(combined_data$d18O)*1.1, y=max(combined_data$d2H)*1.05,
             label="GMWL (y=8x+10)", hjust=0, color="black") +
    labs(title="d18O vs d2H with Linear Fits & GMWL") +
    theme_minimal()
  
} else if (n_precip >= 2) {
  
  # only precipitation
  precip_df <- na.omit(data.frame(
    d18O = precip_sample_data$precip_d18O_measurement,
    d2H  = precip_sample_data$precip_d2H_measurement
  ))
  mod_p <- lm(d2H ~ d18O, data=precip_df)
  eq_p <- with(coef(mod_p), sprintf("y=%.2fx+%.2f, R²=%.2f",
                                    d18O, `(Intercept)`, summary(mod_p)$r.squared))
  
  ggplot(precip_df, aes(x=d18O, y=d2H)) +
    geom_point(color="blue") +
    geom_smooth(method="lm", se=FALSE, color="blue") +
    geom_abline(intercept=10, slope=8, linetype="dashed") +
    annotate("text", x=min(precip_df$d18O), y=max(precip_df$d2H)*1.05,
             label=eq_p, hjust=0, color="blue") +
    annotate("text", x=max(precip_df$d18O)*1.1, y=max(precip_df$d2H)*1.05,
             label="GMWL (y=8x+10)", hjust=0) +
    labs(title="Precipitation d18O vs d2H") +
    theme_minimal()
  
} else if (n_drip >= 2) {
  
  # only drip
  drip_df <- na.omit(data.frame(
    d18O = drip_iso_sample_data$drip_d18O_measurement,
    d2H  = drip_iso_sample_data$drip_d2H_measurement
  ))
  mod_d <- lm(d2H ~ d18O, data=drip_df)
  eq_d <- with(coef(mod_d), sprintf("y=%.2fx+%.2f, R²=%.2f",
                                    d18O, `(Intercept)`, summary(mod_d)$r.squared))
  
  ggplot(drip_df, aes(x=d18O, y=d2H)) +
    geom_point(color="red") +
    geom_smooth(method="lm", se=FALSE, color="red") +
    geom_abline(intercept=10, slope=8, linetype="dashed") +
    annotate("text", x=min(drip_df$d18O), y=max(drip_df$d2H)*1.05,
             label=eq_d, hjust=0, color="red") +
    annotate("text", x=max(drip_df$d18O)*1.1, y=max(drip_df$d2H)*1.05,
             label="GMWL (y=8x+10)", hjust=0) +
    labs(title="Drip Isotope d18O vs d2H") +
    theme_minimal()
  
} else {
  
  plot.new()
  text(0.5, 0.5,
       "Not enough non-NA d18O & d2H pairs to fit a regression",
       cex=1.2, col="red", adj=0.5)
}







###########################################################


# Plot 4: “precip_d2H_measurement” versus start-end day and time on a scatter plot

#________________________________________________________
#OLD
#p4 <- ggplot() +
#  geom_point(data = precip_sample_data, aes(x = precip_start_dd, y = precip_d2H_measurement), color = "blue", alpha = 0.6) +
#  geom_point(data = precip_sample_data, aes(x = precip_end_dd, y = precip_d2H_measurement), color = "red", alpha = 0.6) +
#  labs(title = "Precip d2H Measurement vs Start-End Day", x = "Day", y = "Precip d2H Measurement") +
#  theme_minimal()
#print(p4)
#________________________________________________________

# Plot precip_d18O_measurement vs percip_start_date
# Plot precip_d2H_measurement vs percip_start_date

if(dim(precip_sample_data)[1] == 0){
  plot.new()
  text(0, 1, labels = paste("No data available on table precip_sample_data"), pos = 4, cex = 1.1, adj = c(0, 1),col="black")
} else {
  date <- concat_date(precip_sample_data$precip_start_yyyy,precip_sample_data$precip_start_mm,precip_sample_data$precip_start_dd)
  
  ggplot(precip_sample_data, aes(x = date, y = precip_d18O_measurement, color = precip_entity_name)) +
    geom_line() +    # Scatter points   
    geom_point() +
    labs(title = "Plot precip_d18O_measurement vs precip_start_date",
         x = "Date", 
         y = "d18O Measurement") +
    theme_minimal()              
}


if(dim(precip_sample_data)[1] == 0){
  plot.new()
  text(0, 1, labels = paste("No data available on table precip_sample_data"), pos = 4, cex = 1.1, adj = c(0, 1),col="black")
} else {
  date <- concat_date(precip_sample_data$precip_start_yyyy,precip_sample_data$precip_start_mm,precip_sample_data$precip_start_dd)
  
  ggplot(precip_sample_data, aes(x = date, y = precip_d2H_measurement, color = precip_entity_name)) +
    geom_line() +    # Scatter points   
    geom_point() +
    labs(title = "Plot precip_d2H_measurement vs precip_start_date",
         x = "date", 
         y = "d2H Measurement") +
    theme_minimal()                        
}





# “cave_temperature_measurement”

#________________________________________________________
#OLD
# Plot 5: “cave_temperature_measurement” versus day and time on a bar graph
#p5 <- ggplot(cave_temperature_sample_data, aes(x = cave_temperature_dd, y = cave_temperature_measurement)) +
#  geom_bar(stat = "identity", alpha = 0.6) +
#  labs(title = "Cave Temperature Measurement vs Day", x = "Day", y = "Cave Temperature Measurement") +
#  theme_minimal()
#print(p5)
#________________________________________________________

if(dim(cave_temperature_sample_data)[1] == 0){
  plot.new()
  text(0, 1, labels = paste("No data available on table cave_temperature_sample_data"), pos = 4, cex = 1.1, adj = c(0, 1),col="black")
} else {
  date <- concat_date(cave_temperature_sample_data$cave_temperature_yyyy,cave_temperature_sample_data$cave_temperature_mm,cave_temperature_sample_data$cave_temperature_dd)
  
  ggplot(cave_temperature_sample_data, aes(x = date, y = cave_temperature_measurement, color = cave_entity_name)) +
    geom_line() +    # Scatter points   
    geom_point() +
    labs(title = "Plot cave_temperature_measurement",
         x = "Date", 
         y = "cave_temperature_measurement") +
    theme_minimal()             
}


#________________________________________________________
#OLD
# Plot 6: “cave_relative_humidity_measurement” versus day and time on a bar graph
#p6 <- ggplot(cave_relative_humidity_sample_data, aes(x = cave_relative_humidity_dd, y = cave_relative_humidity_measurement)) +
#  geom_bar(stat = "identity", alpha = 0.6) +
#  labs(title = "Cave Relative Humidity Measurement vs Day", x = "Day", y = "Cave Relative Humidity Measurement") +
#  theme_minimal()
#print(p6)
#________________________________________________________
if(dim(cave_relative_humidity_sample_data)[1] == 0){
  plot.new()
  text(0, 1, labels = paste("No data available on table cave_relative_humidity_sample_data"), pos = 4, cex = 1.1, adj = c(0, 1),col="black")
} else {
  date <- concat_date(cave_relative_humidity_sample_data$cave_relative_humidity_yyyy,cave_relative_humidity_sample_data$cave_relative_humidity_mm,cave_relative_humidity_sample_data$cave_relative_humidity_dd)
  
  ggplot(cave_relative_humidity_sample_data, aes(x = date, y = cave_relative_humidity_measurement, color = cave_entity_name)) +
    geom_line() +    # Scatter points   
    geom_point() +
    labs(title = "Plot cave_relative_humidity_measurement",
         x = "Date", 
         y = "cave_relative_humidity_measurement") +
    theme_minimal()
}

#________________________________________________________
#OLD
# Plot 7: “cave_pCO2_measurement” versus day and time on a bar graph
# p7 <- ggplot(cave_pCO2_sample_data, aes(x = cave_pCO2_sample_dd, y = cave_pCO2_sample_measurement)) +
# geom_bar(stat = "identity", alpha = 0.6) +
#  labs(title = "Cave pCO2 Measurement vs Day", x = "Day", y = "Cave pCO2 Measurement") +
#  theme_minimal()
#print(p7)
#________________________________________________________
if(dim(cave_pCO2_sample_data)[1] == 0){
  plot.new()
  text(0, 1, labels = paste("No data available on table cave_pCO2_sample_data"), pos = 4, cex = 1.1, adj = c(0, 1),col="black")
} else {
  date <- concat_date(cave_pCO2_sample_data$cave_pCO2_yyyy,cave_pCO2_sample_data$cave_pCO2_mm,cave_pCO2_sample_data$cave_pCO2_dd)
  
  ggplot(cave_pCO2_sample_data, aes(x = date, y = cave_pCO2_measurement, color = cave_entity_name)) +
    geom_line() +    # Scatter points   
    geom_point() +
    labs(title = "Plot cave_pCO2_measurement",
         x = "Date", 
         y = "cave_pCO2_measurement") +
    theme_minimal()
}

#________________________________________________________
#OLD
# Plot 8: “drip_iso_d18O_measurement” versus start-end day and time on a scatter plot
#p8 <- ggplot() +
#  geom_point(data = drip_iso_sample_data, aes(x = drip_iso_start_dd, y = drip_d18O_measurement), color = "blue", alpha = 0.6) +
#  geom_point(data = drip_iso_sample_data, aes(x = drip_iso_end_dd, y = drip_d18O_measurement), color = "red", alpha = 0.6) +
#  labs(title = "Drip Iso d18O Measurement vs Start-End Day", x = "Day", y = "Drip Iso d18O Measurement") +
#  theme_minimal()
#print(p8)
#________________________________________________________

if(dim(drip_iso_sample_data)[1] == 0){
  plot.new()
  text(0, 1, labels = paste("No data available on table drip_iso_sample_data"), pos = 4, cex = 1.1, adj = c(0, 1),col="black")
} else {
  date <- concat_date(drip_iso_sample_data$drip_iso_start_yyyy,drip_iso_sample_data$drip_iso_start_mm,drip_iso_sample_data$drip_iso_start_dd)
  
  ggplot(drip_iso_sample_data, aes(x = date, y = drip_d18O_measurement, color = drip_entity_name)) +
    geom_line() +    # Scatter points   
    geom_point() +
    labs(title = "Plot drip_iso_d18O_measurementvs drip_iso_start_date",
         x = "Date", 
         y = "drip_iso_d18O_measurement") + 
    theme_minimal()
}

#________________________________________________________
#OLD
# Plot 9: “drip_iso_d2H_measurement” versus start-end day and time on a scatter plot
#p9 <- ggplot() +
#  geom_point(data = drip_iso_sample_data, aes(x = drip_iso_start_dd, y = drip_d2H_measurement), color = "blue", alpha = 0.6) +
#  geom_point(data = drip_iso_sample_data, aes(x = drip_iso_end_dd, y = drip_d2H_measurement), color = "red", alpha = 0.6) +
#  labs(title = "Drip Iso d2H Measurement vs Start-End Day", x = "Day", y = "Drip Iso d2H Measurement") +
#  theme_minimal()
#print(p9)
#________________________________________________________

if(dim(drip_iso_sample_data)[1] == 0){
  plot.new()
  text(0, 1, labels = paste("No data available on table drip_iso_sample_data"), pos = 4, cex = 1.1, adj = c(0, 1),col="black")
} else {
  date <- concat_date(drip_iso_sample_data$drip_iso_start_yyyy,drip_iso_sample_data$drip_iso_start_mm,drip_iso_sample_data$drip_iso_start_dd)
  
  ggplot(drip_iso_sample_data, aes(x = date, y = drip_d2H_measurement, color = drip_entity_name)) +
    geom_line() +    # Scatter points   
    geom_point() +
    labs(title = "Plot drip_iso_d2H_measurementvs drip_iso_start_date",
         x = "Date", 
         y = "drip_iso_d2H_measurement") + 
    theme_minimal()
}

#________________________________________________________
#OLD
# Plot 10: “drip_rate_measurement” versus “drip_rate_accumulation_time” with “drip_rate_accumulation_unit” on a bar graph
#p10 <- ggplot(drip_rate_sample_data, aes(x = drip_rate_accumulation_time, y = drip_rate_measurement, fill = drip_rate_accumulation_unit)) +
#  geom_bar(stat = "identity", position = "dodge", alpha = 0.6) +
#  labs(title = "Drip Rate Measurement vs Drip Rate Accumulation Time by Unit", x = "Drip Rate Accumulation Time", y = "Drip Rate Measurement") +
#  theme_minimal()
#print(p10)
#________________________________________________________

if(dim(drip_rate_sample_data)[1] == 0){
  plot.new()
  text(0, 1, labels = paste("No data available on table drip_rate_sample_data"), pos = 4, cex = 1.1, adj = c(0, 1),col="black")
} else {
  date <- concat_date(drip_rate_sample_data$drip_rate_start_yyyy,drip_rate_sample_data$drip_rate_start_mm,drip_rate_sample_data$drip_rate_start_dd)
  
  ggplot(drip_rate_sample_data, aes(x = date, y = drip_rate_measurement, color = drip_entity_name)) +
    geom_line() +    # Scatter points   
    geom_point() +
    labs(title = "Plot drip_rate_measurement vs drip_rate_start_date",
         x = "Date", 
         y = "drip_rate_measurement") + 
    theme_minimal()
}


#________________________________________________________
#OLD
# Plot 11: “mod_carb_d18O_measurement” versus start-end day and time on a scatter plot
#p11 <- ggplot() +
#  geom_point(data = mod_carb_sample_data, aes(x = mod_carb_start_dd, y = mod_carb_d18O_measurement), color = "blue", alpha = 0.6) +
#  geom_point(data = mod_carb_sample_data, aes(x = mod_carb_end_dd, y = mod_carb_d18O_measurement), color = "red", alpha = 0.6) +
#  labs(title = "Mod Carb d18O Measurement vs Start-End Day", x = "Day", y = "Mod Carb d18O Measurement") +
#  theme_minimal()
#print(p11)
#________________________________________________________

if(dim(mod_carb_sample_data)[1] == 0){
  plot.new()
  text(0, 1, labels = paste("No data available on table mod_carb_sample_data"), pos = 4, cex = 1.1, adj = c(0, 1),col="black")
} else {
  date <- concat_date(mod_carb_sample_data$mod_carb_start_yyyy,mod_carb_sample_data$mod_carb_start_mm,mod_carb_sample_data$mod_carb_start_dd)
  
  ggplot(mod_carb_sample_data, aes(x = date, y = mod_carb_d18O_measurement, color = drip_entity_name)) +
    geom_line() +    # Scatter points   
    geom_point() +
    labs(title = "Plot mod_carb_d18O_measurement vs mod_carb_start_date",
         x = "Date", 
         y = "mod_carb_d18O_measurement") + 
    theme_minimal()
}


#________________________________________________________
#OLD
# Plot 12: “drip_iso_d13C_measurement” versus start-end day and time on a scatter plot
#p12 <- ggplot() +
#  geom_point(data = drip_iso_sample_data, aes(x = drip_iso_start_dd, y = drip_iso_d13C_measurement), color = "blue", alpha = 0.6) +
#  geom_point(data = drip_iso_sample_data, aes(x = drip_iso_end_dd, y = drip_iso_d13C_measurement), color = "red", alpha = 0.6) +
#  labs(title = "Drip Iso d13C Measurement vs Start-End Day", x = "Day", y = "Drip Iso d13C Measurement") +
#  theme_minimal()
#print(p12)
#________________________________________________________

if(dim(mod_carb_sample_data)[1] == 0){
  plot.new()
  text(0, 1, labels = paste("No data available on table drip_rate_sample_data"), pos = 4, cex = 1.1, adj = c(0, 1),col="black")
} else {
  date <- concat_date(mod_carb_sample_data$mod_carb_start_yyyy,mod_carb_sample_data$mod_carb_start_mm,mod_carb_sample_data$mod_carb_start_dd)
  
  ggplot(mod_carb_sample_data, aes(x = date, y = mod_carb_d13C_measurement, color = drip_entity_name)) +
    geom_line() +    # Scatter points   
    geom_point() +
    labs(title = "Plot mod_carb_d13C_measurement vs mod_carb_start_date",
         x = "Date", 
         y = "mod_carb_d13C_measurement") + 
    theme_minimal()
}


# Close the PDF
dev.off()

# Output the path to the PDF
pdf_path

