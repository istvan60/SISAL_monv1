#Please write R code that populates a SQL database with data stored on worksheets in an excel file. In the code, open the connection to the SQL server. Read and store each worksheet in the excel file in the R environment and upload the data of each worksheet to the database. Finally, close the database link. Return a log of the steps and their results.

# DATABASE CONNECTION ENVIRONMENT VARIABLES
# DB_HOST=127.0.0.1
# DB_USER=sisaladmin
# DB_NAME=sisalv3
# DB_PASSWORD=Mapejo19_

# Install and load necessary libraries
# install.packages("DBI")
# install.packages("RMySQL") # You can change this to your SQL database driver (e.g., RPostgres for PostgreSQL)
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("openxlsx")

library(DBI)
library(RMySQL)   # Change if using a different database like PostgreSQL or SQL Server
library(readxl)
library(dplyr)
library(openxlsx)

# display_precision for drip_rate_measurement
display_precision <- function(number) {
  if (number >= 10 && number == floor(number)) {
    return(0)  # Integer greater than 10 → no decimal places
  } else if (number >= 1 && number < 10) {
    return(1)  # Between 1 and 10 → 1 decimal place
  } else if (number >= 0 && number < 1) {
    # Extract decimal part
    decimal_part <- sub("^-?\\d+\\.", "", format(number, scientific = FALSE))
    decimals <- unlist(strsplit(decimal_part, ""))
    non_zeros <- which(decimals != "0")
    
    if (length(non_zeros) == 0) {
      return(0)  # Only zeros → no decimal places
    } else if (length(non_zeros) == 1) {
      return(non_zeros[1])  # One non-zero → display up to that digit
    } else {
      return(non_zeros[2])  # Two or more non-zeros → display up to the second one
    }
  } else {
    return(NA)  # Undefined case
  }
}

# Define the path to your Excel file
excel_file_path <- "G:\\Saját meghajtó\\01Munka\\CAVE DB PAGES\\test wbs"
setwd(excel_file_path)
file_list <- list.files(excel_file_path)
for (excel_file_name in file_list) {
  # excel_file_name <- "2024-01-08_monitoringdb_nk_lcb_workbook_bunker.xlsx"
  
  # Define database connection parameters
  db_host <- "127.0.0.1"
  db_user <- "sisaladmin"
  db_password <- "Mapejo19_"
  db_name <- "SISAL_Monv1"
  
  # Open connection to SQL database
  conn <- dbConnect(RMySQL::MySQL(), 
                    dbname = db_name, 
                    host = db_host, 
                    user = db_user, 
                    password = db_password,
                    client.flag = CLIENT_LOCAL_FILES)
  
  # Initialize an empty log to store the steps and results
  log <- vector("list", length = 0)
  
  # Initialize the sisalv3 site_ids
  sisalv3_site_ids <- read.csv("G:\\Saját meghajtó\\01Munka\\CAVE DB PAGES\\sisalv3 site_ids.csv", header = TRUE, sep = ",")
  max_sisalv3_site_id <- max(sisalv3_site_ids$site_id)
  
  # Step x: List all worksheet names in the Excel file
  worksheet_names <- excel_sheets(excel_file_name)[3:17][-4]
  
  # Step: maping worksheet_names with db table_names as a global variable
  table_names_with_worksheet_names <- dbGetQuery(conn, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'cavedb_v3';")
  names(table_names_with_worksheet_names) <- "table_name"
  table_names_with_worksheet_names$worksheet_name <- sort(c(worksheet_names,"entity_link_reference_NA"))
  table_names_with_worksheet_names$worksheet_name[which(table_names_with_worksheet_names$worksheet_name=="entity_link_reference_NA")] = "NA"
  table_names_with_worksheet_names$worksheet_name[which(table_names_with_worksheet_names$table_name=="precip")] = "precip_sample"
  table_names_with_worksheet_names$worksheet_name[which(table_names_with_worksheet_names$table_name=="precip_entity_metadata")] = "precip_entity_metadata"
  
  # Step: set site_name as a global variable
  site_name <- read.xlsx(excel_file_name, sheet = tolower('site'), startRow = 2, colNames = TRUE)$site_name
  # *** chk op length of site_name must be single value *** 
  
  # Step 2: Loop through each worksheet and upload data to SQL
  for (sheet in worksheet_names) {
    
    log <- append(log, paste("Processing worksheet:", sheet))
    
    # Read the data from the worksheet
    data <-  read.xlsx(excel_file_name, sheet = sheet, startRow = 2, colNames = TRUE)
    
    # If the data is not empty
    if (nrow(data) > 0) {
      
      # if sisalv3.site_id is exists for site_name then set site_id from sisalv3.site_id else auto increment from 1000
      if (sheet == "site") {
        sisalv3_site_id <- sisalv3_site_ids$site_id[tolower(sisalv3_site_ids$site_name) == tolower(data$site_name)]
        if (length(sisalv3_site_id) > 0) {
          data$site_id <- sisalv3_site_id
        }
      }
      
      # get/set the site_id based on cavedb for table NOTES, REFERENCE, PRECIP_SITE_METADATA, CAVE_ENTITY_METADATA, DRIP_ENTITY_METADATA based on site_name
      if (sheet %in% c("notes", "reference", "precip_site_metadata", "cave_entity_metadata", "drip_entity_metadata") ) {
        query <- sprintf("SELECT site_id FROM site WHERE lower(site_name) = '%s';", tolower(site_name)) 
        site_id <- dbGetQuery(conn, query)
        data$site_id <- site_id$site_id
        # remove site_name column if it exists
        if (length(which(names(data) == "site_name")) == 1) {
          data <- data[, -which(names(data) == "site_name")]        
        }
      }
      
      # record the precip_site_name and precip_entity_name pairs in the workbook
      if (sheet == "precip_site_metadata") {
        precip_site_name_precip_entity_name_pairs = data[,c("precip_site_name","precip_entity_name")]
        data <- data[, -which(names(data) == "precip_entity_name")] 
      }
      
      # get/set the precip_site_id for upload records to PRECIP_ENTITY_METADATA table based on precip_site_name & precip_entity_name pairs from "PRECIP_SITE_METADATA" worksheet
      if (sheet == "precip_entity_metadata") {
        precip_site_names <- paste(sprintf("'%s'", precip_site_name_precip_entity_name_pairs$precip_site_name), collapse=", ")
        query <- sprintf("SELECT precip_site_name, precip_site_id FROM precip_site_metadata WHERE lower(precip_site_name) in (%s);", tolower(precip_site_names))
        precip_site_ids <- dbGetQuery(conn, query)
        precip_site_name_precip_entity_name_pairs <- merge(
          precip_site_name_precip_entity_name_pairs, 
          precip_site_ids, 
          by = "precip_site_name", 
          all.x = TRUE
        )
        data <- merge(
          data,
          precip_site_name_precip_entity_name_pairs,
          by = "precip_entity_name",
          all.x = TRUE
        )
        if (length(which(names(data) == "precip_site_name")) == 1) {
          data <- data[, -which(names(data) == "precip_site_name")] 
        }
      }
      
      # get/set precip_entity_ids for upload PRECIP_SAMPLE table
      if (sheet == "precip_sample") {
        precip_entity_names <- paste(sprintf("'%s'", unique(data$precip_entity_name)), collapse=", ")
        query <- sprintf("SELECT precip_entity_name, precip_entity_id FROM precip_entity_metadata WHERE lower(precip_entity_name) in (%s);", tolower(precip_entity_names))
        precip_entity_ids <- dbGetQuery(conn, query)
        
        # precip_d18O_measurement & precip_d2H_measurement correction to 2 decimals
        data$precip_d18O_measurement <- round(as.numeric(data$precip_d18O_measurement),2)
        data$precip_d2H_measurement <- round(as.numeric(data$precip_d2H_measurement),2)
        
        data <- merge(
          data,
          precip_entity_ids,
          by = "precip_entity_name",
          all.x = TRUE
        )
        if (length(which(names(data) == "precip_entity_name")) == 1) {
          data <- data[, -which(names(data) == "precip_entity_name")]
        }
      }
      
      # get/set cave_entity_id for upload tables "cave_temperature_sample", "cave_relative_humidity_sample", "cave_pCO2_sample"
      if (sheet %in% c("cave_temperature_sample", "cave_relative_humidity_sample", "cave_pCO2_sample")) {
        cave_entity_names <- paste(sprintf("'%s'", unique(data$cave_entity_name)), collapse=", ")
        query <- sprintf("SELECT cave_entity_name, cave_entity_id FROM cave_entity_metadata WHERE lower(cave_entity_name) in (%s);", tolower(cave_entity_names))
        cave_entity_ids <- dbGetQuery(conn, query)
        data <- merge(
          data,
          cave_entity_ids,
          by = "cave_entity_name",
          all.x = TRUE
        )
        if (length(which(names(data) == "cave_entity_name")) == 1) {
          data <- data[, -which(names(data) == "cave_entity_name")]
        }
        if (length(which(names(data) == "cave_pCO2_.measurement")) == 1) {
          names(data)[which(names(data) == "cave_pCO2_.measurement")] <- "cave_pCO2_measurement"
        }
      }
      
      # get/set drip_entity_id for upload tables "drip_iso_sample", "drip_rate_sample", "mod_carb_sample"
      if (sheet %in% c("drip_iso_sample", "drip_rate_sample", "mod_carb_sample")) {
        # correction to 2 decimals in (drip_d18O_measurement, drip_d2H_measurement, drip_rate_measurement, mod_carb_d18O_measurement, mod_carb_d13C_measurement)
        if (sheet == "drip_iso_sample") {
          data$drip_d18O_measurement <- round(as.numeric(data$drip_d18O_measurement),2)
          data$drip_d2H_measurement <- round(as.numeric(data$drip_d2H_measurement),2)
        }
        if (sheet == "drip_rate_sample") {
          data$drip_rate_measurement <- round(as.numeric(data$drip_rate_measurement),2)
        }m
        if (sheet == "mod_carb_sample") {
          data$mod_carb_d18O_measurement <- round(as.numeric(data$mod_carb_d18O_measurement),2)
          data$mod_carb_d13C_measurement <- round(as.numeric(data$mod_carb_d13C_measurement),2)
        }

        
        drip_entity_names <- paste(sprintf("'%s'", unique(data$drip_entity_name)), collapse=", ")
        query <- sprintf("SELECT drip_entity_name, drip_entity_id FROM drip_entity_metadata WHERE lower(drip_entity_name) in (%s);", tolower(drip_entity_names))
        drip_entity_ids <- dbGetQuery(conn, query)
        data <- merge(
          data,
          drip_entity_ids,
          by = "drip_entity_name",
          all.x = TRUE
        )
        if (length(which(names(data) == "drip_entity_name")) == 1) {
          data <- data[, -which(names(data) == "drip_entity_name")]
        }
        if (length(which(names(data) == "drip_d18O_measurement")) == 1) {
          names(data)[which(names(data) == "drip_d18O_measurement")] <- "drip_iso_d18O_measurement"
        }
        if (length(which(names(data) == "drip_d18O_precision")) == 1) {
          names(data)[which(names(data) == "drip_d18O_precision")] <- "drip_iso_d18O_precision"
        }
        if (length(which(names(data) == "drip_d2H_measurement")) == 1) {
          names(data)[which(names(data) == "drip_d2H_measurement")] <- "drip_iso_d2H_measurement"
        }
        if (length(which(names(data) == "drip_d2H_precision")) == 1) {
          names(data)[which(names(data) == "drip_d2H_precision")] <- "drip_iso_d2H_precision"
        }
      }
      
      try({
        # Assuming the SQL table name matches the worksheet name (you can customize this part)
        # table_name <- gsub(" ", "_", tolower(sheet)) # Replace spaces with underscores for table name
        
        table_name <- table_names_with_worksheet_names$table_name[table_names_with_worksheet_names$worksheet_name == sheet]
        
        # Write the data to the SQL database (table will be created if it doesn't exist)
        dbWriteTable(conn, table_name, data, overwrite = FALSE, row.names = FALSE, append = TRUE)
        
        log <- append(log, paste("Successfully uploaded data from worksheet:", sheet))
        
      }, silent = TRUE)
      if (exists("error")) {
        log <- append(log, paste("Error occurred while uploading worksheet:", sheet, "-", error$message))
        rm(error)
      }
    } else {
      log <- append(log, paste("No data found in worksheet:", sheet))
    }
  }
  
  # Step 3: Close the database connection
  dbDisconnect(conn)
  
  # Step 4: Return the log
  print(log)
}


# closure step creating entity_link_reference table after all worbook uploaded

# Open connection to SQL database
conn <- dbConnect(RMySQL::MySQL(), 
                  dbname = db_name, 
                  host = db_host, 
                  user = db_user, 
                  password = db_password,
                  client.flag = CLIENT_LOCAL_FILES)

q1 <- "SELECT cave_entity_metadata.site_id, cave_entity_metadata.cave_entity_id from cave_entity_metadata;"
q2 <- "SELECT reference.site_id, reference.ref_id from reference;"
q3 <- "SELECT drip_entity_metadata.site_id, drip_entity_metadata.drip_entity_id from drip_entity_metadata"
q4 <- "SELECT precip_site_metadata.site_id, precip_entity_metadata.precip_entity_id
FROM precip_site_metadata
JOIN precip_entity_metadata ON precip_site_metadata.precip_site_id = precip_site_metadata.precip_site_id"
cave_entity_metadata_ids <- dbGetQuery(conn, q1)
reference_ids <- dbGetQuery(conn, q2)
drip_entity_metadata_ids <- dbGetQuery(conn, q3)
precip_entity_metadata_ids <- dbGetQuery(conn, q4)
entity_link_reference <- merge(cave_entity_metadata_ids, reference_ids, by = "site_id", all.x = TRUE)
entity_link_reference <- merge(entity_link_reference, drip_entity_metadata_ids, by = "site_id", all.x = TRUE)
entity_link_reference <- merge(entity_link_reference, precip_entity_metadata_ids, by = "site_id", all.x = TRUE)
entity_link_reference <- entity_link_reference[, -which(names(entity_link_reference) == "site_id")]



# Write the data to the SQL database (table will be created if it doesn't exist)
dbWriteTable(conn, "entity_link_reference", entity_link_reference, overwrite = FALSE, row.names = FALSE, append = TRUE)

dbDisconnect(conn)
