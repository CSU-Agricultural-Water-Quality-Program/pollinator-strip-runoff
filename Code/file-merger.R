# ALS to AWQP Data Clean and Merge Tool
# AJ Brown, ansley.brown@colostate.edu
# Started: 13 July 2022
# Updated: 21 Feb 2023

# Tool to clean and merge multiple .htm files directly downloaded
# from the ALS global portal into one master file and create categories for
# further analysis.

# Script work flow:
  # 0) Import libraries
  # 1) Define global variables
  # 1a) Working file_path
  # 1b) Set the default file directory
  # 1c) Define dictionaries for interpreting sample ID codes
  # 2) Define functions
  # 2a) map_values <-- a function to map sample ID text to dictionary values
  # 2b) import data
  # 2c) clean data
  # 2d) process data
  # 2e) execute previous functions
  # 2f) repeat for all files in a directory
  # 4) View resulting dataframe(s) for QA/QC
  # 3) Export data as csv

# Example code execution for users
# df_test <- returnSingleFile(path=file_path, export=FALSE)
# df_all <- returnAllFiles(d=directory, export=FALSE)
# Then take df_test or df_all and do whatever you want with it (e.g., graph)


# Import libraries
package.list <- c("magrittr",
                  "dplyr",
                  "readr",
                  "readxl",
                  "ggplot2",
                  "lattice",
                  "rvest",
                  "xml2",
                  "stringr",
                  'tidyverse',
                  'lubridate',
                  'stringr'
                  )
packageLoad <- function(packages){
  for (i in packages) {
    if (!require(i, character.only = TRUE)) {
      install.packages(i, repos = "http://cran.us.r-project.org")
      library(i, character.only = TRUE)
    }
  }
}
packageLoad(package.list)

# Global Variables
# source file paths from config.r
source('./Code/config.R')


# Dictionaries for interpreting sample ID codes
  # Add to these at needed for new locations, treatments, methods, etc.
location.dict <- c(
  "ARDEC" = "A2", #TODO: fix 2200 labeling in process data fxn; 2200 is removed
  "ARDEC South - Conv" = "ASC",
  "ARDEC South - Org" = "ASO",
  "AVRC STAR" = c("AV", "AVST1", "AVST2", "AVCT1", "AVCT2"),
  "Barley" = "BAR",
  "Berthoud" = "BT",
  "Big Hollow" = "HOL",
  "Boulder Lake" = "BOL",
  "Gunnison" = "GU",
  "Kerbel" = c("K", "KB", "ST1", "ST2", "CT1", "CT2", "MT1", "MT2", "INF"),
  "Legacy" = "LG",
  "Molina" = "MOL",
  "Stagecoach" = "SB",
  "Morrison Creek" = c("MOR","SB-MOR"),
  "Stage Coach Above" = c("SCA","SB-SCA"),
  "Stage Coach In" = c("SCI", "SB-SCI"),
  "Stage Coach Dam Outflow" = c("SCO","SB-SCO"),
  "The Ranch" = c("TR","SB-TR"), # Formerly, "Todd's Ranch"
  "Upper Yampa" = "UYM",
  "Yellow Jacket " = "YJ",
  "Lab Blank" = "BK"
)

trt.dict <- c(
  "ST1" = c("ST1", "AVST1"),
  "ST2" = c("ST2", "AVST2"),
  "CT1" = c("CT1", "AVCT1"),
  "CT2" = c("CT2", "AVCT2"),
  "MT1" = "MT1",
  "MT2" = "MT2",
  "Inflow" = "INF",
  "River A" = "RVA",
  "River B" = "RVB",
  "River Middle" = "RVMID",
  "Piezometer East" = "PZE",
  "Piezometer West" = "PZW",
  "Piezometer North" = "PZN",
  "Piezometer South" = "PZS",
  "Tile Drainage River" = "TDR",
  "Tile Drainage Lake" = "TDL",
  "Confluence" = "CON",
  "Upstream of Bridge" = "UP",
  "Downstream of Bridge" = "DOWN",
  "Middle at Bridge" = "MID",
  "Arapahoe Natl. Forest" = "ANF",
  "Willow Creek" = "WC",
  "Duck Pond" = "DP",
  "Upper willow at @ culvert (swale)" = "CUL",
  "Fish Pond" = "FP",
  "Fire 2" = "FR2"
  )

method.dict <- c(
  "ISCO" = c("ISC", "IN", "OT", "0T"),
  "Low-Cost Sampler" = c("LC", "INLC", "OTLC"),
  "Grab Sample" = c("GB", "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9"),
  "Hourly Grab" = c("GBH"),
  "Lab Blank" = c("BK")
  )

eventType.dict <- c(
  "Inflow" = c("IN", "INF", "INLC", "IN1", "IN2", "IN3", "IN4", "IN5", "IN6", 
               "IN7", "IN8", "IN9"),
  "Outflow" = c("OUT", "OT", "OTLC", "ST1", "ST2", "CT1", "CT2", "MT1", "MT2",
                "0T"),
  "Lab Blank" = c("BK")
  )

eventCount.dict <- c(
  "Irrigation 1" = c("01"),
  "Irrigation 2" = c("02"),
  "Irrigation 3" = c("03"),
  "Irrigation 4" = c("04"),
  "Irrigation 5" = c("05"),
  "Irrigation 6" = c("06"),
  "Irrigation 7" = c("07"),
  "Irrigation 8" = c("08"),
  "Storm 1" = c("S1"),
  "Storm 2" = c("S2"),
  "Storm 3" = c("S3"),
  "Storm 4" = c("S4"),
  "Storm 5" = c("S5")
  )

tssUnits.dict <- c(
  "TSS" = "mg/L",
  "EC" = "mS/cm",
  "pH" = "pH"
   )


# copy/paste excel data below to create geodata dataframe (i.e., separated by tabs, \t)
geo_key <- read.csv(text = "
location.name	treatment.name	event.type	long	lat
ARDEC		Outflow	-104.9869329	40.65356941
ARDEC		Inflow	-104.9888983	40.65361825
AVRC STAR	CT1	Point Sample	-103.689906	38.03957397
AVRC STAR	ST1	Point Sample	-103.6898805	38.03982154
AVRC STAR	ST2	Point Sample	-103.68987	38.04005803
AVRC STAR	CT2	Point Sample	-103.6898893	38.04030463
Barley		Outflow	-105.0167433	40.35369119
Below Stagecoach Dam		Point Sample	-106.8290859	40.28655773
Berthoud		Outflow	-105.08495	40.2818
Berthoud		Inflow	-105.0855686	40.28104111
Berthoud	River A	Point Sample	-105.09267	40.27863
Berthoud	River Middle	Point Sample	-105.08875	40.27962
Berthoud	River B	Point Sample	-105.08402	40.28317
Berthoud	Tile Drainage Lake	Point Sample	-105.089151	40.27926715
Berthoud	Tile Drainage River	Point Sample	-105.089	40.27928
Berthoud	Piezometer East	Point Sample	-105.0847844	40.28175382
Berthoud 	Piezometer West	Point Sample	-105.0849372	40.28170676
Big Hollow	Confluence	Point Sample	-105.018	40.29267
Big Hollow	Downstream of Bridge	Point Sample	-105.02235	40.28622
Big Hollow	Middle at Bridge	Point Sample	-105.02345	40.28388
Big Hollow	Upstream of Bridge	Point Sample	-105.03335	40.27663
Fire 1		Point Sample	-105.995761	40.152369
Fire 2		Point Sample	-105.987369	40.157551
Fire 3		Point Sample	-105.990499	40.154385
Gunnison		Outflow	-106.8269323	38.52626188
Gunnison		Inflow	-106.8160882	38.52338862
Kerbel	CT1	Outflow	-104.99828	40.67641
Kerbel	CT2	Outflow	-104.99666	40.67637
Kerbel	ST1	Outflow	-104.99764	40.67641
Kerbel	ST2	Outflow	-104.99701	40.67636
Kerbel	MT1	Outflow	-104.99798	40.67641
Kerbel	MT2	Outflow	-104.99735	40.67638
Legacy		Outflow	-106.8205175	40.43192773
Molina		Inflow	-108.04037	39.163825
Molina		Outflow	-108.04204	39.17067
Morrison Creek		Point Sample	-106.8158776	40.28942095
Stage Coach Above		Point Sample	-106.8812387	40.2691647
Stage Coach In		Point Sample	-106.8495245	40.28580534
Stagecoach		Outflow	-106.825602	40.29226711
The Ranch		Point Sample	-106.8150173	40.29174667
Upper Yampa		Outflow	-106.922182	40.20191997
Upper Yampa		Inflow	-106.91579	40.19042
Upper Yampa	Piezometer North	Point Sample	-106.92215	40.20168
Upper Yampa	Piezometer South 	Point Sample	-106.92193	40.20131
", sep = '\t', header = TRUE, stringsAsFactors = FALSE)

# Define Private Functions (i.e., do not call them directly)
map_values <- function(text, dict) {
 # function to map sample ID text to dictionary values
  # Split the text by spaces
  text_values <- unlist(strsplit(text, "-"))

  # Map each value to the corresponding dictionary value
  mapped_values <- lapply(text_values, function(x) {
    for (key in names(dict)) {
      if (x %in% dict[[key]]) {
        return(key)
      }
    }
    return(NA)
  })

  # Combine the mapped values into a single vector
  combined_values <- unlist(mapped_values)

  # Remove any NAs
  combined_values <- combined_values[!is.na(combined_values)]

  # Return the first combined value (or NA if there are no values)
  if (length(combined_values) > 0) {
    return(combined_values[1])
  } else {
    return(NA)
  }
}

importData <- function(file_path) {
  # ALS exports data as xls, but it is actually htm,
   # so it requires some cleaning here.
  df <- read_html(file_path) %>% # read in html file
    html_table() %>% # convert to table
    data.frame() # convert to dataframe
  return(df) # return dataframe
}

importDataXls <- function(file_path) {
  # ALS exports meta data as xls. Very confusing.
   # so we have to import it differently
  df <- read_excel(file_path) %>% # read in html file
    data.frame() # convert to dataframe
  return(df) # return dataframe
}

# Se and other results from the Kelso, WA ALS lab are in CSV currently.
# So, we may put an importDataCSV() function here for merging Kelso results.
# However, if i get a login for the Kelso, WA lab, and that export format is
# .xls (.htm) like the Houston lab, then it may not be needed.

cleanData <- function(df) {
# takes imported data and cleans it for processing
   # Drop unnecessary rows containing the word "sample:"
  df <- df[!grepl("Sample:", df$SAMPLE.ID),] %>% 
    # other cleaning processes:
    mutate(
      # create flag column for later
      flag = NA,
      # convert values containing "<" to 0
      RESULT = ifelse(grepl("<", RESULT), 0, RESULT),
      #  Flag values with H as past hold time and remove "H"
      flag = ifelse(str_detect(RESULT, "H"), "H", flag),
      RESULT = gsub("H", "", RESULT),
      # remove "See Attached" values, code 9999 set for flagging in flagData()
      RESULT = gsub("See Attached", 9999, RESULT),
      # make results numeric
      RESULT = as.numeric(ifelse(RESULT == "N/A", NA, RESULT)),
      # remove the scentific notation from results
      RESULT = ifelse(!is.na(RESULT), format(RESULT, scientific = FALSE), NA),
      # create column to indicate if a result value was a non-detect
      non.detect = ifelse(RESULT == 0, TRUE, FALSE),
      # change "N/A" to NA in any column
      across(everything(), ~ ifelse(. == "N/A", NA, .))) %>%
      # convert select columns to numeric if needed
      mutate_at(c("RESULT",
                  "DILUTION",
                  "MDL",
                  "RL",
                  "PERCENT.MOISTURE",
                  "PERCENT.SOLID"),
                 as.numeric) %>%
  return(df)
}

processData <- function(df) {
  # Process data to create new columns for analysis based on ID codes
    # create new columns based on ID codes
  df <- df %>%
    mutate(
      # create duplicate column
      duplicate = ifelse(grepl("-D", SAMPLE.ID, fixed = FALSE), TRUE, FALSE),
      # create location name column based on Sample ID
      location.name = sapply(SAMPLE.ID,
                             function(x) map_values(x, location.dict)),
      # create treatment name column based on Sample ID
      treatment.name = sapply(SAMPLE.ID, function(x) map_values(x, trt.dict)),
      # create method name column based on Sample ID
      method.name = sapply(SAMPLE.ID, function(x) map_values(x, method.dict)),
      # create event type name column based on Sample ID
      event.type = sapply(SAMPLE.ID, function(x) map_values(x, eventType.dict)),
      # create event count column based on Sample ID
      event.count = sapply(SAMPLE.ID, function(x) map_values(x, eventCount.dict)),
      # create flow volume column (in Liters), and fill it with NAs for now
      flow.vol.liter = NA
      ) %>%
    # If event.type is NA, use "Point Sample" as default
    mutate(
      event.type = if_else(is.na(event.type), "Point Sample", event.type)
    ) %>%
    mutate(
      # remove numbers from new columns due to dict mapping
      # caution: if there are more than 10 dict keys, this will not work
      # note: avoid naming future locations with numbers in the name
      across(c(
          "location.name", 
          "method.name", 
          "event.type"
        ), 
        ~gsub("[0-9]", "", .)
      ),
      # treatment.name needs special treatment because of CT/MT/ST 1/2 having #'s
      # so we modify treatment.name based on whether the last TWO characters are
      # numbers. If yes, then drop one digit, else leave it alone to preserve.
      treatment.name = if_else(
        grepl("[0-9][0-9]$", treatment.name), 
        substr(treatment.name, 1, nchar(treatment.name) - 1), 
        treatment.name
      )
    )
  return(df)
}

flagData <- function(df){
  # TODO: this function is messing things up. Fix it. then put it back in executeFxns()
# function to flag data and perform QA/QC after merging both htm and xls files
  # check water data for flags such as:
    # H = past hold time (declared in cleanData function)
    # J = minimum detection limit (MDL) > value > reporting limit (RL)
    # N = non-EPA method used
    # P = Ortho-P > Total P
    # more?
  
  # Compare Orthophosphate and Total Phosphorus within groups
    # Preparing a separate dataframe for comparison
  comparison_df <- df %>%
    group_by(duplicate, method.name, event.count, location.name, treatment.name, event.type) %>%
    summarize(
      ortho_P = max(RESULT[ANALYTE == "Phosphorus, Total Orthophosphate (as P)"], na.rm = TRUE),
      total_P = max(RESULT[ANALYTE == "Phosphorus, Total (As P)"], na.rm = TRUE),
      .groups = 'drop'
    )

    # Joining the comparison dataframe back to the original dataframe
  df <- df %>%
    left_join(comparison_df, by = c("duplicate", "method.name", "event.count", "location.name", "treatment.name", "event.type")) %>%
    mutate(
      # Updating the flag column based on the comparison
      flag = ifelse(ortho_P > total_P, ifelse(is.na(flag), "P", paste0(flag, ", P")), flag),
      # Other flagging conditions...
    ) %>%
    select(-ortho_P, -total_P) # Removing the extra columns
  
  # Other, more simple flag conditions...
  df <- df %>%
    mutate(
      # Check for J values and append to existing flag
      flag = ifelse(RESULT > MDL & RESULT < RL, ifelse(is.na(flag), "J", paste0(flag, ", J")), flag),
      # Identify "See Attached" results and append
      flag = ifelse(RESULT == 9999, ifelse(is.na(flag), "See Attached", paste0(flag, ", See Attached")), flag),
      # convert ALS specific conductivity to mS/cm like we use at AWQP
      RESULT = ifelse(
        "Suspended Solids (Residue, Non-Filterable)" %in% names(.) && ANALYTE == "Suspended Solids (Residue, Non-Filterable)", 
        RESULT / 1000, # they report uS/cm
        RESULT
      )
    )
  return(df)
}

addCoord <- function(df, geo_key) {
  # Merge the main dataframe with the geospatial key
  df <- merge(df, geo_key, by = c(
    "location.name", 
    "treatment.name",
    "event.type"
    ), 
    all.x = TRUE
    )
  return(df)
}

dfTss <- function(tss_fp) {
  df <- read_excel(tss_fp, sheet = "MasterData") %>%
    # collect relevant columns
    select(Sample_ID, Collection_date, `TSS_mg/L`, pH, `EC_mS/cm`) %>%
    # rename to be congruent with ALS data
    rename(SAMPLE.ID = Sample_ID,
           COLLECTED = Collection_date,
           `Suspended Solids (Residue, Non-Filterable)` = `TSS_mg/L`,
           `Specific Conductance` = `EC_mS/cm`) %>%
    # get rid of stock solution and D.I. TSS values
    filter(!(SAMPLE.ID %in% c("Stock Solution", "DI"))) %>%
    # omit NA values
    na.omit() %>%
    # processData here to get other columns correctly designated
    processData() %>%
    # TSS-specific column filling/data cleaning
    mutate(
      non.detect = FALSE,
      `Suspended Solids (Residue, Non-Filterable)` = as.numeric(
        `Suspended Solids (Residue, Non-Filterable)`),
      `Specific Conductance` = as.numeric(`Specific Conductance`),
      `pH` = as.numeric(`pH`)
    ) %>%
    pivot_longer(cols = c(
                          'pH', 'Suspended Solids (Residue, Non-Filterable)', 
                          'Specific Conductance'),
                 names_to = "ANALYTE", 
                 values_to = "RESULT") %>%
    mutate(
      DILUTION = 1,
      `RESULT.REPORTED.TO` = "RL",
      MDL = case_when(
        ANALYTE == "pH" ~ 0.1,
        ANALYTE == "Suspended Solids (Residue, Non-Filterable)" ~ 2.5,
        ANALYTE == "Specific Conductance" ~ 5.0,
        TRUE ~ NA_real_
      ),
      RL = case_when(
        ANALYTE == "pH" ~ 0.1,
        ANALYTE == "Suspended Solids (Residue, Non-Filterable)" ~ 2.5,
        ANALYTE == "Specific Conductance" ~ 5.0,
        TRUE ~ NA_real_
      ),
      METHOD = case_when(
        ANALYTE == "pH" ~ "EPA150.1",
        ANALYTE == "Suspended Solids (Residue, Non-Filterable)" ~ "E160.2 - TSS_W_160.2",
        ANALYTE == "Specific Conductance" ~ "E120.1 - COND_W",
        TRUE ~ NA_character_
      ),
      RESULT = as.numeric(RESULT),
      UNITS = tssUnits.dict[ANALYTE]
    ) 
  return(df)
}

executeFxns <- function(file_path) {
  # execute all previous functions and return final dataframe
  df <- importData(file_path) %>%
    cleanData() %>% # clean ALS format
    processData() %>% # create new columns using IDs
    addCoord(geo_key) %>% # add spatial data
    # flagData() %>% # flag and QA/QC data
    { select(., -all_of( # remove unnecessary columns
      c("REPORT.BASIS", 
        "PERCENT.MOISTURE", 
        "PERCENT.SOLID", 
        "LAB.ID.y", 
        "MATRIX", 
        "HOLD")
      [c("REPORT.BASIS", 
         "PERCENT.MOISTURE", 
         "PERCENT.SOLID", 
         "LAB.ID.y", 
         "MATRIX", 
         "HOLD") %in% 
          names(.)])) }
  return(df)
}

mergeFiles <- function(directory, tss_fp) {
  # import all htm files in the directory, merge, and return df
  print("Merging files...")
  file_list <- list.files(path = directory,
                          pattern = "*.xls", 
                          full.names = TRUE)
  print("Data files to be merged:")
  data_files <- file_list[!grepl("-Samples", file_list)]
  print(data_files)
  print("Metadata files to be merged:")
  meta_files <- file_list[grepl("-Samples", file_list)]
  print(meta_files)
  # merge data files
  df_data <- data_files %>%
    # pair and merge files here
    lapply(executeFxns) %>%
    bind_rows
  # merge metadata files
  df_meta <- meta_files %>%
    # pair and merge files here
    lapply(importDataXls) %>%
    bind_rows
  # merge data and metadata
  # print(df_data[721,]) # this row causes multiple row match warning
  df_merge <- df_data %>%
    left_join(df_meta, by = 'SAMPLE.ID')
  # change to posixct
  df_merge$COLLECTED <- as.POSIXct(df_merge$COLLECTED, format = '%d %b %Y %H:%M')
  # import TSS data to df w/ metadata
  df_tss <- dfTss(tss_fp)
  # merge tss data with als data
  df <- bind_rows(df_merge, df_tss) %>%
    filter(!grepl("Analysis", ANALYTE, ignore.case = TRUE)) 
  
  # Drop unnecessary columns that get re-created during merge
  # List of columns to drop
  cols_to_drop <- c("LAB.ID.y", 
                    "MATRIX", 
                    "HOLD")
  # Drop only if the column exists
  cols_to_drop <- cols_to_drop[cols_to_drop %in% names(df)]
  df <- select(df, -all_of(cols_to_drop))

  return(df)
  
}

# Define public functions (i.e., to be called by user)
returnSingleFile <- function(path = file_path, export = FALSE) {
  # return and optionally export a single file for QA/QC
  df <- executeFxns(path)
  if (export == TRUE) {
    # Get the current date in YYYY-MM-DD format
    currentDate <- format(Sys.Date(), "%Y-%m-%d")
    # Create a file name with the current date
    fileName <- paste0("./Report/single_file_", currentDate, ".csv")
    # Export the file with the new file name
    write.csv(df, file = fileName, row.names = FALSE)
  }
  return(df)
}

returnAllFiles <- function(d = directory, tss_fp = tss_file_path, export = TRUE) {
  # return and optionally export all files for QA/QC
  df <- mergeFiles(d, tss_fp)
  # for debugging only; uncomment as necessary
  #View(df)
  if (export == TRUE) {
    # Get the current date in YYYY-MM-DD format
    currentDate <- format(Sys.Date(), "%Y-%m-%d")
    # Create a file name with the current date
    fileName <- paste0("./Report/all_files_", currentDate, ".csv")
    # Export the file with the new file name
    write.csv(df, file = fileName, row.names = FALSE)
  }
  return(df)
}
