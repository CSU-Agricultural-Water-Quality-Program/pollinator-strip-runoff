# configuration file for ALS data cleaning tool
# AJ Brown
# Ansley.Brown@colostate.edu
# 3 Nov 2023

# How to:
# Define your file paths below to be used in this tool.  Please note that these
# variables are found in the file-merger.r script, which ultimately controls the
# file path for all other scripts.

# Definitions:
# directory = where ALS raw data is stored (xls and htm)
# tss_file_path = where TSS data excel file is stored (xlsx)

# Function to create directory if it doesn't exist
create_directory_if_not_exists <- function(directory_path) {
  if (!file.exists(directory_path)) {
    dir.create(directory_path, recursive = TRUE)
    message("Created directory: ", directory_path)
  } else {
    message("Directory already exists: ", directory_path)
  }
}

# Native GitHub Repo Folders:
directory <- "./Data"
tss_file_path <- './TSS/TSS_Master_2023.xlsx'

# for real data in file that won't be uploaded to github repo:
# directory <- "{your file path to ALS data FOLDER here}"
# tss_file_path <- '{your file path to TSS data FILE here}'

# Example using real data files for and create folder function:
# directory <- "./Confidential Data"
# tss_file_path <- './Confidential TSS/TSS_Master_2023.xlsx'
# create_directory_if_not_exists(directory)
# create_directory_if_not_exists(dirname(tss_file_path))

# Getting the directory name for TSS file path
tss_directory <- dirname(tss_file_path)