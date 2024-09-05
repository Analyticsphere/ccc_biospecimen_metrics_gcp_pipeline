# ------------------------------------------------------------------------------
# Script Name: extract_renv_dependencies.R
# Author: Jake Peters
# Date: Sept 4, 2024
# Description: 
#   This script extracts R script filenames from a `config.yml` file (using `yaml`),
#   identifies the required packages via `renv::dependencies()`, and snapshots 
#   the environment to an `renv.lock` file for reproducibility. The script ensures
#   `renv` is initialized (if needed) and skips prompts during the snapshot.
#   The `renv.lock` file can then be used to generate a Dockerfile for running 
#   these R scripts in GCP Cloud Run.
#
# Usage: 
#   - Ensure `config.yml` contains an `r_file_name` field for each relevant script.
#   - Requires `yaml`, `renv`, and `jsonlite` packages.
#
# Input:
#   - config.yml: YAML configuration file listing R script filenames.
#
# Output:
#   - renv.lock: Captures package versions required by the scripts.
#
# Intended Use:
#   - Use the generated `renv.lock` to create a Dockerfile for running the R scripts 
#     in GCP Cloud Run.
#
# Dependencies:
#   - R packages: yaml, renv, jsonlite
# ------------------------------------------------------------------------------

# Load required libraries
library(yaml)
library(renv)
library(jsonlite)

# Function to extract dependencies from the config file and snapshot them
extract_and_snapshot_dependencies <- function(config_file = "config.yml", extra_r_file = NULL) {
  
  # Ensure renv is initialized for the project
  if (!file.exists("renv.lock")) {
    renv::init(bare = TRUE)
  }
  
  # Read the YAML config file as a list
  config <- yaml::read_yaml(config_file)
  
  # Extract 'r_file_name' from the config
  r_file_names <- sapply(config, function(x) {
    if (is.list(x) && !is.null(x$r_file_name)) {
      return(x$r_file_name)
    } else {
      return(NULL)
    }
  })
  
  # Remove NULL values (sections without 'r_file_name')
  r_file_names <- r_file_names[!sapply(r_file_names, is.null)]
  
  # Convert the list of file names to a character vector (removing names)
  r_file_names_vector <- as.character(r_file_names)
  
  # Manually append the extra R file to the vector (if provided)
  if (!is.null(extra_r_file)) {
    r_file_names_vector <- c(r_file_names_vector, extra_r_file)
  }
  
  # Print the extracted R script names (for verification)
  print(r_file_names_vector)
  
  # Call renv::dependencies() on the list of R file paths to get required packages
  dependencies <- renv::dependencies(r_file_names_vector)
  
  # Extract the package names from the dependencies data
  package_list <- unique(dependencies$Package)
  
  # Print the detected dependencies (for verification)
  print(package_list)
  
  # Snapshot only the libraries explicitly used in the scripts, based on the package list
  renv::snapshot(packages = package_list, prompt = FALSE)
}

# Function to print a summary of the renv.lock file
print_renv_lock_summary <- function(lockfile_path = "renv.lock", n = 10) {
  # Check if renv.lock file exists
  if (!file.exists(lockfile_path)) {
    cat("renv.lock file not found.\n")
    return()
  }
  
  # Read and parse the renv.lock file
  lockfile <- fromJSON(lockfile_path)
  
  # Extract R version
  r_version <- lockfile$R$Version
  
  # Extract package information
  packages <- lockfile$Packages
  
  # Prepare a summary of package names and versions, using package names as row names
  package_summary <- data.frame(
    Version = sapply(packages, function(pkg) pkg$Version),
    row.names = names(packages),
    stringsAsFactors = FALSE
  )
  
  # Print the R version
  cat("R version used in renv.lock file:", r_version, "\n\n")
  
  # Print the package summary, limited to 'n' rows
  cat("Summary of packages in renv.lock file (showing", n, "packages):\n")
  print(head(package_summary, n = n))
  
  # Add ... if there are more packages than displayed
  if (nrow(package_summary) > n) {
    cat("...\n")
    cat("There are", nrow(package_summary) - n, "more packages.\n")
  }
}

# Run the main extraction and snapshot process
# Add the manually included R file by specifying it as 'extra_r_file'
extract_and_snapshot_dependencies(extra_r_file = "ccc_biospecimen_metrics_api.R")

# Example usage of summary function
print_renv_lock_summary(n = 10)

