# File:        ccc_biospecimen_metrics_api.R
# Description: This script generates a plumber api that runs/renders Kelsey's 
#              Rmarkdown file.
# Author:      Jake Peters
# Date:        October 2022

library(plumber)
library(rmarkdown)
library(googleCloudStorageR)
library(gargle)
library(tools)
library(glue)

#* heartbeat...for testing purposes only. Not required to run analysis.
#* @get /
#* @post /
function() {
  return("alive")
}

#* Runs Kelsey's markdown file
#* @param report:string Which report to run
#* @param testing:boolean Whether we're testing or not
#* @get /run-biospecimen-metrics
#* @post /run-biospecimen-metrics
function(report, testing = FALSE) {
  testing <- as.logical(testing)
  report <- as.character(report)

  # Determines which config from config.yml to use
  Sys.setenv(R_CONFIG_ACTIVE = report)

  # Set parameters using arguments and config.yml file
  configuration <- config::get(config = report)
  r_file_name <- configuration$r_file_name
  report_file_name <- configuration$report_file_name
  bucket <- configuration$bucket
  print(paste0("bucket: ", bucket))
  if (testing) {
    box_folders <- configuration$test_box_folders
  } else {
    box_folders <- configuration$box_folders
  }

  # Create "_boxfolder_123456789012" tag
  box_str <- ""
  for (folder in box_folders) {
    box_str <- paste0(box_str, "_boxfolder_", folder)
  }
  print(paste0("box_str: ", box_str))

  # Check if a filename ends with ".r" or ".rmd" in all lowercase
  is_r_file <- grepl("\\.r$", r_file_name
                   , ignore.case = TRUE) # TRUE if ends in ".r"
  is_rmd_file <- grepl("\\.rmd$", r_file_name
                     , ignore.case = TRUE) # TRUE if ends in ".rmd"

  if (is_rmd_file) {
    # Add time stamp and box folder tag to to report name
    report_fid <- paste0(
      file_path_sans_ext(report_file_name),
      format(Sys.time(), "_%m_%d_%Y"), box_str,
      ".", file_ext(report_file_name)
    )
    Sys.setenv(REPORT_FID = report_fid)

    # Select document type given the extension of the report file name
    output_format <- switch(file_ext(report_file_name),
      "pdf"  = "pdf_document",
      "html" = "html_document"
    )
    if (is.null(output_format)) {
      stop("Report file extension is invalid. Script did not execute.")
    }

    # Render the rmarkdown file
    rmarkdown::render(r_file_name
                   ,
      output_format = output_format,
      output_file = report_fid,
      clean = TRUE
    )
  } else if (is_r_file) {
    source(r_file_name)
  } else {
    stop("The file extension of the R script is invalid. Script did not execute.") 
  }
  
  # Retrieve variables relevant to gcs upload
  # They do not persist after rmarkdown::render for some reports, including mod1_stats
  bucket <- config::get(value = "bucket")
  report_fid <- Sys.getenv("REPORT_FID")

  # Authenticate with Google Storage and write report file to bucket
  scope <- c("https://www.googleapis.com/auth/cloud-platform")
  token <- token_fetch(scopes = scope)
  gcs_auth(token = token)

  # Loop through CSV and PDF files, write them to GCP Cloud Storage, and print their names
  filelist <- list.files(pattern = "*.csv$|*.xlsx$|*.pdf$")
  uploaded_files <- lapply(filelist, function(x) {
    gcs_upload(x, bucket = bucket, name = x)
    print(paste("Uploaded file:", x))
    x # Return the file name for further processing if needed
  })

  # Return a string for for API testing purposes
  ret_str <- paste("All done. Check", bucket, "for", report_fid)
  print(ret_str)
  return(ret_str)
}
