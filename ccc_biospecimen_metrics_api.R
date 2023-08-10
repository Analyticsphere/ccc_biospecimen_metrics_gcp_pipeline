# File:       ccc_biospecimen_metrics_api.R
# Decription: This script generates a plumber api that runs/renders Kelsey/Jings's
#             Rmarkdown file.
# Author:     Jake Peters
# Date:       October 2022
# Updated:    August 2023

library(plumber)
library(rmarkdown)
library(googleCloudStorageR)
library(gargle)
library(tools)

#* heartbeat...for testing purposes only. Not required to run analysis.
#* @get /
#* @post /
function() {
  return("alive")
  }

#* Runs report
#* @get /run-ccc-biospecimen-metrics
#* @post /run-ccc-biospecimen-metrics
function() {
    # Define parameters
    rmd_file_name    <- "Weekly_Biospecimen_Metrics.Rmd"
    report_file_name <- paste("Weekly_Biospecimen_Metrics_", 
                              Sys.Date()-1,
                              "_boxfolder_204235111776", 
                              ".pdf", 
                              sep="")
    bucket           <- "gs://ccc_weekly_metrics_report"

    # Authenticate to BigQuery
    print("Authenticating to BigQuery...")
    bigrquery::bq_auth()
    print("...authentication to BigQuery successful!")
    
    # Render the RMD file
    rmarkdown::render(rmd_file_name, output_file = report_file_name)

    # Authenticate with Google Storage and write report file to bucket
    scope <- c("https://www.googleapis.com/auth/cloud-platform")
    token <- token_fetch(scopes = scope)
    gcs_auth(token = token)

    # Loop through CSV and PDF files, write them to GCP Cloud Storage, and print their names
    filelist <- list.files(pattern = "*.csv$|*.pdf$")
    uploaded_files <- lapply(filelist, function(x) {
      gcs_upload(x, bucket = bucket, name = x)
      print(paste("Uploaded file:", x))
      x  # Return the file name for further processing if needed
    })
    
    # Return a string for API testing purposes, including the uploaded file names
    ret_str <- paste("All done. Check", bucket, "for output files:", paste(uploaded_files, collapse = ", "))
    return(ret_str)
}
