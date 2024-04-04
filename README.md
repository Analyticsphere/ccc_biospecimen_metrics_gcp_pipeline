# CCC Biospecimen Metrics API

This repository contains an R script that generates a Plumber API for running and rendering Kelsey's Rmarkdown files. The API is designed to run various module metrics reports based on the specified parameters.

## Contributors

- Report Developer: Kelsey Dowling
- Pipeline Developer: Jake Peters
- Date: October 2022
- Modified: August 2023

## Introduction

The ccc_module_metrics_api.R script provides a Plumber API to run and render Kelsey's Rmarkdown files. The API can be used to generate module metrics reports with customizable parameters.

## Endpoints

### Heartbeat

- **Method**: GET, POST
- **Route**: `/`
- **Description**: Returns "alive" for testing purposes.

### Run Module Analytics

- **Method**: GET, POST
- **Route**: `/run-biospecimen-analytics`
- **Description**: Runs Kelsey's markdown file with the specified report and testing parameters. Renders the Rmarkdown file, uploads the report to Google Cloud Storage, and returns a message indicating completion.

#### Parameters

- `report`: Which report to run.
- `testing`: Whether we're testing or not.

## Configuration

The configuration for the API and reports is stored in the [`config.yml`](config.yml) file. The `config.yml` file contains various parameters for different reports, including RMD file names, report file names, box folders, and cadence.

```yaml
default:
  report_maintainer: Kelsey Dowling
  pipeline_maintainer: Jake Peters
  consumer: Dominique Hopkins
  bucket: ccc_weekly_metrics_report
  test_box_folders: 
    - "222593912729"
  gcp_info:
    cloud_build_trigger: ccc-biospecimen-metrics
    cloud_run: ccc-biospecimen-metrics

weekly_biospecimen_metrics:
  rmd_file_name: Weekly_Biospecimen_Metrics.Rmd
  report_file_name: Weekly_Biospecimen_Metrics.pdf
  box_folders:
    - "204235111776" 
  gcp_info:
    cloud_scheduler: ccc-weekly-biospecimen-metrics
    frequency: "0 11 * * 1" # every Monday at 11 AM"

biospecimen_qc_metrics:
  rmd_file_name: Biospe_Custom_QC_Accession_Dup.Rmd
  report_file_name: Biospe_Custom_QC_Accession_Dup.pdf
  box_folders:
    - "221297686961" 
  gcp_info:
    cloud_scheduler: ccc-biospecimen-qc-metrics
    frequency: "30 11 * * 1" # every Monday at 11:30 AM"
    
bu_clinical_survey_stats:
  rmd_file_name: "BU Clinical Survey Summary Statistics.Rmd"
  report_file_name: BU_Clinical_Survey_Summary_Statistics.pdf
  box_folders:
    - "251314285484"
  gcp_info:
    cloud_scheduler: ccc-bu-clinical-biospecimen-survey-stats
    frequency: "30 11 1 * *" # first of the Month at 11:30 AM"
    
bum_research_survey_stats:
  rmd_file_name: "BUM Research Summary Statistics.Rmd"
  report_file_name: BUM_Research_Summary_Statistics.pdf
  box_folders:
    - "251314285484"
  gcp_info:
    cloud_scheduler: ccc-bum-research-biospecimen-survey-stats
    frequency: "0 12 1 * *"# first of the Month at 12 PM"

covid_survey_stats:
  rmd_file_name: "COVID Survey Summary Statistics.Rmd"
  report_file_name: COVID_Survey_Summary_Statistics.Rmd
  box_folders:
    - "251314285484"
  gcp_info:
    cloud_scheduler: ccc-covid-survey-stats
    frequency: "30 12 1 * *"# first of the Month at 12:30 PM"
```

## Running the API with Google Cloud Run and Cloud Scheduler

To deploy the API, you can use Google Cloud Run and schedule API requests using Cloud Scheduler. Here's a basic outline of the process:

1. **Containerize the API**: Build a Docker container with your API script and its dependencies.

2. **Push to Container Registry**: Push the Docker container image to Google Container Registry.

3. **Deploy to Cloud Run**: Deploy the container image to Google Cloud Run and configure the necessary environment variables.

4. **Schedule API Requests**: Use Google Cloud Scheduler to schedule API requests by specifying the API endpoint URL and desired frequency.

5. **Authenticate with Google Cloud**: Ensure proper authentication between the services.

