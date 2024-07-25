# Dockerfile

FROM rocker/tidyverse:latest

# Set the correct path for xelatex
ENV PATH="$PATH:/root/bin:/usr/local/lib"
# Message daniel on gitter when this doesn't work

# Install tinytex linux dependencies, pandoc, and rmarkdown
# Reference: https://github.com/csdaw/rmarkdown-tinytex/blob/master/Dockerfile
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    tcl-dev \
    tk-dev \
    libmagick++-dev \ 
    wget \
    graphviz \ 
    imagemagick \
    perl && \
    /rocker_scripts/install_pandoc.sh && \
    install2.r rmarkdown 

### Install tcltk which is needed for summarytools library
# I added this when I got this error in GCP Cloud Run:
# ! package or namespace load failed for 'summarytools':
# .onLoad failed in loadNamespace() for 'tcltk', details:
# call: dyn.load(file, DLLpath = DLLpath, ...)
# error: unable to load shared object '/usr/local/lib/R/library/tcltk/libs/tcltk.so':
# libtcl8.6.so: cannot open shared object file: No such file or directory
# Backtrace:
# 1. └─base::library(summarytools)

RUN apt-get update \
  && apt-get install -y \
    tcl-dev \
    tk-dev 

# Install tinytex
# RUN Rscript -e 'tinytex::install_tinytex()'
RUN Rscript -e 'tinytex::install_tinytex(repository = "illinois")'

# Needed for summarytools namespace
RUN Rscript -e 'install.packages("tcltk")'

# Install R libraries
RUN install2.r --error plumber gridExtra bigrquery dplyr \
               epiDisplay lubridate tidyverse gtsummary \
               googleCloudStorageR data.table reshape listr ggplot2 \
               RColorBrewer stringr janitor expss magrittr arsenal rio \
               finalfit sqldf gmodels glue webshot2 cowplot crosstable \
               magrittr gmodels magick gargle tools expss config openxlsx
              

# These libraries might not be available from install2.R so use CRAN
RUN R -e "install.packages(c('gt'), dependencies=TRUE, repos='http://cran.rstudio.com/')"

# When I try to use kable extra with a normal installation from CRAN or install2.r
# I get the error:
# Error: package or namespace load failed for 'kableExtra':
# .onLoad failed in loadNamespace() for 'kableExtra', details:
#  call: !is.null(rmarkdown::metadata$output) && rmarkdown::metadata$output %in% 
#  error: 'length = 2' in coercion to 'logical(1)'
# The solution is to install a patched version from github
# https://github.com/haozhu233/kableExtra/issues/750
RUN R -e "devtools::install_github('kupietz/kableExtra')"
RUN R -e "devtools::install_github('dcomtois/summarytools')"

# Copy R code to directory in instance
COPY ["./ccc_biospecimen_metrics_api.R", "./ccc_biospecimen_metrics_api.R"]
COPY ["./Weekly_Biospecimen_Metrics.Rmd", "./Weekly_Biospecimen_Metrics.Rmd"]
COPY ["./Biospe_Custom_QC_Accession_Dup.Rmd", "./Biospe_Custom_QC_Accession_Dup.Rmd"]
COPY ["./BU Clinical Survey Summary Statistics.Rmd", "./BU Clinical Survey Summary Statistics.Rmd"]
COPY ["./BUM Research Summary Statistics.Rmd", "./BUM Research Summary Statistics.Rmd"]
COPY ["./COVID Survey Summary Statistics.Rmd", "./COVID Survey Summary Statistics.Rmd"]
COPY ["./Sanford_Clincal_Locations.csv", "./Sanford_Clincal_Locations.csv"]
COPY ["./config.yml", "./config.yml"]

# Run R code
ENTRYPOINT ["R", "-e","pr <- plumber::plumb('ccc_biospecimen_metrics_api.R'); pr$run(host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')))"]
