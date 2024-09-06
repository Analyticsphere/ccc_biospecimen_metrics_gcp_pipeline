# Step 1: Set R version
ARG R_VERSION=4.3  # Default value, can be overridden at build time
FROM rocker/r-ver:${R_VERSION}

# Step 2: Set correct paths (for xelatex)
ENV PATH="$PATH:/root/bin:/usr/local/lib"

# Step 3: Install system dependencies for TinyTeX, R packages, and tcltk (for summarytools)
RUN apt-get update && apt-get install -y --no-install-recommends \
    tcl-dev tk-dev libmagick++-dev wget graphviz imagemagick perl \
    libcurl4-openssl-dev libssl-dev libxml2-dev cmake libsodium-dev pkg-config \
    libharfbuzz-dev libfribidi-dev libfreetype6-dev \
    && /rocker_scripts/install_pandoc.sh 
    
#RUN Rscript -e "install.packages('tinytex', repos='http://cran.rstudio.com/')" && \
#    Rscript -e 'tinytex::install_tinytex(repository = "illinois")'&& \ 
#    Rscript -e 'install.packages("tcltk")' 

# Step 4: Copy only the renv.lock and renv/ to avoid cache invalidation
COPY renv.lock renv.lock
COPY renv/ renv/

# Step 5: Install renv, restore the environment & install additional packages
RUN Rscript -e "install.packages('renv')" && \
    Rscript -e "renv::restore()" 
#    Rscript -e "install.packages('gt', dependencies=TRUE, repos='http://cran.rstudio.com/')" && \
#    Rscript -e "devtools::install_github('kupietz/kableExtra')" && \
#    Rscript -e "devtools::install_github('dcomtois/summarytools')"


# Step 6: Copy R code and other resources to the container
COPY ["./ccc_biospecimen_metrics_api.R", "./ccc_biospecimen_metrics_api.R"]
COPY ["./Weekly_Biospecimen_Metrics.Rmd", "./Weekly_Biospecimen_Metrics.Rmd"]
COPY ["./Biospe_Custom_QC_Accession_Dup.Rmd", "./Biospe_Custom_QC_Accession_Dup.Rmd"]
COPY ["./BU Clinical Survey Summary Statistics.Rmd", "./BU Clinical Survey Summary Statistics.Rmd"]
COPY ["./BUM Research Summary Statistics.Rmd", "./BUM Research Summary Statistics.Rmd"]
COPY ["./COVID Survey Summary Statistics.Rmd", "./COVID Survey Summary Statistics.Rmd"]
COPY ["./Sanford_Clincal_Locations.csv", "./Sanford_Clincal_Locations.csv"]
COPY ["./Weekly Biospecimen CSV Outputs.R", "./Weekly Biospecimen CSV Outputs.R"]
COPY ["./config.yml", "./config.yml"]

# Step 7: Run R code (Plumber API)
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('ccc_biospecimen_metrics_api.R'); port <- as.numeric(Sys.getenv('PORT', '8000')); pr$run(host = '0.0.0.0', port = port)"]
