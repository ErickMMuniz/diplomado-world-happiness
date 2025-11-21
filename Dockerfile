# Start from the official rocker/rstudio image
FROM rocker/rstudio:latest

# Install necessary system libraries for R package compilation (optional but recommended)
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libcairo2-dev \
    libssl-dev \
    libcurl4-gnutls-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages.
# We use the R command line to install packages.
# The 'Ncpus' argument speeds up installation on multi-core systems.
# The 'repos' argument specifies the CRAN mirror.
RUN R -e "install.packages(c('tidyverse', 'data.table', 'caret', 'randomForest', 'e1071'), \
    Ncpus = 4, \
    repos = 'https://cran.rstudio.com/')"

# You can optionally set the default working directory for the RStudio session
# The RStudio user's home directory is typically /home/rstudio
# WORKDIR /home/rstudio