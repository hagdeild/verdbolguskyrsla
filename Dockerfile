# Base image with R + tidyverse preinstalled
FROM rocker/tidyverse:4.5.0

# System libraries many R packages need
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev \
    libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    wget gdebi-core \
 && rm -rf /var/lib/apt/lists/*

# Install Quarto CLI (pin to your current version)
ARG QUARTO_VERSION=1.5.57
RUN wget -q https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb \
 && gdebi -n quarto-${QUARTO_VERSION}-linux-amd64.deb \
 && rm quarto-${QUARTO_VERSION}-linux-amd64.deb

# Fast package installer
RUN R -q -e "install.packages('pak', repos='https://cloud.r-project.org')"

# Preinstall the R packages you listed (some are in tidyverse already; preinstall anyway for speed)
# Note: modeltime pulls in timetk + tidymodels
RUN R -q -e "pak::pkg_install(c( \
  'modeltime','timetk','tidymodels','plotly','eurostat','XML','httr', 'fredr', \
  'openxlsx','zoo','rvest','readxl','here','scales','rmarkdown','knitr','quarto' \
))"

# Default workdir for the runner
WORKDIR /work
