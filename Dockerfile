# Use the rocker/shiny-verse image as the base (includes Shiny, Tidyverse, and many deps)
FROM rocker/shiny-verse:latest

# Install system dependencies for the R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl-dev \
    libxml2-dev \
    libcurl4-gnutls-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libglpk-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install additional R packages found in global.R
RUN R -e "install.packages(c('shinydashboard', 'leaflet', 'randomForest', 'caret', 'plotly', 'DT', 'cluster', 'httr', 'jsonlite', 'shinyjs', 'shinyWidgets', 'shinyanimate', 'shinycssloaders', 'openxlsx'), repos='https://cran.rstudio.com/')"

# Set the working directory
WORKDIR /app

# Copy the application code into the image
COPY . /app

# Expose the port Railway will provide (defaults to 3838 for local testing)
EXPOSE 3838

# Run the Shiny app
# host='0.0.0.0' is required for the app to be accessible outside the container
# port is dynamically assigned by Railway via the PORT environment variable
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.integer(Sys.getenv('PORT', 3838)))"]
