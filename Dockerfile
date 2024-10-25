# Build stage
FROM rocker/shiny:4 AS builder

ENV HOST 0.0.0.0

# Install system dependencies and clean up in a single RUN statement
RUN apt-get update && apt-get install --no-install-recommends -y \
    libv8-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    /tmp/downloaded_packages/ /tmp/*.rds

# Install R packages in one layer to leverage Docker caching
RUN install2.r --error --skipinstalled -n 4 \
    remotes \
    apexcharter \
    config \
    dplyr \
    golem \
    htmltools \
    deckgl \
    reactable \
    reactablefmtr \
    shiny \
    rlang \
    scales \
    htmlwidgets \
    V8 \
    grDevices \
    memoise

# Final stage
FROM rocker/shiny

# Copy installed R packages from builder
COPY --from=builder /usr/local/lib/R /usr/local/lib/R

# Copy application files
COPY multimedia_reduced.parquet /srv/shiny-server/multimedia_reduced.parquet
COPY occurrence_country /srv/shiny-server/occurrence_country
COPY styles.css /srv/shiny-server/styles.css
COPY R /srv/shiny-server/R
COPY DESCRIPTION /srv/shiny-server/DESCRIPTION
COPY NAMESPACE /srv/shiny-server/NAMESPACE
COPY app.R /srv/shiny-server/app.R


# Install the local package
RUN Rscript -e 'remotes::install_local("/srv/shiny-server", dependencies = FALSE)'

# Copy Shiny server configuration
COPY shiny.config /etc/shiny-server/shiny-server.conf

# Expose the port for the Shiny app
EXPOSE 8080

# Use the 'shiny' user to run the app
USER shiny

# Start the Shiny server
CMD ["/usr/bin/shiny-server"]
