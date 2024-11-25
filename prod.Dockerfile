FROM rocker/shiny:4

ENV HOST 0.0.0.0
ENV SHINY_LOG_STDERR=1
ENV SHINY_LOG_LEVEL='INFO'

# Install system dependencies
RUN apt-get update && apt-get install --no-install-recommends -y \
    curl \
    ca-certificates \
    && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Create necessary directories
RUN mkdir -p /srv/shiny-server/www/vendor \
    && mkdir -p /var/cache/shiny \
    && mkdir -p /var/log/shiny-server

# Download and cache JavaScript dependencies locally
RUN cd /srv/shiny-server/www/vendor \
    && curl -L -o apexcharts.min.js https://cdn.jsdelivr.net/npm/apexcharts@3.26.2/dist/apexcharts.min.js \
    && curl -L -o jquery.min.js https://code.jquery.com/jquery-3.6.0.min.js \
    && curl -L -o tabler.min.css https://unpkg.com/@tabler/core@1.0.0-beta21/dist/css/tabler.min.css \
    && curl -L -o tabler.min.js https://unpkg.com/@tabler/core@1.0.0-beta21/dist/js/tabler.min.js \
    && curl -L -o deck.min.js https://cdn.jsdelivr.net/npm/deck.gl@9.0.34/dist/index.js

# Extra R packages
RUN install2.r --error --skipinstalled -n 2 \
    remotes \
    dplyr \
    reactable \
    deckgl \
    htmltools \
    memoise \
    apexcharter \
    reactablefmtr \
    magrittr \
    scales \
    rlang \
    purrr \
    viridisLite \
    config \
    httr2 \
    golem \
    shinyjs


# Copy application files
COPY inst /srv/shiny-server/inst
COPY R /srv/shiny-server/R
COPY DESCRIPTION /srv/shiny-server/DESCRIPTION
COPY NAMESPACE /srv/shiny-server/NAMESPACE
COPY data /srv/shiny-server/data
COPY www /srv/shiny-server/www/

RUN Rscript -e 'remotes::install_local("/srv/shiny-server", dependencies = FALSE)'

# Copy configuration and app entry point
COPY shiny.config /etc/shiny-server/shiny-server.conf
COPY app.R /srv/shiny-server/app.R

# Set proper permissions
RUN chown -R shiny:shiny /srv/shiny-server \
    && chown -R shiny:shiny /var/cache/shiny \
    && chown -R shiny:shiny /var/log/shiny-server \
    && chmod -R 755 /srv/shiny-server/www/vendor

# Add health check
HEALTHCHECK --interval=30s --timeout=5s --start-period=30s --retries=3 \
    CMD curl -f http://localhost:8080/ || exit 1

EXPOSE 8080

USER shiny

CMD ["/usr/bin/shiny-server"]
