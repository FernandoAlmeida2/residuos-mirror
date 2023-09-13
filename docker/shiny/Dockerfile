FROM rocker/shiny

#COPY r-packages.txt /opt/r-packages.txt
COPY apt-packages.txt /opt/apt-packages.txt
# Install system requirements for index.R as needed
RUN apt-get update && \
    xargs apt-get install -y --no-install-recommends </opt/apt-packages.txt \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV _R_SHLIB_STRIP_=true
COPY Rprofile.site /etc/R
#RUN install2.r --error --skipinstalled \
#    shiny \
#    forecast \
#    jsonlite \
#    ggplot2 \
#    htmltools \
#    plotly

#RUN xargs install2.r </opt/r-packages.txt
#COPY ./app/* /srv/shiny-server/
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

#RUN R 0
USER shiny

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
