FROM rocker/shiny-verse

RUN install2.r --error tidyr
RUN install2.r --error shinyFeedback
RUN install2.r --error shinyWidgets
RUN install2.r --error httr
RUN install2.r --error reactable
RUN install2.r --error RestRserve
RUN install2.r --error rmarkdown
RUN install2.r --error openxlsx
RUN install2.r --error janitor
RUN install2.r --error lubridate
RUN install2.r --error kableExtra

COPY apps/shiny/shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY apps/shiny/init.sh /etc/shiny-server/init.sh
RUN chmod 777 /etc/shiny-server/init.sh

#COPY R/AF_APIFunction.R /
#COPY apps/shiny/* /

RUN rm /srv/shiny-server/index.html
RUN rm -rf /srv/shiny-server/*
COPY apps/shiny/ /srv/shiny-server/
COPY docs/ /srv/shiny-server/docs/

EXPOSE 3838

#CMD ["R", "-e", "shiny::runApp('.', host = '0.0.0.0', port = 3838)"]

ENTRYPOINT ["/etc/shiny-server/init.sh"]
