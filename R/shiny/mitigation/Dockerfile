FROM rocker/shiny-verse

RUN install2.r --error tidyr
RUN install2.r --error shinyFeedback
RUN install2.r --error shinyWidgets
RUN install2.r --error httr
RUN install2.r --error reactable
RUN install2.r --error RestRserve

#COPY R/AF_APIFunction.R /
#COPY apps/shiny/* /

#EXPOSE 3838

#CMD ["R", "-e", "shiny::runApp('.', host = '0.0.0.0', port = 3838)"]
