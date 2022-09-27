FROM rocker/tidyverse

# setup nginx
RUN apt-get update && \
    apt-get install -y libsodium-dev && \
    apt-get install -y libpq-dev

RUN install2.r --error purrr
RUN install2.r --error lubridate
RUN install2.r --error RestRserve
RUN install2.r --error tidyr

RUN rstudio-server stop

COPY R/AF_APIFunction.R /
COPY apps/api/* /
COPY data/data_input.RDS /

# setup nginx
#RUN apt-get update && \
#    apt-get install -y nginx apache2-utils

#ADD ./nginx.conf /etc/nginx/nginx.conf

#EXPOSE 80 443
EXPOSE 8080

#CMD service nginx start && R -e "source('app.R')"
CMD R -e "source('app.R')"
