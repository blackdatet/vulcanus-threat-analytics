# Docker-конфигурация проекта Vulcanus
# Автор: blackdatet

FROM rocker/shiny:latest

WORKDIR /app

RUN R -e "install.packages(c('shiny', 'dplyr', 'ggplot2', 'lubridate', 'httr', 'jsonlite'), repos='https://cloud.r-project.org')"

COPY . /app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app/app', host='0.0.0.0', port=3838)"]
