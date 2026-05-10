library(shiny)
library(dplyr)
library(lubridate)

source("../R/metrics.R")
source("../R/anomaly_detection.R")
source("../R/developer_profile.R")

load("../data/example_commits.rda")
commits <- example_commits

commits_per_author <- get_commits_per_author(commits)
activity_by_hour   <- get_activity_by_hour(commits)
night_commits      <- detect_night_commits(commits)
suspicious_authors <- detect_suspicious_authors(commits)
dev_profiles       <- get_developer_profile(commits)

ui <- fluidPage(
  titlePanel("Vulcanus — Анализ активности разработчиков"),

  fluidRow(
    column(12,
      h4(paste("Всего коммитов:", get_total_commits(commits))),
      hr()
    )
  ),

  fluidRow(
    column(12,
      h4("Список коммитов"),
      tableOutput("commits_table")
    )
  ),

  hr(),

  fluidRow(
    column(6,
      h4("Коммиты по авторам"),
      plotOutput("commits_by_author", height = "300px")
    ),
    column(6,
      h4("Активность по часам суток (UTC)"),
      plotOutput("activity_by_hour", height = "300px")
    )
  ),

  hr(),

  fluidRow(
    column(12,
      h4("Профили разработчиков"),
      tableOutput("dev_profiles_table")
    )
  ),

  hr(),

  fluidRow(
    column(6,
      h4("Аномалии: ночные коммиты (00:00–05:59)"),
      tableOutput("night_commits_table")
    ),
    column(6,
      h4("Аномалии: подозрительные авторы"),
      tableOutput("suspicious_table")
    )
  )
)

server <- function(input, output, session) {

  output$commits_table <- renderTable({
    commits %>%
      arrange(desc(date)) %>%
      mutate(date = format(date, "%Y-%m-%d %H:%M")) %>%
      select(date, author, repo, message)
  })

  output$commits_by_author <- renderPlot({
    d <- commits_per_author
    barplot(
      height    = d$commits,
      names.arg = d$author,
      col       = "#4e79a7",
      main      = "Количество коммитов",
      xlab      = "Разработчик",
      ylab      = "Коммиты",
      border    = NA,
      las       = 2
    )
  })

  output$activity_by_hour <- renderPlot({
    all_hours <- data.frame(hour = 0:23)
    d <- all_hours %>%
      left_join(activity_by_hour, by = "hour") %>%
      mutate(commits = ifelse(is.na(commits), 0L, commits))
    barplot(
      height    = d$commits,
      names.arg = d$hour,
      col       = "#f28e2b",
      main      = "Активность по часам",
      xlab      = "Час",
      ylab      = "Коммиты",
      border    = NA
    )
  })

  output$dev_profiles_table <- renderTable({
    dev_profiles
  })

  output$night_commits_table <- renderTable({
    if (nrow(night_commits) == 0) {
      data.frame(Сообщение = "Ночных коммитов не обнаружено")
    } else {
      night_commits %>%
        mutate(date = format(date, "%Y-%m-%d %H:%M")) %>%
        select(date, hour, author, repo, message)
    }
  })

  output$suspicious_table <- renderTable({
    if (nrow(suspicious_authors) == 0) {
      data.frame(Сообщение = "Подозрительных авторов не обнаружено")
    } else {
      suspicious_authors
    }
  })
}

shinyApp(ui = ui, server = server)
