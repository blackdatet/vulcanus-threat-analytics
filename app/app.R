library(shiny)
library(dplyr)
library(lubridate)

source("../R/metrics.R")
source("../R/anomaly_detection.R")
source("../R/developer_profile.R")

example_commits <- readRDS("../data/example_commits.rds")
example_commits$date <- as.POSIXct(example_commits$date, tz = "UTC")
commits <- example_commits

commits_per_author <- get_commits_per_author(commits)
activity_by_hour   <- get_activity_by_hour(commits)
night_commits      <- detect_night_commits(commits)
suspicious_authors <- detect_suspicious_authors(commits)
dev_profiles       <- get_developer_profile(commits)

ui <- fluidPage(

  tags$head(tags$style(HTML("
    body { background-color: #f4f6f9; font-family: 'Segoe UI', Arial, sans-serif; }
    .page-header { padding: 24px 32px 8px 32px; border-bottom: 1px solid #dee2e6; margin-bottom: 24px; }
    .page-title  { font-size: 22px; font-weight: 700; color: #2c3e50; margin: 0; }
    .page-sub    { font-size: 13px; color: #6c757d; margin-top: 4px; }
    .content     { padding: 0 32px 32px 32px; }
    .stat-card   {
      background: #fff; border-radius: 8px; padding: 16px 20px;
      box-shadow: 0 1px 4px rgba(0,0,0,0.07); text-align: center;
    }
    .stat-val    { font-size: 30px; font-weight: 700; color: #4e79a7; line-height: 1.1; }
    .stat-lbl    { font-size: 12px; color: #6c757d; margin-top: 4px; }
    .panel       {
      background: #fff; border-radius: 8px; padding: 20px 24px;
      box-shadow: 0 1px 4px rgba(0,0,0,0.07); margin-bottom: 20px;
    }
    .panel-title {
      font-size: 14px; font-weight: 600; color: #2c3e50;
      border-left: 4px solid #4e79a7; padding-left: 10px;
      margin-bottom: 16px;
    }
    .anomaly-title {
      font-size: 13px; font-weight: 600; color: #495057; margin-bottom: 8px;
    }
    table.dataTable, .shiny-output-error { font-size: 13px; }
  "))),

  # Шапка
  div(class = "page-header",
    h2("Vulcanus", class = "page-title"),
    p("Аналитическая панель активности разработчиков", class = "page-sub")
  ),

  div(class = "content",

    # Сводные метрики
    fluidRow(
      column(3, div(class = "stat-card",
        div(class = "stat-val", get_total_commits(commits)),
        div(class = "stat-lbl", "Всего коммитов")
      )),
      column(3, div(class = "stat-card",
        div(class = "stat-val", n_distinct(commits$author)),
        div(class = "stat-lbl", "Авторов")
      )),
      column(3, div(class = "stat-card",
        div(class = "stat-val", nrow(night_commits)),
        div(class = "stat-lbl", "Ночных коммитов")
      )),
      column(3, div(class = "stat-card",
        div(class = "stat-val", format(min(as.Date(commits$date)), "%d.%m.%Y")),
        div(class = "stat-lbl", "Начало активности")
      ))
    ),

    tags$br(),

    # Графики
    div(class = "panel",
      div(class = "panel-title", "Активность"),
      fluidRow(
        column(6, plotOutput("commits_by_author", height = "280px")),
        column(6, plotOutput("activity_by_hour",  height = "280px"))
      )
    ),

    # Профили разработчиков
    div(class = "panel",
      div(class = "panel-title", "Профили разработчиков"),
      tableOutput("dev_profiles_table")
    ),

    # Аномалии
    div(class = "panel",
      div(class = "panel-title", "Обнаруженные аномалии"),
      fluidRow(
        column(6,
          p(class = "anomaly-title", "Ночные коммиты (00:00–05:59 UTC)"),
          tableOutput("night_commits_table")
        ),
        column(6,
          p(class = "anomaly-title", "Авторы с аномальной активностью"),
          tableOutput("suspicious_table")
        )
      )
    ),

    # Последние коммиты
    div(class = "panel",
      div(class = "panel-title", "Последние коммиты (20 из загруженных)"),
      tableOutput("commits_table")
    )
  )
)

server <- function(input, output, session) {

  output$commits_by_author <- renderPlot({
    d <- commits_per_author
    par(mar = c(7, 4, 3, 1))
    bp <- barplot(
      height    = d$commits,
      col       = "#4e79a7",
      main      = "Коммиты по авторам",
      ylab      = "Количество коммитов",
      border    = NA,
      las       = 2,
      cex.names = 0.85,
      cex.axis  = 0.85,
      names.arg = rep("", nrow(d))
    )
    text(
      x      = bp,
      y      = -max(d$commits) * 0.04,
      labels = d$author,
      srt    = 40,
      adj    = 1,
      xpd    = TRUE,
      cex    = 0.85
    )
  })

  output$activity_by_hour <- renderPlot({
    all_hours <- data.frame(hour = 0:23)
    d <- all_hours %>%
      left_join(activity_by_hour, by = "hour") %>%
      mutate(commits = ifelse(is.na(commits), 0L, commits))

    colors <- ifelse(d$hour <= 5, "#e15759", "#59a14f")

    par(mar = c(4, 4, 3, 1))
    barplot(
      height    = d$commits,
      names.arg = d$hour,
      col       = colors,
      main      = "Активность по часам суток (UTC)",
      xlab      = "Час",
      ylab      = "Количество коммитов",
      border    = NA,
      cex.names = 0.75,
      cex.axis  = 0.85
    )
    legend("topright",
      legend = c("Рабочее время", "Ночное время (0–5)"),
      fill   = c("#59a14f", "#e15759"),
      border = NA, bty = "n", cex = 0.8
    )
  })

  output$dev_profiles_table <- renderTable({
    dev_profiles %>%
      rename(
        "Автор"                   = author,
        "Коммитов"                = total_commits,
        "Активных дней"           = active_days,
        "Коммитов / день"         = avg_commits_per_day,
        "Первый час активности"   = first_activity_hour,
        "Последний час активности"= last_activity_hour
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

  output$night_commits_table <- renderTable({
    if (nrow(night_commits) == 0) {
      data.frame(Сообщение = "Ночных коммитов не обнаружено")
    } else {
      night_commits %>%
        arrange(hour) %>%
        mutate(date = format(date, "%Y-%m-%d %H:%M")) %>%
        select(date, hour, author, message) %>%
        rename(
          "Дата"      = date,
          "Час (UTC)" = hour,
          "Автор"     = author,
          "Сообщение" = message
        )
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

  output$suspicious_table <- renderTable({
    df <- suspicious_authors %>%
      filter(!is.na(commits)) %>%
      mutate(z_score = round(z_score, 2)) %>%
      select(author, commits, z_score) %>%
      rename(
        "Автор"    = author,
        "Коммитов" = commits,
        "Z-оценка" = z_score
      )
    if (nrow(df) == 0) {
      data.frame(Сообщение = "Подозрительных авторов не обнаружено")
    } else {
      df
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

  output$commits_table <- renderTable({
    commits %>%
      arrange(desc(date)) %>%
      head(20) %>%
      mutate(date = format(date, "%Y-%m-%d %H:%M")) %>%
      select(date, author, repo, message) %>%
      rename(
        "Дата"          = date,
        "Автор"         = author,
        "Репозиторий"   = repo,
        "Сообщение"     = message
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")
}

shinyApp(ui = ui, server = server)
