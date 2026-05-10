library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)

source("../R/metrics.R")
source("../R/anomaly_detection.R")
source("../R/developer_profile.R")
source("../R/commit_classifier.R")

example_commits <- readRDS("../data/example_commits.rds")
example_commits$date <- as.POSIXct(example_commits$date, tz = "UTC")
commits <- example_commits

commits_per_author <- get_commits_per_author(commits)
activity_by_hour   <- get_activity_by_hour(commits)
night_commits      <- detect_night_commits(commits)
suspicious_authors <- detect_suspicious_authors(commits)
dev_profiles       <- get_developer_profile(commits)
commits_by_type    <- get_commits_by_type(commits)
loc_stats          <- get_loc_stats(commits)

TYPE_COLORS <- c(
  feature  = "#4e79a7",
  fix      = "#e15759",
  docs     = "#76b7b2",
  refactor = "#f28e2b",
  test     = "#59a14f",
  config   = "#b07aa1",
  other    = "#bab0ac"
)

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
    .anomaly-title { font-size: 13px; font-weight: 600; color: #495057; margin-bottom: 8px; }
    .shiny-output-error { font-size: 13px; }
  "))),

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

    # Активность: коммиты по авторам + по часам
    div(class = "panel",
      div(class = "panel-title", "Активность"),
      fluidRow(
        column(6, plotOutput("commits_by_author", height = "260px")),
        column(6, plotOutput("activity_by_hour",  height = "260px"))
      )
    ),

    # Тепловая карта + типы коммитов
    div(class = "panel",
      div(class = "panel-title", "Характер изменений"),
      fluidRow(
        column(8, plotOutput("activity_heatmap",   height = "240px")),
        column(4, plotOutput("commits_type_chart", height = "240px"))
      )
    ),

    # Профиль разработчика: таблица + LoC
    div(class = "panel",
      div(class = "panel-title", "Профили разработчиков"),
      fluidRow(
        column(7, tableOutput("dev_profiles_table")),
        column(5, plotOutput("loc_chart", height = "220px"))
      )
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
      div(class = "panel-title", "Последние коммиты"),
      tableOutput("commits_table")
    )
  )
)

server <- function(input, output, session) {

  # --- Коммиты по авторам ---
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
      cex.axis  = 0.85,
      names.arg = rep("", nrow(d))
    )
    text(bp, -max(d$commits) * 0.04, labels = d$author,
         srt = 40, adj = 1, xpd = TRUE, cex = 0.85)
  })

  # --- Активность по часам ---
  output$activity_by_hour <- renderPlot({
    all_hours <- data.frame(hour = 0:23)
    d <- all_hours %>%
      left_join(activity_by_hour, by = "hour") %>%
      mutate(commits = ifelse(is.na(commits), 0L, commits))
    colors <- ifelse(d$hour <= 5, "#e15759", "#59a14f")
    par(mar = c(4, 4, 3, 1))
    barplot(d$commits, names.arg = d$hour, col = colors,
            main = "Активность по часам суток (UTC)",
            xlab = "Час", ylab = "Количество коммитов",
            border = NA, cex.names = 0.75, cex.axis = 0.85)
    legend("topright",
           legend = c("Рабочее время", "Ночное время (0–5)"),
           fill = c("#59a14f", "#e15759"), border = NA, bty = "n", cex = 0.8)
  })

  # --- Тепловая карта час × день недели ---
  output$activity_heatmap <- renderPlot({
    day_labels <- c("1" = "Вс", "2" = "Пн", "3" = "Вт", "4" = "Ср",
                    "5" = "Чт", "6" = "Пт", "7" = "Сб")
    hm <- commits %>%
      mutate(
        hour = hour(date),
        wday = factor(day_labels[as.character(wday(date))],
                      levels = c("Пн","Вт","Ср","Чт","Пт","Сб","Вс"))
      ) %>%
      group_by(wday, hour) %>%
      summarise(commits = n(), .groups = "drop")

    ggplot(hm, aes(x = hour, y = wday, fill = commits)) +
      geom_tile(color = "white", linewidth = 0.4) +
      scale_fill_gradient(low = "#e8f4fb", high = "#2c7bb6", name = "") +
      scale_x_continuous(breaks = seq(0, 23, 2)) +
      labs(title = "Тепловая карта активности (UTC)", x = "Час", y = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid  = element_blank(),
        plot.title  = element_text(face = "bold", size = 12),
        axis.text.y = element_text(size = 9),
        legend.position = "right"
      )
  })

  # --- Типы коммитов ---
  output$commits_type_chart <- renderPlot({
    d <- commits_by_type
    colors <- TYPE_COLORS[d$type]
    par(mar = c(2, 2, 3, 1))
    pie(d$commits,
        labels  = paste0(d$type, "\n", d$commits),
        col     = colors,
        border  = "white",
        main    = "Типы коммитов",
        cex     = 0.8)
  })

  # --- LoC по авторам ---
  output$loc_chart <- renderPlot({
    d <- loc_stats
    par(mar = c(7, 4, 3, 1))
    bp <- barplot(
      rbind(d$avg_additions, d$avg_deletions),
      beside    = TRUE,
      col       = c("#59a14f", "#e15759"),
      main      = "Среднее LoC на коммит",
      ylab      = "Строк",
      border    = NA,
      cex.axis  = 0.8,
      names.arg = rep("", nrow(d)),
      las       = 2
    )
    text(colMeans(bp), -max(c(d$avg_additions, d$avg_deletions)) * 0.06,
         labels = d$author, srt = 40, adj = 1, xpd = TRUE, cex = 0.8)
    legend("topright", legend = c("Добавлено", "Удалено"),
           fill = c("#59a14f", "#e15759"), border = NA, bty = "n", cex = 0.8)
  })

  # --- Профили разработчиков ---
  output$dev_profiles_table <- renderTable({
    dev_profiles %>%
      rename(
        "Автор"                    = author,
        "Коммитов"                 = total_commits,
        "Активных дней"            = active_days,
        "Коммитов / день"          = avg_commits_per_day,
        "Первый час"               = first_activity_hour,
        "Последний час"            = last_activity_hour
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

  # --- Ночные коммиты ---
  output$night_commits_table <- renderTable({
    if (nrow(night_commits) == 0) {
      data.frame(Сообщение = "Ночных коммитов не обнаружено")
    } else {
      night_commits %>%
        arrange(hour) %>%
        mutate(date = format(date, "%Y-%m-%d %H:%M")) %>%
        select(date, hour, author, message) %>%
        rename("Дата" = date, "Час (UTC)" = hour,
               "Автор" = author, "Сообщение" = message)
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

  # --- Подозрительные авторы ---
  output$suspicious_table <- renderTable({
    df <- suspicious_authors %>%
      filter(!is.na(commits)) %>%
      mutate(z_score = round(z_score, 2)) %>%
      select(author, commits, z_score) %>%
      rename("Автор" = author, "Коммитов" = commits, "Z-оценка" = z_score)
    if (nrow(df) == 0) {
      data.frame(Сообщение = "Подозрительных авторов не обнаружено")
    } else df
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

  # --- Последние коммиты ---
  output$commits_table <- renderTable({
    commits %>%
      arrange(desc(date)) %>%
      head(20) %>%
      mutate(date = format(date, "%Y-%m-%d %H:%M")) %>%
      select(date, author, repo, message) %>%
      rename("Дата" = date, "Автор" = author,
             "Репозиторий" = repo, "Сообщение" = message)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")
}

shinyApp(ui = ui, server = server)
