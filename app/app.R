library(shiny)

# Тестовые данные-заглушки
test_commits <- data.frame(
  author  = c("denrouz", "khlestkoff", "ryzenin"),
  commits = c(14, 9, 6),
  repo    = c("vulcanus", "vulcanus", "vulcanus"),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  titlePanel("Vulcanus"),

  fluidRow(
    column(12,
      h4("Активность разработчиков (тестовые данные)"),
      tableOutput("commits_table")
    )
  ),

  hr(),

  fluidRow(
    column(12,
      h4("Коммиты по разработчикам"),
      plotOutput("commits_plot", height = "300px")
    )
  )
)

server <- function(input, output, session) {

  output$commits_table <- renderTable({
    test_commits
  })

  output$commits_plot <- renderPlot({
    barplot(
      height = test_commits$commits,
      names.arg = test_commits$author,
      col = c("#4e79a7", "#f28e2b", "#e15759"),
      main = "Количество коммитов",
      xlab = "Разработчик",
      ylab = "Коммиты",
      border = NA
    )
  })
}

shinyApp(ui = ui, server = server)
