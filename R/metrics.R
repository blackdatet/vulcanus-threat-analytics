# Модуль расчета метрик по Git-коммитам
# Автор: IKhlestkov

library(dplyr)
library(lubridate)

# Количество коммитов на автора
get_commits_per_author <- function(df) {
  df %>%
    group_by(author) %>%
    summarise(commits = n(), .groups = "drop") %>%
    arrange(desc(commits))
}

# Общее количество коммитов
get_total_commits <- function(df) {
  nrow(df)
}

# Активность по часам
get_activity_by_hour <- function(df) {
  df %>%
    mutate(hour = hour(date)) %>%
    group_by(hour) %>%
    summarise(commits = n(), .groups = "drop") %>%
    arrange(hour)
}

# Топ авторов по количеству коммитов
get_top_authors <- function(df, n_authors = 5) {
  get_commits_per_author(df) %>%
    slice_head(n = n_authors)
}

# Среднее количество коммитов в день
get_average_commits_per_day <- function(df) {
  df %>%
    mutate(day = as.Date(date)) %>%
    group_by(day) %>%
    summarise(commits = n(), .groups = "drop") %>%
    summarise(avg_commits_per_day = mean(commits))
}

# Распределение коммитов по дням недели
get_commits_by_weekday <- function(df) {
  df %>%
    mutate(weekday = weekdays(as.Date(date))) %>%
    group_by(weekday) %>%
    summarise(commits = n(), .groups = "drop") %>%
    arrange(desc(commits))
}
