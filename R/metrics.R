library(dplyr)

# количество коммитов на автора
get_commits_per_author <- function(df) {
  df %>%
    group_by(author) %>%
    summarise(commits = n())
}

# общее количество коммитов
get_total_commits <- function(df) {
  nrow(df)
}

# активность по часам
get_activity_by_hour <- function(df) {
  df %>%
    mutate(hour = format(date, "%H")) %>%
    group_by(hour) %>%
    summarise(commits = n())
}
