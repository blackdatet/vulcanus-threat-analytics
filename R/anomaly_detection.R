# Модуль поиска аномалий
# Автор: IKhlestkov

library(dplyr)
library(lubridate)

# Поиск ночных коммитов
detect_night_commits <- function(df) {

  df %>%
    mutate(hour = hour(date)) %>%
    filter(hour >= 0 & hour <= 5)
}

# Поиск авторов с аномально высокой активностью
detect_high_activity <- function(df) {

  stats <- df %>%
    group_by(author) %>%
    summarise(commits = n(), .groups = "drop")

  avg_commits <- mean(stats$commits)
  sd_commits <- sd(stats$commits)

  stats %>%
    mutate(
      z_score = (commits - avg_commits) / sd_commits
    ) %>%
    filter(z_score > 1)
}

# Общий список подозрительных авторов
detect_suspicious_authors <- function(df) {

  high_activity <- detect_high_activity(df)

  night_commits <- detect_night_commits(df) %>%
    distinct(author)

  suspicious <- full_join(
    high_activity,
    night_commits,
    by = "author"
  )

  return(suspicious)
}
