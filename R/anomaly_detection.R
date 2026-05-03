library(dplyr)

# коммиты ночью
detect_night_commits <- function(df) {
  df %>%
    mutate(hour = as.numeric(format(date, "%H"))) %>%
    filter(hour >= 0 & hour <= 5)
}

# слишком частые коммиты (условно)
detect_high_activity <- function(df) {
  df %>%
    group_by(author) %>%
    summarise(commits = n()) %>%
    filter(commits > mean(commits) * 2)
}
