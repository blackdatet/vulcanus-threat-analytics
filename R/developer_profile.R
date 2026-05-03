library(dplyr)

get_developer_profile <- function(df) {
  df %>%
    group_by(author) %>%
    summarise(
      total_commits = n(),
      avg_commits_per_day = n() / n_distinct(as.Date(date))
    )
}
