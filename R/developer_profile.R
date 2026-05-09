# Модуль профиля разработчика
# Автор: IKhlestkov

library(dplyr)
library(lubridate)

# Формирование профиля разработчика
get_developer_profile <- function(df) {

  profile <- df %>%
    mutate(
      day = as.Date(date),
      hour = hour(date)
    ) %>%
    group_by(author) %>%
    summarise(
      total_commits = n(),
      active_days = n_distinct(day),
      avg_commits_per_day = round(n() / n_distinct(day), 2),
      first_activity_hour = min(hour, na.rm = TRUE),
      last_activity_hour = max(hour, na.rm = TRUE),
      .groups = "drop"
    )

  return(profile)
}
