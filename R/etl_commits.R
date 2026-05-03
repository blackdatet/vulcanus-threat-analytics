# Модуль преобразования данных коммитов в датафрейм
# Автор: denrouz
library(dplyr)

# Преобразование данных GitHub API в аккуратный датафрейм
transform_commits <- function(commits, repo_name) {
  df <- commits %>%
    transmute(
      repo = repo_name,
      author = commit.author.name,
      date = as.POSIXct(commit.author.date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      message = commit.message,
      additions = NA_integer_,
      deletions = NA_integer_,
      files_changed = NA_integer_
    )

  return(df)
}
