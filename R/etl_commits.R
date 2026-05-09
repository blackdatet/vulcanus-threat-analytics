# Модуль преобразования данных коммитов в датафрейм
# Автор: denrouz

library(dplyr)

# Преобразование данных GitHub API в аккуратный датафрейм
transform_commits <- function(commits, repo_name) {

  if (is.null(commits) || nrow(commits) == 0) {
    stop("Нет данных для преобразования")
  }

  df <- commits %>%
    transmute(
      repo = repo_name,
      author = commit.author.name,
      date = as.POSIXct(commit.author.date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      message = commit.message,
      additions = if ("stats.additions" %in% names(commits)) stats.additions else NA_integer_,
      deletions = if ("stats.deletions" %in% names(commits)) stats.deletions else NA_integer_,
      files_changed = if ("files" %in% names(commits)) lengths(files) else NA_integer_
    )

  return(df)
}
