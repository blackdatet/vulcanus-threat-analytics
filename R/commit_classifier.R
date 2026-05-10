library(dplyr)

classify_commit_type <- function(message) {
  msg <- tolower(trimws(message))
  dplyr::case_when(
    grepl("^feat|^add|^new",                          msg) ~ "feature",
    grepl("^fix|^bug|^hotfix|^patch",                 msg) ~ "fix",
    grepl("^doc|^readme|^comment",                    msg) ~ "docs",
    grepl("^refactor|^clean|^style|^format",          msg) ~ "refactor",
    grepl("^test|^spec",                              msg) ~ "test",
    grepl("^chore|^config|^ci|^docker|^build|^deps",  msg) ~ "config",
    TRUE                                                   ~ "other"
  )
}

get_commits_by_type <- function(df) {
  df %>%
    mutate(type = classify_commit_type(message)) %>%
    group_by(type) %>%
    summarise(commits = n(), .groups = "drop") %>%
    arrange(desc(commits))
}

get_loc_stats <- function(df) {
  df %>%
    group_by(author) %>%
    summarise(
      total_additions = sum(additions,    na.rm = TRUE),
      total_deletions = sum(deletions,    na.rm = TRUE),
      avg_additions   = round(mean(additions,    na.rm = TRUE), 1),
      avg_deletions   = round(mean(deletions,    na.rm = TRUE), 1),
      avg_files       = round(mean(files_changed, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(total_additions))
}
