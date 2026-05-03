source("R/github_api.R")
source("R/etl_commits.R")

# Пример загрузки данных из публичного GitHub-репозитория
commits_raw <- get_github_commits(
  owner = "torvalds",
  repo = "linux",
  per_page = 30
)

example_commits <- transform_commits(
  commits = commits_raw,
  repo_name = "torvalds/linux"
)

save(example_commits, file = "data/example_commits.rda")
