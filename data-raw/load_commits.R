source("R/github_api.R")
source("R/etl_commits.R")

# Загрузка примерных данных из публичного GitHub-репозитория
example_commits <- get_clean_commits(
  owner = "torvalds",
  repo = "linux"
)

save(example_commits, file = "data/example_commits.rda")
