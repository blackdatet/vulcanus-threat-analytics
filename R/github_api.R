library(httr)
library(jsonlite)

# Получение списка коммитов из GitHub API
get_github_commits <- function(owner, repo, per_page = 30) {
  url <- paste0(
    "https://api.github.com/repos/",
    owner, "/", repo,
    "/commits?per_page=", per_page
  )

  response <- GET(url)

  if (response$status_code != 200) {
    stop("Ошибка при получении данных из GitHub API")
  }

  data <- content(response, as = "text", encoding = "UTF-8")
  commits <- fromJSON(data, flatten = TRUE)

  return(commits)
}
