# Модуль получения данных из GitHub API
# Автор: denrouz

# Модуль получения данных GitHub API
# Возвращает информацию о коммитах репозитория

library(httr)
library(jsonlite)

# Получение списка коммитов из GitHub API
get_github_commits <- function(owner, repo, per_page = 30) {

  url <- paste0(
    "https://api.github.com/repos/",
    owner, "/", repo,
    "/commits?per_page=", per_page
  )

  response <- tryCatch(
    GET(url),
    error = function(e) {
      stop("Ошибка подключения к GitHub API")
    }
  )

  if (response$status_code != 200) {
    stop("GitHub API вернул ошибку")
  }

  data <- content(response, as = "text", encoding = "UTF-8")

  if (nchar(data) == 0) {
    stop("Пустой ответ от GitHub API")
  }

  commits <- fromJSON(data, flatten = TRUE)

  return(commits)
}

# Получение готового датафрейма коммитов
get_clean_commits <- function(owner, repo) {

  raw_commits <- get_github_commits(owner, repo)

  clean_commits <- transform_commits(
    commits = raw_commits,
    repo_name = paste0(owner, "/", repo)
  )

  return(clean_commits)
}
