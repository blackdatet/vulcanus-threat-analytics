set.seed(42)

authors <- c("whtadgdn", "blackdatet", "IKhlestkov", "denrouz")
repos   <- c("vulcanus-threat-analytics")

messages <- c(
  "fix: исправлена обработка пустых коммитов",
  "feat: добавлен модуль аномалий",
  "refactor: переработан ETL-пайплайн",
  "docs: обновлён README",
  "feat: добавлен профиль разработчика",
  "fix: корректная обработка дат UTC",
  "feat: визуализация активности по часам",
  "chore: обновлены зависимости",
  "fix: исправлен баг в метриках",
  "feat: добавлена таблица коммитов",
  "refactor: упрощена логика фильтрации",
  "feat: подключены реальные данные",
  "fix: исправлены ночные коммиты",
  "test: добавлены юнит-тесты",
  "feat: docker-compose конфигурация",
  "fix: корректный подсчёт авторов",
  "feat: детектор подозрительных авторов",
  "refactor: модуль github_api переработан",
  "docs: добавлены комментарии к ETL",
  "feat: инициализация структуры пакета"
)

n <- 120

author_weights <- c(35, 30, 25, 30)
authors_sample <- sample(authors, n, replace = TRUE,
                         prob = author_weights / sum(author_weights))

base_date <- as.POSIXct("2025-01-15 09:00:00", tz = "UTC")

hours_normal  <- sample(9:22, n * 0.85, replace = TRUE)
hours_night   <- sample(0:5,  n * 0.15, replace = TRUE)
hours_all     <- sample(c(hours_normal, hours_night), n)

dates <- base_date +
  sort(sample(0:(120 * 86400), n)) +
  as.difftime(hours_all - 12, units = "hours")

example_commits <- data.frame(
  repo          = sample(repos, n, replace = TRUE),
  author        = authors_sample,
  date          = dates,
  message       = sample(messages, n, replace = TRUE),
  additions     = as.integer(sample(1:500, n, replace = TRUE)),
  deletions     = as.integer(sample(0:200, n, replace = TRUE)),
  files_changed = as.integer(sample(1:20,  n, replace = TRUE)),
  stringsAsFactors = FALSE
)

save(example_commits, file = "data/example_commits.rda")
cat("Сохранено", nrow(example_commits), "коммитов в data/example_commits.rda\n")
