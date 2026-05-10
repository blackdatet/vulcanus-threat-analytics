set.seed(42)

authors <- c("whtadgdn", "blackdatet", "IKhlestkov", "denrouz")
repo    <- "vulcanus-threat-analytics"

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

n              <- 120
author_weights <- c(35, 30, 25, 30)

# 85% дневные (9–22), 15% ночные (0–5)
n_day   <- round(n * 0.85)
n_night <- n - n_day
hours   <- sample(c(
  sample(9:22, n_day,   replace = TRUE),
  sample(0:5,  n_night, replace = TRUE)
))

base        <- as.POSIXct("2025-01-15 09:00:00", tz = "UTC")
offsets_sec <- sort(sample(seq(0, 120 * 86400), n))

dates <- as.POSIXct(
  mapply(function(offset, hour) {
    d <- base + offset
    as.numeric(as.POSIXct(format(d, "%Y-%m-%d"), tz = "UTC")) +
      hour * 3600 + sample(0:59, 1) * 60
  }, offsets_sec, hours),
  origin = "1970-01-01", tz = "UTC"
)

example_commits <- data.frame(
  repo          = repo,
  author        = sample(authors, n, replace = TRUE, prob = author_weights / sum(author_weights)),
  date          = dates,
  message       = sample(messages, n, replace = TRUE),
  additions     = sample(1:500, n, replace = TRUE),
  deletions     = sample(0:200, n, replace = TRUE),
  files_changed = sample(1:20,  n, replace = TRUE),
  stringsAsFactors = FALSE
)

saveRDS(example_commits, file = "data/example_commits.rds")
cat(sprintf("Сохранено %d коммитов в data/example_commits.rds\n", nrow(example_commits)))
