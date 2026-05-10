import pyreadr
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import random

random.seed(42)
np.random.seed(42)

authors = ["whtadgdn", "blackdatet", "IKhlestkov", "denrouz"]
repo = "vulcanus-threat-analytics"

messages = [
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
    "feat: инициализация структуры пакета",
]

n = 120
author_weights = [35, 30, 25, 30]
total_w = sum(author_weights)
author_probs = [w / total_w for w in author_weights]

# 85% дневные (9-22), 15% ночные (0-5)
n_day   = int(n * 0.85)
n_night = n - n_day
hours = (
    [random.randint(9, 22) for _ in range(n_day)] +
    [random.randint(0, 5)  for _ in range(n_night)]
)
random.shuffle(hours)

base = datetime(2025, 1, 15, 9, 0, 0)
offsets_sec = sorted(random.sample(range(120 * 86400), n))

dates = []
for i in range(n):
    d = base + timedelta(seconds=offsets_sec[i])
    d = d.replace(hour=hours[i], minute=random.randint(0, 59))
    dates.append(d)

df = pd.DataFrame({
    "repo":          [repo] * n,
    "author":        random.choices(authors, weights=author_probs, k=n),
    "date":          pd.to_datetime(dates),
    "message":       [random.choice(messages) for _ in range(n)],
    "additions":     np.random.randint(1, 501, n).astype("int32"),
    "deletions":     np.random.randint(0, 201, n).astype("int32"),
    "files_changed": np.random.randint(1, 21,  n).astype("int32"),
})

pyreadr.write_rds("data/example_commits.rds", df)
print(f"Сохранено {len(df)} коммитов в data/example_commits.rds")
