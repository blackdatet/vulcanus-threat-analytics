# Запуск проекта Vulcanus

## Локальный запуск

```r
shiny::runApp("app")
```

## Docker

```bash
docker build -t vulcanus .
docker run -p 3838:3838 vulcanus
```

## Docker Compose

```bash
docker compose up --build
```
