pacman::p_load(
  tidyverse,
  lubridate,
  snakecase,
  janitor,
  rvest,
  glue,
  here,
  fs
)

forecast <-
  read_csv(
    file = "https://projects.fivethirtyeight.com/2020-primary-data/primary_forecast_2020.csv",
    col_types = cols(
      .default = col_double(),
      race = col_character(),
      state = col_character(),
      district = col_character(),
      forecastdate = col_date("%m/%d/%Y"),
      candidate_name = col_character()
    )
  )

write_csv(
  x = forecast,
  path = here::here("data", "forecast", glue("{today()}_forecasts.csv"))
)
