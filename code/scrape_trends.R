pacman::p_load(
  tidyverse,
  lubridate,
  snakecase,
  trendyy,
  janitor,
  rvest,
  glue,
  here,
  fs
)

candidates <-
  read_html("https://en.wikipedia.org/wiki/2020_Democratic_Party_presidential_primaries") %>%
  html_node("table.wikitable:nth-child(23)") %>%
  html_table() %>%
  pull(Name) %>%
  unique() %>%
  word(2, -1) %>%
  to_snake_case()

trends <-
  trendy(
    search_terms = candidates,
    from = floor_date(today(), "year"),
    to = today()
  ) %>%
  get_interest()

write_csv(
  x = trends,
  path = here("data", "trends", glue("{today()}_trends.csv"))
)
