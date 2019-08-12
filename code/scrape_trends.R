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
  html_node(".wikitable") %>%
  html_table() %>%
  pull(Name) %>%
  unique()

trends <-
  trendy(
    search_terms = candidates,
    from = floor_date(today(), "year"),
    to = today()
  ) %>%
  get_interest() %>%
  mutate(keyword = to_snake_case(word(keyword, 2, -1)))

write_csv(
  x = trends,
  path = here::here("data", "trends", glue("{today()}_trends.csv"))
)
