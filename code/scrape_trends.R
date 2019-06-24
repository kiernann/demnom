pacman::p_load(
  tidyverse,
  lubridate,
  trendyy,
  janitor,
  rvest,
  here,
  fs
)

candidates <-
  read_html("https://en.wikipedia.org/wiki/2020_Democratic_Party_presidential_primaries") %>%
  html_node("table.wikitable:nth-child(22)") %>%
  html_table() %>%
  pull(Name)

trends <-
  trendy(
    search_terms = candidates,
    from = floor_date(today(), "year"),
    to = today()
  ) %>%
  get_interest()

write_csv(
  x = trends,
  path = here::here("data", "trends", paste(today(), "trends.csv", sep = "_"))
)
