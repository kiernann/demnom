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

endorse <-
  read_csv(
    file = "https://projects.fivethirtyeight.com/endorsements-2020-data/endorsements-2020.csv",
    col_types = cols(
      date = col_date(format = ""),
      position = col_character(),
      city = col_character(),
      state = col_character(),
      endorser = col_character(),
      endorsee = col_character(),
      `endorser party` = col_character(),
      source = col_character(),
      order = col_double(),
      category = col_character(),
      body = col_character(),
      district = col_double(),
      points = col_double()
    )
  ) %>%
  clean_names() %>%
  mutate(
    endorsee = endorsee %>%
      word(2, -1) %>%
      to_snake_case()
  )

write_csv(
  x = endorse,
  path = here::here("data", "endorsements", glue("{today()}_endorsements.csv"))
)
