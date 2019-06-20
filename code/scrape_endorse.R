pacman::p_load(
  tidyverse,
  lubridate,
  janitor,
  rvest,
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
  clean_names()

write_csv(
  x = endorse,
  path = here("data", paste(today(), "endorse.csv", sep = "_"))
)
