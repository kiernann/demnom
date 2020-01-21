pacman::p_load(
  tidyverse,
  lubridate,
  janitor,
  rvest,
  glue,
  here,
  fs
)

market <-
  read_csv(
    file = "https://projects.fivethirtyeight.com/polls-page/president_primary_polls.csv",
    col_types = cols(
      ContractName = col_character(),
      Date = col_date("%m/%d/%Y %H:%M:%S %p"),
      OpenSharePrice = col_number(),
      HighSharePrice = col_number(),
      LowSharePrice = col_number(),
      CloseSharePrice = col_number(),
      TradeVolume = col_double()
    )
  ) %>%
  rename(
    name   = ContractName,
    date   = Date,
    open   = OpenSharePrice,
    high   = HighSharePrice,
    low    = LowSharePrice,
    close  = CloseSharePrice,
    volume = TradeVolume
  ) %>%
  mutate(name = to_snake_case(name))

write_csv(
  x = polls,
  path = here::here("data", "polling", glue("{today()}_polls.csv"))
)
