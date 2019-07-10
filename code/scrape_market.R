pacman::p_load(
  tidyverse,
  lubridate,
  snakecase,
  here,
  glue,
  fs
)
mid <- 3633
span <- "90d"
market <-
  read_csv(
    file = glue("https://www.predictit.org/Resource/DownloadMarketChartData?marketid={mid}&timespan={span}"),
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
  x = market,
  path = here::here("data", "markets", glue("{today()}_markets.csv"))
)

