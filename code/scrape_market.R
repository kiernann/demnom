pacman::p_load(
  tidyverse,
  lubridate,
  snakecase,
  janitor,
  here,
  glue,
  fs
)

# dem nom ------------------------------------------------------------------------------------

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

# swing state general ------------------------------------------------------------------------

college_markets <-
  fromJSON(txt = "https://www.predictit.org/api/marketdata/all/") %>%
  use_series(markets) %>%
  filter(str_detect(name, "Which party will win (.*) in the 2020 presidential election?"))


college_states <- str_extract(college_markets$shortName, "[:upper:]{2}")

college_urls <- glue("https://www.predictit.org/Resource/DownloadMarketChartData?marketid={college_markets$id}&timespan=90d")

college_data <- map(
  college_urls,
  read_csv,
  col_types = cols(
    ContractName = col_character(),
    Date = col_date("%m/%d/%Y %H:%M:%S %p"),
    OpenSharePrice = col_number(),
    HighSharePrice = col_number(),
    LowSharePrice = col_number(),
    CloseSharePrice = col_number(),
    TradeVolume = col_double()
  )
)

college_data <- college_data %>%
  set_names(value = college_states) %>%
  bind_rows(.id = "state") %>%
  rename(
    name   = ContractName,
    date   = Date,
    open   = OpenSharePrice,
    high   = HighSharePrice,
    low    = LowSharePrice,
    close  = CloseSharePrice,
    volume = TradeVolume
  )

write_csv(
  x = college_data,
  path = here::here("data", "markets", glue("{today()}_college.csv"))
)
