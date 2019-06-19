pacman::p_load(
  tidyverse,
  lubridate,
  here,
  fs
)

market <-
  paste0(
    "https://www.predictit.org/Resource/DownloadMarketChartData",
    "?marketid=", "3633",
    "&timespan=", "90d"
  ) %>%
  read_csv(
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
  )

dir_create("data")
write_csv(
  x = market,
  path = here("data", paste(today(), "dem-nom-market.csv", sep = "_"))
)

market %>%
  filter(close > 0.03) %>%
  ggplot(mapping = aes(date, close)) +
  geom_line(mapping = aes(color = name)) +
  geom_label(
    data = filter(market, date == max(date)),
    mapping = aes(label = name, y = close)
  )
