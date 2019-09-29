mid <- 3537
span <- "30d"
impeach <-
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
  select(
    date   = Date,
    open   = OpenSharePrice,
    high   = HighSharePrice,
    low    = LowSharePrice,
    close  = CloseSharePrice,
    volume = TradeVolume
  )

brew_set1 <- RColorBrewer::brewer.pal(3, "Set1")[c(3, 1)]

impeach %>%
  ggplot(mapping = aes(x = date)) +
  geom_candlestick(mapping = aes(open = open, high = high, low = low, close = close, size = volume)) +
  scale_fill_manual(values = test) +
  scale_color_manual(values = test) +
  scale_y_continuous(labels = scales::dollar, breaks = seq(0, 1, by = 0.1)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Will Donald Trump be impeached in his first term?",
    subtitle = "Equilibrium price reflects consensus probability",
    caption = "Source: PredictIt.org (Market 3537)",
    x = "Date",
    y = "Price of \"Yes\" Contract"
    )

ggsave(
  filename = "plots/impeachment_30d_2019-09-29.png",
  height = 5,
  width = 9,
  dpi = "retina"
)
