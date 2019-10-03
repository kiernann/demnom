library(tidyverse)
library(janitor)
library(readxl)
library(campfin)
library(glue)

closed_markets <- read_excel(
  path = "~/Downloads/PI Closed Markets List.xlsx",
  col_types = c(rep("text", 4), rep("date", 2)),
  skip = 1
)

closed_markets <- closed_markets %>%
  clean_names("snake") %>%
  remove_empty("cols") %>%
  rename(
    start = start_date_et,
    end = closed_date_et
  )

election_markets <- closed_markets %>%
  filter(
    str_detect(market, rx_break("win")) |
      str_detect(market, rx_break("election"))
  )

scrape_market <- function(mid = NULL, span = "90d") {
  murl <- glue::glue("https://www.predictit.org/Resource/DownloadMarketChartData?marketid={mid}&timespan={span}")
  market <-
    readr::read_csv(
      file = murl,
      col_types = readr::cols(
        ContractName = readr::col_character(),
        Date = readr::col_date("%m/%d/%Y %H:%M:%S %p"),
        OpenSharePrice = readr::col_number(),
        HighSharePrice = readr::col_number(),
        LowSharePrice = readr::col_number(),
        CloseSharePrice = readr::col_number(),
        TradeVolume = readr::col_double()
      )
    ) %>%
    dplyr::rename(
      contract = ContractName,
      date = Date,
      open = OpenSharePrice,
      high = HighSharePrice,
      low = LowSharePrice,
      close = CloseSharePrice,
      vol = TradeVolume
    ) %>%
    dplyr::mutate(id = as.character(mid)) %>%
    dplyr::select(id, dplyr::everything())
  return(market)
}

scrape_market(4444)

market_history <- tibble()
for (id in election_markets$id) {
  market_history <- bind_rows(
    market_history,
    scrape_market(id)
  )
}

market_history <- right_join(
  election_markets,
  market_history
)

write_csv(
  x = market_history,
  path = "~/Downloads/election_market_history.csv",
  na = ""
)

m <- read_csv(
  file = "~/Downloads/election_market_history.csv",
  col_types = cols(
    .default = col_guess(),
    id = col_character()
  )
)

x <- m %>%
  select(
    id,
    ticker,
    contract,
    date,
    close,
    vol
  )

outcomes <- x %>%
  group_by(id, contract) %>%
  arrange(desc(date)) %>%
  slice(1) %>%
  mutate(
    yes = close >= 0.98,
    no = close <= 0.2
  ) %>%
  select(id, contract, yes)

x <-
  left_join(x, outcomes) %>%
  mutate(bucket = round(close * 20) / 20) %>%
  group_by(bucket) %>%
  summarise(prop = mean(yes), n = n())

download.file(
  url = "https://github.com/fivethirtyeight/checking-our-work-data/raw/master/raw_forecasts.zip",
  destfile = "~/Downloads/raw_forecasts.zip"
)

unzip("~/Downloads/raw_forecasts.zip")

model <- read_csv(
  file = "~/Downloads/raw_forecasts.csv",
  col_types = cols(
    .default = col_guess(),
    outcome = col_logical()
  )
)

y <- model %>%
  group_by(bucket) %>%
  summarize(prop = mean(outcome), n = n())

color_model  <-  # 538 brand color
color_market <-  # PredictIt brand color

calib_plot <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_point(data = x, aes(x = bucket, y = prop, size = n), color = "#07A0BB", alpha = 0.75) +
  geom_point(data = y, aes(x = bucket, y = prop, size = n), color = "#ED713A", alpha = 0.75) +
  scale_size_continuous(range = c(7, 12), guide = FALSE) +
  labs(
    title = "Comparing Forecast Calibration"
  )

ggsave(
  filename = "~/Pictures/calib.png",
  plot = calib_plot,
  height = 5,
  width = 9,
  dpi = "retina"
)
