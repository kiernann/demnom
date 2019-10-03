library(tidyverse)
library(janitor)
library(readxl)
library(campfin)
library(glue)

closed_markets <- read_excel(
  path = "~/Documents/PI Closed Markets List.xlsx",
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
  path = "docs/market_calib/market_history.csv",
  na = ""
)

outcomes <- market_history %>%
  group_by(id, contract) %>%
  arrange(desc(date)) %>%
  slice(1) %>%
  mutate(
    yes = close >= 0.98,
    no = close <= 0.2
  ) %>%
  select(id, contract, yes)

market_calib <-
  left_join(market_history, outcomes) %>%
  mutate(bucket = round(close * 20) / 20) %>%
  group_by(bucket) %>%
  summarise(prop = mean(yes), n = n()) %>%
  mutate(source = "market") %>%
  select(source, everything())

zip_file <- fs::file_temp()
zip_url <- "https://github.com/fivethirtyeight/checking-our-work-data/raw/master/raw_forecasts.zip"

download.file(zip_url, destfile = tmp_file)
unzip(tmp_file, exdir = fs::path_temp())
csv_file <- fs::dir_ls(fs::path_temp(), glob = "*raw_forecasts.csv")

model <- read_csv(
  file = csv_file,
  col_types = cols(
    .default = col_guess()
  )
)

model$outcome <- as.logical(model$outcome)

model_calib <- model %>%
  filter(topic == "politics") %>%
  group_by(bucket) %>%
  summarize(prop = mean(outcome), n = n()) %>%
  mutate(source = "model") %>%
  select(source, everything())

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

bind_rows(
  market_calib,
  model_calib
) %>%
  ggplot(aes(x = bucket, y = prop)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_point(aes(color = source, size = n), alpha = 0.75) +
  scale_size_continuous(range = c(7, 12), guide = FALSE) +
  geom_label(mapping = aes(label = "Underconfident", x = 0.25, y = 0.75),
             label.size = 0,
             fill = "#ebebeb",
             size = 6) +
  geom_label(mapping = aes(label = "Overconfident", x = 0.75, y = 0.25),
             label.size = 0,
             fill = "#ebebeb",
             size = 6) +
  scale_color_manual(values = c("#07A0BB", "#ED713A")) +
  scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(labels = scales::percent,  breaks = seq(0, 1, by = 0.1)) +
  theme(legend.position = "bottom") +
  labs(
    title = "Comparing Forecast Calibrations",
    x = "Predicted Probability",
    y = "Actual Occurance",
    color = "Prediction five"
  )

ggsave(
  filename = "docs/market_calib/calib.png",
  plot = last_plot(),
  height = 5,
  width = 9,
  dpi = "retina"
)
