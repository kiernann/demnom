library(rvest)
library(jsonlite)
library(tidyverse)

# get past election results
past_results <-
  read_html("https://en.wikipedia.org/wiki/2016_United_States_presidential_election") %>%
  html_node("table.wikitable:nth-child(1)") %>%
  html_table(fill = TRUE) %>%
  na_if("–") %>%
  as_tibble(.name_repair = "unique") %>%
  select(1, 3, 6) %>%
  slice(-1, -58, -59) %>%
  set_names(c("state", "dem", "rep")) %>%
  mutate(
    dem = parse_number(dem),
    rep = parse_number(rep),
    dem = dem/(dem + rep)
  ) %>%
  select(-rep) %>%
  arrange(state)

# format state names,
me <- str_which(past_results$state, "Maine")
ne <- str_which(past_results$state, "Nebraska")

state.name <- c(state.name, "District of Columbia")
state.abb <- c(state.abb, "DC")
past_results$state <- state2abbr(past_results$state)

# append ME and NE with district number
for (i in seq_along(me) - 1) {
  past_results$state[me][i + 1] <- paste0("ME", i)
}

for (i in seq_along(ne) - 1) {
  past_results$state[ne][i + 1] <- paste0("NE", i)
}

# scrape PredictIt markets on battleground states
all_markets <- as_tibble(fromJSON("https://www.predictit.org/api/marketdata/all/")$markets)
ec_markets <- all_markets %>%
  filter(name %>% str_detect("Which party will win (.*) in the 2020 presidential election?")) %>%
  mutate(state = shortName %>% str_extract("[:upper:]{2}")) %>%
  select(state, id)

build_url <- function(id, span = c("24h", "7d", "30d", "90d")) {
  glue("https://www.predictit.org/Resource/DownloadMarketChartData?marketid={id}&timespan={span}")
}

markets_data <- map(
  .x = build_url(ec_markets$id, "7d"),
  .f = read_csv,
  col_types = cols(
    ContractName = col_character(),
    Date = col_date("%m/%d/%Y %H:%M:%S %p"),
    OpenSharePrice = col_number(),
    HighSharePrice = col_number(),
    LowSharePrice = col_number(),
    CloseSharePrice = col_number(),
    TradeVolume = col_number()
  )
)

bind_rows(markets_data) %>%
  mutate(MarketID = rep(ec_markets$id, each = 14)) %>%
  filter(ContractName == "Democratic") %>%
  select(
    id = MarketID,
    close = CloseSharePrice,
    vol = TradeVolume
  )

x <- all_markets %>%
  filter(name %>% str_detect("Which party will win (.*) in the 2020 presidential election?"))

ec_markets <-
  fromJSON("https://www.predictit.org/api/marketdata/all/") %>%
  use_series(markets) %>%
  as_tibble() %>%
  filter(name %>% str_detect("Which party will win (.*) in the 2020 presidential election?")) %>%
  unnest(contracts, names_repair = "unique") %>%
  filter(shortName...11 == "Democratic") %>%
  select(state = shortName...3, price = lastTradePrice) %>%
  mutate(state = str_extract(state, "[:upper:]{2}"))
