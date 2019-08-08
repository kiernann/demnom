library(rvest)
library(campfin)
library(openintro)
library(jsonlite)
library(tidyverse)
library(magrittr)

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
ec_markets <-
  fromJSON("https://www.predictit.org/api/marketdata/all/") %>%
  use_series(markets) %>%
  as_tibble() %>%
  filter(name %>% str_detect("Which party will win (.*) in the 2020 presidential election?")) %>%
  unnest(contracts, names_repair = "unique") %>%
  filter(shortName...11 == "Democratic") %>%
  select(state = shortName...3, price = lastTradePrice) %>%
  mutate(state = str_extract(state, "[:upper:]{2}"))

# compare
left_join(past_results, ec_markets)

# get votes
ec_votes <-
  read_html("https://en.wikipedia.org/wiki/United_States_Electoral_College") %>%
  html_node("table.wikitable:nth-child(122)") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  as_tibble(.name_repair = "unique") %>%
  slice(4:54) %>%
  select(2, 36) %>%
  na_if("") %>%
  set_names(c("state", "votes")) %>%
  mutate(
    state = abrev_state(str_replace(state, "D.C.", "District of Columbia")),
    votes = as.double(votes)
  )

# compare
x <- past_results %>%
  left_join(ec_markets) %>%
  left_join(ec_votes) %>%
  mutate(
    price = if_else(!is.na(price), price, if_else(dem > 0.5, 1.0, 0.0)),
    dem = price > 0.5
  ) %>%
  arrange(votes)
