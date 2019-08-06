library(jsonlite)
library(tidyverse)
p <- fromJSON("https://www.predictit.org/api/marketdata/all/")
p <- as_tibble(p$markets)
p %>%
  filter(name %>% str_detect("Which party will win (.*) in the 2020 presidential election?")) %>%
  mutate(state = shortName %>% str_extract("[:upper:]{2}")) %>%
  select(state, id)

library(rvest)
page <- read_html("https://en.wikipedia.org/wiki/2016_United_States_presidential_election")
table <- page %>%
  html_node("table.wikitable:nth-child(1)") %>%
  html_table(fill = TRUE) %>%
  na_if("–") %>%
  as_tibble(.name_repair = "unique") %>%
  select(1, 3, 6) %>%
  slice(-1, -58, -59) %>%
  set_names(c("state", "dem", "rep")) %>%
  gather(
    -state,
    key = "candidate",
    value = "votes"
  ) %>%
  mutate(votes = parse_number(votes)) %>%
  arrange(state, candidate)

table %>%
  group_by(state) %>%
  arrange(votes) %>%
  slice(1)
