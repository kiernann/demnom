library(jsonlite)
library(tidyverse)
p <- fromJSON("https://www.predictit.org/api/marketdata/all/")
p <- as_tibble(p$markets)
p %>%
  filter(name %>% str_detect("Which party will win (.*) in the 2020 presidential election?")) %>%
  mutate(state = shortName %>% str_extract("[:upper:]{2}")) %>%
  select(state, id)
