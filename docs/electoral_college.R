library(rvest)
library(campfin)
library(jsonlite)
library(tidyverse)
library(magrittr)
library(usmap)

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
    past = dem/(dem + rep)
  ) %>%
  select(-dem, -rep) %>%
  arrange(state)

# format state names,
me <- str_which(past_results$state, "Maine")
ne <- str_which(past_results$state, "Nebraska")

state.name <- c(state.name, "District of Columbia")
state.abb <- c(state.abb, "DC")
past_results$state <- abrev_state(past_results$state)

# append ME and NE with district number
for (i in seq_along(me) - 1) {
  past_results$state[me][i + 1] <- paste0("ME", i)
}

for (i in seq_along(ne) - 1) {
  past_results$state[ne][i + 1] <- paste0("NE", i)
}

plot_usmap(data = past_results, values = "prob") +
  scale_fill_gradient2(mid = 0.5)

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
  arrange(desc(price), desc(past))

x$votes[x$state == "ME1"] <- 1
x$votes[x$state == "ME2"] <- 1
x$votes[x$state == "ME0"] <- 2
x$votes[x$state == "NE1"] <- 1
x$votes[x$state == "NE2"] <- 1
x$votes[x$state == "NE3"] <- 1
x$votes[x$state == "NE0"] <- 2

sum(x$votes) == 538

write_csv(x, "data/electoral_college.csv")

library(ggnewscale)

ec <- read_csv("data/electoral_college.csv")

set.seed(5)

ec <- mutate(ec, prob = NA)

for (i in seq_along(ec$prob)) {
  sims <- rnorm(n = 1000000, mean = ec$past[i], sd = 0.10)
  wins <- sims > 0.5
  ec$prob[i] <- round(mean(wins), digits = 5)
}

ec <- ec %>%
  mutate(
    market = !is.na(price),
    prob = coalesce(price, prob)
  ) %>%
  select(state, prob, market, votes)

plot_usmap(data = ec, values = "prob") +
  scale_fill_gradient2(low = "red", high = "blue", midpoint = 0.50)

usa <- map_data("usa")
ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group)) +
  coord_quickmap()

states_ec <-
  left_join(
    x = map_data("state") %>% mutate(region = abrev_state(region)),
    y = ec,
    by = c("region" = "state")
  )

states_base <-
  states_ec %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", alpha = 0) +
  coord_quickmap()

states_base +
  geom_polygon(mapping = aes(fill = prob), data = states_ec %>% filter(!market)) +
  scale_fill_gradient(low = "white", high = "black") +
  new_scale_fill() +
  geom_polygon(mapping = aes(fill = prob), data = states_ec %>% filter(market)) +
  scale_fill_gradient2(low = "red", high = "blue", midpoint = 0.50)
