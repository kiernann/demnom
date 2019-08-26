Simulating 2020 Using Markets
================

  - [Market Data](#market-data)
  - [Past Data](#past-data)
  - [Probabilities](#probabilities)
  - [Combine Sources](#combine-sources)
  - [Electoral College](#electoral-college)

Election prediction helps party officials, campaign operatives, and
journalists interpret campaigns in a quantitative manner. Uncertainty is
key to a useful election prediction.

The forecast model has become a staple of political punditry.
Popularized by the data journalist at FiveThirtyEight, the forecasting
model is a statistical tool used to incorporate a number of quantitative
inputs and produce a *probabilistic* view of all possible outcomes.

Prediction markets can be used to generate similarly probabilistic views
of election outcomes by utilizing the economic forces of price discovery
and risk aversion to overcome the ideological bias of self-interested
traders on a binary options exchange.

Can we possibly use these prediction markets to generate a useful
probabilistic simulation of the electoral college? We’ll try and use
data from the PredictIt exchange and R code to answer this question.

``` r
if (!require("pacman")) install.packages("pacman")
pacman::p_load_current_gh("kiernann/campfin")
pacman::p_load(
  tidyverse,
  jsonlite,
  magrittr,
  janitor,
  rvest,
  usmap
)
```

## Market Data

PredictIt hosts markets for most of the competitive battleground states.
We can scrape these markets using their API and the
`jsonlite::fromJSON()` function.

``` r
markets_prices <-
  fromJSON(txt = "https://www.predictit.org/api/marketdata/all/") %>%
  use_series(markets) %>%
  filter(str_detect(name, "Which party will win (.*) in the 2020 presidential election?")) %>%
  unnest(contracts, names_repair = make_clean_names) %>%
  filter(short_name_2 == "Democratic") %>%
  select(state = short_name, price = last_close_price) %>%
  mutate(state = str_extract(state, "[:upper:]{2}")) %>% 
  arrange(price)
```

From this API, we get probability data for 15 battleground states.

![](sim_college_files/figure-gfm/map_markets-1.png)<!-- -->

These states alone aren’t enough to simulate the electoral college in
2020. To predict the remaining states, we have a few options. The
easiest route is to simply assume the party which won in 2016 will win
again in 2020. This isn’t a terrible idea, as the majority of states
rarely flip, especially not the 35 states without a prediction market.
We can start from this assumption and improve upon it very easily.

## Past Data

We can scrape the 2016 election results from Wikipedia, where we can
find the percentage of the popular vote as well as the number of
electoral college votes up for grab in each state.

``` r
past_results <-
  read_html("https://en.wikipedia.org/wiki/2016_United_States_presidential_election") %>%
  html_node("table.wikitable:nth-child(1)") %>%
  html_table(fill = TRUE) %>%
  na_if("–") %>%
  as_tibble(.name_repair = "unique") %>%
  select(1, 4, 5, 8) %>%
  slice(-1, -58, -59) %>%
  set_names(c("state", "dem", "dem_votes", "rep_votes")) %>%
  map_dfc(parse_guess) %>% 
  mutate(
    votes = coalesce(dem_votes, rep_votes),
    dem = parse_double(str_remove(dem, "%"))/100,
    state = state %>% 
      str_remove("\\(at-lg\\)") %>% 
      str_remove(",\\s\\d..$") %>% 
      abrev_state()
  ) %>% 
  group_by(state) %>% 
  summarize(
    past = mean(dem),
    votes = sum(votes)
  )
```

![](sim_college_files/figure-gfm/2016_map-1.png)<!-- -->

## Probabilities

Any good election forecast needs to be *probabilistic*. Professional
forecasts take this division of votes (usually from an aggregate of
polls) then calculate the probability distribution around that range
with a series of other factors.

For this simulation, we already have probabilities for 15 states.
PredictIt only hosts markets for the most competitive states. The
reality is, the other 36 contests are fairly noncompetitive. From the
density plot below, we can see how the 2016 popular vote differed for
those states *with* 2020 markets and those without.

![](sim_college_files/figure-gfm/vote_range-1.png)<!-- -->

For those states without a market, we need to convert these vote shares
to probabilities. In reality, a even a 5% edge results in a significant
advantage and a probability difference much greater than 5%.

To make this conversion, we simulate many new election using the 2016
result. These simulated elections are normally distributed around the
2016 results with a standard deviation of 0.5 to account for the
uncertainty that’s developed in the last 3 years. Below you can see the
results of 1,000 simulated elections in Maryland, which voted 60.3% for
the Democratic candidate in the last election. The area under the curve
past 50% is the *probability* of a democrat winning again in the next
election.

![](sim_college_files/figure-gfm/example_range_md-1.png)<!-- -->

Now, lets see the distribution of 1,000 simulated elections in Florida,
a much closer election with only 47.8% of voters supporting the
democratic candidate.

![](sim_college_files/figure-gfm/example_range_fl-1.png)<!-- -->

We can generate this probability by calculating the average number of
simulated elections won by the democrat. Below, we see how this is done
by simulating the Connecticut election 30 times.

``` r
(ex_past <- past_results$past[past_results$state == "CT"])
#> [1] 0.5457
(ex_sims <- round(x = rnorm(n = 30, mean = ex_past, sd = 0.05), digits = 4))
#>  [1] 0.6234 0.5126 0.5509 0.5029 0.4792 0.4658 0.5719 0.5901 0.5819 0.5536 0.4721 0.6597 0.6276
#> [14] 0.5319 0.4987 0.5972 0.5586 0.5693 0.5311 0.5678 0.5069 0.4514 0.5095 0.5266 0.4761 0.6453
#> [27] 0.5827 0.5462 0.6450 0.5887
(ex_wins <- ex_sims > 0.5)
#>  [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE
#> [16]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
mean(ex_wins)
#> [1] 0.8
```

We can perform such random normal simulations for *every* state. We’ll
generate *10,000* simulated elections and calculate the percent of those
simulated elections where the democrat won.

``` r
past_results <- mutate(past_results, prob = NA)
for (i in seq_along(past_results$state)) {
  sims <- rnorm(n = 10000, mean = past_results$past[i], sd = 0.10)
  past_results$prob[i] <- mean(sims > 0.5)
}
```

Below, you can see how the 2016 vote results result in more extreme
probabilities.

![](sim_college_files/figure-gfm/past_results_hist-1.png)<!-- -->

![](sim_college_files/figure-gfm/sim_prob_hist-1.png)<!-- -->

This relationship depends entirely on our chosen standard deviation.
Again, since we are only simulating probabilities for those states
*without* markets, these probabilities tend to be extreme.

![](sim_college_files/figure-gfm/sim_relationship-1.png)<!-- -->

## Combine Sources

The efficient market hypothesis holds that our markets are a more
accurate method to generate probabilistic predictions. We will uses
these market prices over our simulated elections where we have them.

``` r
ec <- past_results %>% 
  left_join(markets_prices, by = "state") %>% 
  mutate(
    dem = coalesce(price, prob),
    market = !is.na(price)
  ) %>% 
  select(state, dem, market, votes)
```

![](sim_college_files/figure-gfm/sim_relationship_market-1.png)<!-- -->

![](sim_college_files/figure-gfm/2020_map-1.png)<!-- -->

## Electoral College

To simulate the entire electoral college, we simple have to perform the
same `sample()` process as we did with Connecticut, above. To simplify
this process, we can create a new `sim_race()` function that takes a
probability and returns a `TRUE` or `FALSE` indicating whether or not
the democrat has won.

``` r
sim_race <- function(dem = 1-rep, rep = 1-dem) {
  sample(
    size = 1,
    x = c(TRUE, FALSE),
    prob = c(dem, rep)
  )
}
```

With this function, we can simulate every state in the country and count
the number of electoral college votes won by each party.

``` r
sim1 <- map_lgl(ec$dem, sim_race)
sum(ec$votes[sim1])
#> [1] 223
```

``` r
sim1_result <- if_else(
  condition = sum(ec$votes[sim1]) > 269,
  true = "the Democrats did win",
  false = "the Democrats did not win"
)
```

In the above election . To best understand the *range* of possible
outcomes, we will perform the same simulation 10,000 times.

``` r
n <- 10000
sims <- rep(NA, n)
for (i in seq(1, n)) {
  state_outcomes <- map_lgl(ec$dem, sim_race)
  dem_total <- sum(ec$votes[state_outcomes])
  sims[i] <- dem_total
}
```

``` r
ggplot(data = NULL) +
  geom_histogram(mapping = aes(x = sims, fill = sims > 270)) +
  geom_vline(xintercept = 270, size = 1) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(8, "RdBu")[c(1, 8)], guide = FALSE) +
  labs(
    title = sprintf("%s Simulated Electoral College Outcomes", scales::comma(n)),
    x = "Simulated Democratic Votes",
    y = "Number of Simulations"
  ) 
```

![](sim_college_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
