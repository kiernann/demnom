Simulating the 2020 Election With Markets
================

  - [Code](#code)
  - [Market Data](#market-data)
  - [Past Data](#past-data)
  - [Probabilities](#probabilities)
  - [Combine Sources](#combine-sources)
  - [Electoral College](#electoral-college)

Election prediction helps party officials, campaign operatives, and
journalists interpret campaigns in a quantitative manner. In the past
few years, the forecasting model has become a staple of political
punditry as big data has sought to supplant arbitrary punditry.
Popularized by the data journalist at FiveThirtyEight, the forecasting
model is a statistical tool used to incorporate a number of quantitative
inputs and produce a *probabilistic* view of all possible outcomes.

However, following the 2016 Presidential election, the public
([wrongly](http://53eig.ht/2fIYJK2)) felt betrayed by promise of data to
predict the future. This left political scientists and journalists alike
reassessing other predictive tools. My favorite of these alternatives is
the prediction market.

Prediction markets can be used to generate similarly probabilistic views
of election outcomes by utilizing the economic forces of price discovery
and risk aversion to overcome the ideological bias of self-interested
traders on a binary options exchange. Traders use real money to buy
shares of [futures
contracts](https://en.wikipedia.org/wiki/Futures_contract) tied to an
outcome. The price of these shares fluctuates on the market as the
underlying *likelihood* of that outcome changes.
[PredictIt](https://www.predictit.org/) is an exchange for such
contracts, run by Victoria University of Wellington.

Following the 2018 Midterm elections, [I wrote a
paper](https://github.com/kiernann/models-markets) comparing these
markets to the congressional model published by the data journalists at
FiveThirtyEight. I found no statistical difference in the two method’s
ability to make [skilled
predictions](https://en.wikipedia.org/wiki/Brier_score) over the course
of the Midterm elections. In fact, the markets showed reasonable
skepticism in a number of upset elections.

![](https://raw.githubusercontent.com/kiernann/models-markets/master/plots/plot_cartesian.png)

With the 2020 Presidential race well under way, the media, voters,
campaigns, and political scientists alike are all looking for the best
way to provide useful predictions and avoid the pitfalls of 2016. This
far from the General Election, what little polling we have [is less than
useless](https://53eig.ht/2IFHxVW). In the absense of more quantitative
data, can we possibly use prediction markets to generate a useful
probabilistic simulation of the electoral college? today I’ll try and
use data from the PredictIt exchange to answer this question.

## Code

I’ll be using the open source [language R](https://www.r-project.org/)
and packages from the [Tidyverse ecosystem](https://www.tidyverse.org/).

``` r
if (!require("pacman")) install.packages("pacman")
pacman::p_load_current_gh("kiernann/campfin")
pacman::p_load(
  tidyverse,
  magrittr,
  jsonlite,
  janitor,
  scales,
  rvest,
  usmap
)
```

## Market Data

PredictIt hosts markets for most of the competitive battleground states.
We can scrape these markets using their API and the
`jsonlite::fromJSON()` function.

``` r
market_prices <-
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
  # scrape table
  read_html("https://en.wikipedia.org/wiki/2016_United_States_presidential_election") %>%
  html_node("table.wikitable:nth-child(1)") %>%
  html_table(fill = TRUE) %>%
  na_if("–") %>%
  as_tibble(.name_repair = "unique") %>%
  # select columns and rows
  select(1, 3, 5, 6, 8) %>%
  slice(-1, -58, -59) %>%
  set_names(c("state", "dem", "dem_votes", "rep", "rep_votes")) %>%
  # parse numeric cols
  map_dfc(parse_guess) %>%
  mutate(
    # coalesce EC votes won by both
    votes = coalesce(dem_votes, rep_votes),
    # calculate the dem share
    dem_prop = dem/(dem + rep),
    # abbreviate the state names
    state = state %>% 
      str_remove("\\(at-lg\\)") %>% 
      str_remove(",\\s\\d..$") %>% 
      abrev_state()
  ) %>% 
  # use mean for ME and NE
  group_by(state) %>% 
  summarize(
    past = mean(dem_prop),
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
to probabilities. In reality, a even a 5% edge in the popular vote
results in a significant advantage and a probability difference much
greater than 5%.

To make this conversion, we simulate many new election using the 2016
result. We make these simulations using a normal distribution around the
2016 result. The standard deviation of such a distribution is where
uncertainty is introduced, converting our popular vote to a probability.

More complicated forecasting models like those released by
FiveThirtyEight use proprietary models to calculate this degree of
uncertainty. We know these models incorperate a number of historically
predictive variables (partisanship, fundraising, incumbency, etc). For
this simple demonstration, I will calculate the standard deviation of
the past 11 Presidential elections in each state.

The MIT Dataverse contains a dataset of Presidential contests since 1976
with party vote counts in each state. We can read the file using
`readr::read_tsv()` and calulating the standard deviation with
`dplyr::group_by()` and `dplyr::summarize()`.

``` r
mit_url <- "https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/42MVDX/MFU99O"
# MIT Election Data and Science Lab, 2017, "1976-2016-president.tab"
# https://doi.org/10.7910/DVN/42MVDX/MFU99O, Harvard Dataverse, V5

potus <- read_delim(
    file = mit_url,
    delim = "\t",
    escape_double = FALSE,
    escape_backslash = TRUE
  )
```

``` r
states_sd <- potus %>%
  filter(candidatevotes > 1000) %>% 
  filter(party %in% c("democrat")) %>%
  mutate(prop = candidatevotes/totalvotes,) %>%
  select(year, state = state_po, prop) %>%
  group_by(state) %>%
  summarize(sd = sd(prop))
```

Standard deviation is the variation in election results. A state with
little change year to year will have a lower standard deviation,
increasing our confidence and decreasing uncertainty in the next
election/.

``` r
left_join(usa_map, states_sd) %>%
  filter(state != "DC") %>% 
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", mapping = aes(fill = sd)) +
  coord_quickmap() +
  scale_fill_distiller(
    type = "seq", palette = 5, direction = 1
  ) +
  theme(
    legend.position = c(0.9, 0.35),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text  = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Variation in Presidential Elections 1976-2016",
    fill = "Standard\nDeviation"
    )
```

![](sim_college_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

Using this standard deviation, we can generate 10,000 values from this
random normal distribution. We can think of each of these values as a
simulated election. The percentage of simulated elections won is the
same as the *probability* of winning.

``` r
past_results <- past_results %>% 
  left_join(states_sd) %>%
  rowwise() %>% 
  mutate(prob = mean(rnorm(10000, past, sd) > 0.50))
```

If we visualize this process, we can see how the 2016 result and a
standard deviation is used to simulate many elections and calculate a
probability.

Below you can see the results of 1,000 simulated elections in Maryland,
where 64.0% of votes were cast for the Democratic candidate in the last
election. The area under the curve past 50% is the *probability* of a
democrat winning again in the next election.

![](sim_college_files/figure-gfm/example_range_md-1.png)<!-- -->

Now, lets see the distribution of 1,000 simulated elections in Florida,
a much closer election with only 49.4% of voters supporting the
democratic candidate.

![](sim_college_files/figure-gfm/example_range_fl-1.png)<!-- -->

We can generate this probability by calculating the average number of
simulated elections won by the democrat. Below, we see how this is done
by simulating the Connecticut election 30 times.

``` r
(ex_past <- past_results$past[past_results$state == "CT"])
#> [1] 0.5714155
(ex_sd <- past_results$sd[past_results$state == "CT"])
#> [1] 0.07728929
(ex_sims <- round(x = rnorm(n = 30, mean = ex_past, sd = ex_sd), digits = 2))
#>  [1] 0.56 0.62 0.63 0.62 0.53 0.59 0.55 0.60 0.43 0.62 0.54 0.58 0.61 0.62 0.48 0.63 0.58 0.64 0.62
#> [20] 0.70 0.50 0.59 0.41 0.58 0.63 0.55 0.50 0.72 0.57 0.49
(ex_wins <- ex_sims > 0.5)
#>  [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
#> [16]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE FALSE
mean(ex_wins)
#> [1] 0.8
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
  left_join(market_prices, by = "state") %>% 
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
#> [1] 256
```

``` r
sim1_result <- if_else(
  condition = sum(ec$votes[sim1]) > 269,
  true = "the Democrats did win",
  false = "the Democrats did not win"
)
```

In the above election, the Democrats did not win.

To best understand the *range* of possible outcomes, we can perform the
same simulation many times.

``` r
n <- 100000
sims <- rep(NA, n)
for (i in seq(1, n)) {
  state_outcomes <- map_lgl(ec$dem, sim_race)
  dem_total <- sum(ec$votes[state_outcomes])
  sims[i] <- dem_total
}
```

From the summary below, you can see a picture of a very close race with
the Democrats holding a slight lead. Of our 100,000 simulations, the
Democrats won 68.7% with the modal outcome being a victory of 279
electoral college votes.

``` r
summary(sims)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   127.0   263.0   285.0   285.3   307.0   414.0
mean(sims > 269)
#> [1] 0.6869
```

![](sim_college_files/figure-gfm/sim_hist-1.png)<!-- -->