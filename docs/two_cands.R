library(tidyverse)
library(snakecase)
library(lubridate)

markets <-
  bind_rows(
    read_csv("data/markets/2019-06-19_market.csv"),
    read_csv("data/markets/2019-07-10_markets.csv")
  ) %>%
  mutate(name = to_snake_case(name)) %>%
  distinct() %>%
  arrange(date) %>%
  select(date, name, close)

trends <-
  read_csv("data/trends/2019-06-24_trends.csv") %>%
  mutate(
    name = to_snake_case(word(keyword, 2, -1)),
    hits = hits / 100,
    date = as_date(date)
  ) %>%
  select(date, name, hits)

polling <-
  bind_rows(
    read_csv("data/polling/2019-06-19_polls.csv") %>% slice(-1),
    read_csv("data/polling/2019-07-10_polls.csv") %>% slice(-1)
  ) %>%
  distinct() %>%
  mutate(points = as.double(na_if(points, "--"))/100) %>%
  select(start, name, points) %>%
  rename(date = start) %>%
  arrange(date)

two_cands_market <- markets %>%
  ggplot(aes(date, close)) +
  geom_line(
    data = markets %>% filter(name %out% c("warren", "o_rourke")),
    mapping = aes(group = name),
    size = 0.5,
    alpha = 0.25
  ) +
  geom_line(
    data = markets %>% filter(name %in% c("warren", "o_rourke")),
    mapping = aes(color = name),
    size = 2
  ) +
  scale_color_manual(values = c("grey10", "springgreen4")) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "A Tale of Two Candidates",
    subtitle = "Prediction Market $ Represents % Probability of Winning Nomination",
    x = "Date",
    y = "Closing Price",
    color = "Candidate",
    caption = "Source: PredictIt #3633"
  )

ggsave(
  filename = "plots/two_cands_market.png",
  plot = two_cands_market,
  dpi = "retina",
  height = 5,
  width = 9
)

trends %>%
  ggplot(aes(date, hits)) +
  geom_line(
    data = trends %>% filter(name %out% c("warren", "o_rourke")),
    mapping = aes(group = name),
    size = 0.25,
    alpha = 0.25
  ) +
  geom_line(
    data = trends %>% filter(name %in% c("warren", "o_rourke")),
    mapping = aes(color = name),
    size = 2
  ) +
  scale_color_manual(values = c("grey10", "springgreen4")) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "A Tale of Two Candidates",
    subtitle = "Relative Proportion of Google Search Traffic",
    x = "Date",
    y = "Search Index",
    color = "Candidate"
  )

two_cands_poll <- polling %>%
  ggplot(aes(date, points)) +
  geom_smooth(
    data = polling %>% filter(name %out% c("warren", "o_rourke")),
    mapping = aes(group = name),
    color = "grey",
    size = 0.5,
    alpha = 0.25,
    se = FALSE
  ) +
  geom_smooth(
    data = polling %>% filter(name %in% c("warren", "o_rourke")),
    mapping = aes(color = name, fill = name),
    size = 2,
    se = TRUE
  ) +
  geom_point(
    data = polling %>% filter(name %in% c("warren", "o_rourke")),
    mapping = aes(color = name),
    size = 2,
    alpha = 0.75
  ) +
  scale_color_manual(values = c("grey10", "springgreen4")) +
  scale_fill_manual(values = c("grey10", "springgreen4"), guide = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "A Tale of Two Candidates",
    subtitle = "Share of Support Across Opinon Polls",
    x = "Start Date",
    y = "Polling Support",
    color = "Candidate",
    caption = "Source: RealClearPolitics"
  )

ggsave(
  filename = "plots/two_cands_polls.png",
  plot = two_cands_poll,
  dpi = "retina",
  height = 5,
  width = 9
)

endorse <-
  read_csv("data/endorsements/2019-07-10_endorse.csv")

endorse <- endorse %>%
  arrange(date) %>%
  group_by(endorsee) %>%
  mutate(cum_points = cumsum(points)) %>%
  filter(!is.na(endorsee)) %>%
  filter(year(date) == 2019)

endorse %>%
  ggplot(aes(date, cum_points)) +
  geom_line(
    data = endorse %>% filter(endorsee %out% c("warren", "o_rourke")),
    mapping = aes(group = endorsee),
    color = "grey",
    size = 0.5,
    alpha = 0.25,
  ) +
  geom_line(
    data = endorse %>% filter(endorsee %in% c("warren", "o_rourke")),
    mapping = aes(color = endorsee),
    size = 2,
  ) +
  scale_color_manual(values = c("grey10", "springgreen4")) +
  labs(
    title = "A Tale of Two Candidates",
    subtitle = "Share of Support Among Polling Sample",
    x = "Start Date",
    y = "Polling Support",
    color = "Candidate",
    caption = "Source: RealClearPolitics"
  )
