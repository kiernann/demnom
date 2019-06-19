pacman::p_load(
  tidyverse,
  lubridate,
  janitor,
  rvest,
  fs
)

polls <-
  read_html("https://www.realclearpolitics.com/epolls/2020/president/us/2020_democratic_presidential_nomination-6730.html") %>%
  html_node("table.data:nth-child(2)") %>%
  html_table(fill = TRUE) %>%
  as_tibble() %>%
  clean_names() %>%
  gather(
    -poll, -date,
    key = "name",
    value = "points"
  ) %>%
  separate(
    col = date,
    into = c("start", "end"),
    sep = "\\s-\\s"
  ) %>%
  mutate(
    start = mdy(str_c(start, "/19")),
    end = mdy(str_c(end, "/19"))
  ) %>%
  filter(name != "spread")

write_csv(
  x = polls,
  path = here("data", paste(today(), "polls.csv", sep = "_"))
)
