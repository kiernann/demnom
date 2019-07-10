pacman::p_load(
  tidyverse,
  lubridate,
  janitor,
  rvest,
  glue,
  here,
  fs
)

polls <-
  read_html("http://bit.ly/2LdZIpm") %>%
  html_node("table.data:nth-child(2)") %>%
  html_table(fill = TRUE) %>%
  as_tibble(.name_repair = make_clean_names) %>%
  select(-spread) %>%
  slice(-1)

polls <- polls %>%
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
    points = as.double(na_if(points, "--")),
    start = mdy(glue("{start}/19")),
    end = mdy(glue("{end}/19")),
    length = end - start
  ) %>%
  arrange(poll, start) %>%
  select(
    name,
    poll,
    points,
    start,
    end,
    length
  )

write_csv(
  x = polls,
  path = here("data", "polling", paste(today(), "polls.csv", sep = "_"))
)
