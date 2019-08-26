library(tidyverse)
mit_url <- "https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/42MVDX/MFU99O"
potus <-
  vroom(
    file = mit_url,
    delim = "\t",
    escape_double = FALSE
  )%>%
  filter(party %in% c("democrat", "republican")) %>%
  mutate(prop = candidatevotes/totalvotes,) %>%
  select(year, state = state_po, party, prop) %>%
  group_by(state, party) %>%
  summarise(sd = sd(prop))
