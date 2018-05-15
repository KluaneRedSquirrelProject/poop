library(tidyverse)

poop <- read_csv("output/poop.csv")
poop$row_id <- seq.int(nrow(poop))

# first identify problems
problems <- group_by(poop, poop_id) %>% 
  filter(n() > 1, n_distinct(year) > 1 | 
           #n_distinct(squirrel_id) > 1 | 
           n_distinct(comments) > 1) %>% 
  ungroup() %>% 
  arrange(poop_id) %>% 
  mutate(problem_duplicate = TRUE)

# drop full duplicates
poop <- filter(poop, !row_id %in% problems$row_id) %>% 
  group_by(poop_id) %>% 
  arrange(trapping_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(problem_duplicate = FALSE) %>% 
  select(-row_id)

# recombine
bind_rows(poop, problems %>% select(-row_id)) %>% 
  write_csv("output/poop_deduped.csv", na = "")