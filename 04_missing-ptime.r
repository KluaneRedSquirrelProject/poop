library(tidyverse)

poop <- read_csv("output/poop_deduped.csv", na = "")

ptime_rgx <- "\\(([:0-9]{3,5}).*\\)"
guesses <- poop %>% 
  filter(is.na(poop_time)) %>% 
  mutate(time_guess = str_match(comments, ptime_rgx)[, 2],
         time_guess = str_replace_all(time_guess, ":", ""),
         poop_time = as.integer(time_guess))
filter(poop, !is.na(poop_time)) %>% 
  bind_rows(guesses) %>% 
  write_csv("output/poop_time-filled.csv", na = "")
