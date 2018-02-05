library(krsp)
library(tidyverse)
library(stringr)
library(lubridate)
list.files("R", full.names = TRUE) %>% 
  walk(source)

con <- krsp_connect(group = "krsp-aws")

# trapping tables
trapping <- "SELECT * FROM trapping WHERE YEAR(date) >= 2008;" %>% 
  krsp_sql(con, .) %>% 
  select(trapping_id = id, squirrel_id, date, comments) %>% 
  as_tibble()
trapping_dba <- "SELECT * FROM dbatrapping WHERE YEAR(date) >= 2008;" %>% 
  krsp_sql(con, .) %>% 
  select(trapping_id = id, squirrel_id, date, comments = TagHist, poop, 
         ptime) %>% 
  as_tibble()

# treat years independently
# 2006-7, 2009-11: external files
# 2008: trappings comments column, no pv
pt_rgx <- "(?<=p[0-9]{4}[-;,:]?\\s{0,2})[0-2]?[0-9](h|:)?[0-9]{2}\\s?(am|pm)?"
poop_2008 <- trapping %>% 
  filter(year(date) == 2008, comments != "") %>% 
  mutate(pv = str_extract(str_to_lower(comments), "p[0-9]{4}"),
         pv_second = str_extract(str_to_lower(comments), "p[0-9]{4}")) %>% 
  filter(!is.na(pv)) %>% 
  mutate(poop_time = str_extract(str_to_lower(comments), pt_rgx)) %>% 
  gather(vial_number, poop_id, pv, pv_second) %>% 
  filter(!(is.na(poop_id) & vial_number == "pv_second")) %>% 
  mutate(vial_number = if_else(vial_number == "pv", 1L, 2L),
         poop_id = str_to_upper(poop_id),
         poop_time = clean_pt(poop_time),
         year = year(date)) %>% 
  select(squirrel_id, trapping_id, year, vial_number, poop_id, poop_time,
         comments)

# 2012: dbatrapping designated columns
poop_2012 <- trapping_dba %>% 
  filter(year(date) == 2012) %>% 
  filter(poop != "") %>% 
  mutate(vial_number = 1, year = year(date), ptime = clean_pt(ptime)) %>% 
  select(squirrel_id, trapping_id, year, vial_number, 
         poop_id = poop,  poop_time = ptime, comments)

# 2014-17: trappings comments column
poop_2014_7 <- trapping %>% 
  filter(year(date) %in% 2014:2017,
         str_detect(comments, "(p|P)(v|V)")) %>% 
  mutate(pv = extract_pv(comments),
         pv_second = extract_pv(comments, first = FALSE),
         poop_time = extract_pt(comments)) %>% 
  gather(vial_number, poop_id, pv, pv_second) %>% 
  filter(!(is.na(poop_id) & vial_number == "pv_second")) %>% 
  mutate(vial_number = if_else(vial_number == "pv", 1L, 2L),
         year = year(date)) %>% 
  select(squirrel_id, trapping_id, year, vial_number, poop_id, poop_time, 
         comments)
poop <- bind_rows(poop_2008, poop_2012, poop_2014_7)