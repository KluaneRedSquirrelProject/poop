library(krsp)
library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
list.files("R", full.names = TRUE) %>% 
  walk(source)

con <- krsp_connect(group = "krsp-aws")

# trapping tables
trapping <- "SELECT * FROM trapping WHERE YEAR(date) >= 2006;" %>% 
  krsp_sql(con, .) %>% 
  filter(!is.na(squirrel_id)) %>% 
  select(trapping_id = id, squirrel_id, grid = gr, obs,
         tag_left = tagLft, tag_right = tagRt,
         wgt,
         locx, locy,
         date, comments) %>% 
  as_tibble()

# treat years independently
# 2006: from hormone file, match to trapping using comments
pfile_2006 <- read_xlsx("data/krsp_fecal-samples_2006.xlsx", 
                        sheet = "BJD Updated Formatting Data") %>% 
  mutate(year = year(`Trapping Date`),
         date = as.character(`Trapping Date`),
         id = row_number()) %>% 
  filter(year == 2006) %>% 
  select(id, grid = Grid, tag_left = TagLft, tag_right = TagRt,
         date, comments = `Trap comments`,
         poop_id = `Poop Vial ID`, poop_time = `Poop Time`) %>% 
  mutate(poop_time = as.integer(poop_time))
# match to trapping based on grid, date, tags, and comments
matches <- trapping %>% 
  filter(!is.na(comments), comments != "", year(date) == 2006) %>% 
  left_join(pfile_2006, ., by = c("grid", "tag_left", "tag_right", "date", 
                                  "comments"))
stopifnot(matches %>% 
            group_by(id) %>% 
            filter(n() > 1) %>% 
            nrow() == 0
)
# split off records that didn't match to trapping
problem_2006 <- matches %>% 
  filter(is.na(trapping_id)) %>% 
  select(grid, date, tag_left, tag_right, poop_id, poop_time, comments)
# prepare the poop table records
# note some times are missing
poop_2006 <- matches %>% 
  filter(!is.na(trapping_id)) %>% 
  mutate(year = 2006, vial_number = 1) %>% 
  select(squirrel_id, trapping_id, year, vial_number, poop_id, poop_time, 
         comments)

# 2007: 
pfile_2007 <- read_xlsx("data/krsp_fecal-samples_2007.xlsx") %>% 
  mutate(year = year(`Trapping Date`),
         date = as.character(`Trapping Date`),
         # read PT stored in 12 hour format
         poop_12 = strptime(Time, "%I:%M %p") %>% format("%H%M"),
         # read PT stored in 24 hour format
         poop_24 = strptime(Time, "%H:%M:S") %>% format("%H%M"),
         poop_time = coalesce(poop_12, poop_24) %>% as.integer(),
         id = row_number()) %>% 
  filter(year == 2007, !is.na(`Lab Poop ID`)) %>% 
  select(id, grid = Grid, tag_left = `Tag L`, tag_right = `Tag R`,
         date, 
         poop_id = `Lab Poop ID`, poop_time) %>% 
  distinct()
# match to trapping - both tags
matches <- trapping %>% 
  filter(year(date) == 2007) %>% 
  left_join(pfile_2007, ., by = c("grid", "tag_left", "tag_right", "date"))
# split off records that didn't match to trapping
no_match <- matches %>% 
  filter(is.na(trapping_id)) %>% 
  select(one_of(names(pfile_2007)))
matches <- filter(matches, !is.na(trapping_id))
# one tag
# left
no_match <- trapping %>% 
  filter(year(date) == 2007) %>% 
  select(-tag_right) %>% 
  left_join(no_match, ., by = c("grid", "tag_left", "date"))
matches <- no_match %>% 
  filter(!is.na(trapping_id)) %>% 
  bind_rows(matches, .)
no_match <- no_match %>% 
  filter(is.na(trapping_id)) %>% 
  select(one_of(names(pfile_2007)))
# right
no_match <- trapping %>% 
  filter(year(date) == 2007) %>% 
  select(-tag_left) %>% 
  left_join(no_match, ., by = c("grid", "tag_right", "date"))
matches <- no_match %>% 
  filter(!is.na(trapping_id)) %>% 
  bind_rows(matches, .)
# no matches at all
problem_2007 <- no_match %>% 
  filter(is.na(trapping_id)) %>% 
  select(one_of(names(pfile_2007)))
# multiple matches
matches %>% 
  group_by(id) %>% 
  filter(n() > 1)
# all dupes have "handled" or "hndld" in comments, remove
matches <- matches %>% 
  group_by(id) %>% 
  filter(n() ==1 | !str_detect(comments, "handled|hndld")) %>% 
  ungroup()
stopifnot(matches %>% 
            group_by(id) %>% 
            filter(n() > 1) %>% 
            nrow() == 0
)
# prepare the poop table records
# note some times are missing
poop_2007 <- matches %>% 
  filter(!is.na(trapping_id)) %>% 
  mutate(year = 2007, vial_number = 1) %>% 
  select(squirrel_id, trapping_id, year, vial_number, poop_id, poop_time, 
         comments)

# 2009 & 2011: 
pfile_2009 <- read_xlsx("data/krsp_fecal-samples_2008-12.xlsx") %>% 
  filter(Year %in% c(2009,2011), !is.na(Poop)) %>% 
  mutate(date = as.character(Date),
         id = row_number(),
         LocX = loc_to_numeric(LocX) %>% round(1) %>% format(nsmall = 1) %>% trimws()) %>% 
  select(id, grid = Grid, obs = Obs,
         tag_left = Taglft, tag_right = Tagrt,
         locx = LocX, locy = LocY, weight = SqWt,
         date, 
         poop_id = Poop, poop_time = PoopTime.24h,
         comments = Comments) %>% 
  distinct() %>% 
  mutate(poop_time = as.integer(poop_time))
# match to trapping, both tags
matches <- trapping %>% 
  filter(year(date) %in% c(2009,2011)) %>% 
  rename(locx_t = locx, locy_t = locy, comments_t = comments) %>% 
  left_join(pfile_2009, ., by = c("grid", "date", "obs",
                                  "tag_left", "tag_right"))
# split off records that didn't match to trapping
no_match <- matches %>% 
  filter(is.na(trapping_id)) %>% 
  select(one_of(names(pfile_2009)))
matches <- filter(matches, !is.na(trapping_id))
# one tag
# left
no_match <- trapping %>% 
  filter(year(date) %in% c(2009,2011)) %>% 
  select(-tag_right, -comments) %>% 
  rename(locx_t = locx, locy_t = locy) %>% 
  left_join(no_match, ., by = c("grid", "date", "obs", "tag_left"))
matches <- no_match %>% 
  filter(!is.na(trapping_id)) %>% 
  bind_rows(matches, .)
no_match <- no_match %>% 
  filter(is.na(trapping_id)) %>% 
  select(one_of(names(pfile_2009)))
# right
no_match <- trapping %>% 
  filter(year(date) %in% c(2009,2011)) %>% 
  select(-tag_left, -comments) %>% 
  rename(locx_t = locx, locy_t = locy) %>% 
  left_join(no_match, ., by = c("grid", "date", "obs", "tag_right"))
matches <- no_match %>% 
  filter(!is.na(trapping_id)) %>% 
  bind_rows(matches, .)
# no matches at all
problem_2009 <- no_match %>% 
  filter(is.na(trapping_id)) %>% 
  select(one_of(names(pfile_2009)))
# multiple matches
multiples_2009 <- matches %>% 
  group_by(id) %>% 
  filter(n() > 1) %>% 
  ungroup()
matches <- filter(matches, !id %in% multiples_2009$id)
# remove ambiguous cases using location
matches <- multiples_2009 %>% 
  mutate(loc_match = coalesce(locx == locx_t & locy == locy_t, FALSE)) %>% 
  group_by(id) %>% 
  filter(sum(loc_match) == 1) %>% 
  ungroup() %>% 
  filter(loc_match) %>% 
  select(-loc_match) %>% 
  bind_rows(matches, .)
multiples_2009 <- filter(multiples_2009, !id %in% matches$id)
# remove ambiguous cases using comments
matches <- multiples_2009 %>% 
  mutate(com_match = coalesce(comments == comments_t, FALSE)) %>% 
  group_by(id) %>% 
  filter(sum(com_match) == 1) %>% 
  ungroup() %>% 
  filter(com_match) %>% 
  select(-com_match) %>% 
  bind_rows(matches, .)
multiples_2009 <- filter(multiples_2009, !id %in% matches$id)
# remove ambiguous cases using weight
matches <- multiples_2009 %>% 
  mutate(wgt_match = coalesce(weight == wgt, FALSE)) %>% 
  group_by(id) %>% 
  filter(sum(wgt_match) == 1) %>% 
  ungroup() %>% 
  filter(wgt_match) %>% 
  select(-wgt_match) %>% 
  bind_rows(matches, .)
multiples_2009 <- filter(multiples_2009, !id %in% matches$id)
# remove released
matches <- multiples_2009 %>% 
  filter(!str_detect(str_to_lower(comments_t), "releas|escape|handle")) %>% 
  group_by(id) %>% 
  filter(n() == 1) %>% 
  ungroup() %>% 
  bind_rows(matches, .)
multiples_2009 <- filter(multiples_2009, !id %in% matches$id)
# chose one from multiples
matches <- multiples_2009 %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  bind_rows(matches, .)
# prepare the poop table records
poop_2009 <- matches %>% 
  mutate(year = year(date), vial_number = 1) %>% 
  select(squirrel_id, trapping_id, year, vial_number, poop_id, poop_time, 
         comments)

# 2010
pfile_2010 <- list.files("data/", "2010", full.names = TRUE) %>% 
  map_df(read_xls) %>% 
  filter(!is.na(Poop)) %>% 
  mutate(date = as.character(Date),
         id = row_number(),
         LocX = loc_to_numeric(LocX) %>% round(1) %>% format(nsmall = 1) %>% trimws()) %>% 
  select(id, grid = Grid, obs = Obs,
         tag_left = Taglft, tag_right = Tagrt,
         locx = LocX, locy = LocY, weight = SqWt,
         date, 
         poop_id = Poop, poop_time = Ptime,
         comments = Comments) %>% 
  distinct() %>% 
  mutate(poop_time = str_replace(poop_time, ":", ""),
         poop_time =  as.double(poop_time))
# match to trapping, both tags
matches <- trapping %>% 
  filter(year(date) == 2010) %>% 
  rename(locx_t = locx, locy_t = locy, comments_t = comments) %>% 
  left_join(pfile_2010, ., by = c("grid", "date", "obs",
                                  "tag_left", "tag_right"))
# split off records that didn't match to trapping
no_match <- matches %>% 
  filter(is.na(trapping_id)) %>% 
  select(one_of(names(pfile_2010)))
matches <- filter(matches, !is.na(trapping_id))
# one tag
# left
no_match <- trapping %>% 
  filter(year(date) == 2010) %>% 
  select(-tag_right, -comments) %>% 
  rename(locx_t = locx, locy_t = locy) %>% 
  left_join(no_match, ., by = c("grid", "date", "obs", "tag_left"))
matches <- no_match %>% 
  filter(!is.na(trapping_id)) %>% 
  bind_rows(matches, .)
no_match <- no_match %>% 
  filter(is.na(trapping_id)) %>% 
  select(one_of(names(pfile_2010)))
# right
no_match <- trapping %>% 
  filter(year(date) == 2010) %>% 
  select(-tag_left, -comments) %>% 
  rename(locx_t = locx, locy_t = locy) %>% 
  left_join(no_match, ., by = c("grid", "date", "obs", "tag_right"))
matches <- no_match %>% 
  filter(!is.na(trapping_id)) %>% 
  bind_rows(matches, .)
# no matches at all
problem_2010 <- no_match %>% 
  filter(is.na(trapping_id)) %>% 
  select(one_of(names(pfile_2010)))
# multiple matches
multiples_2010 <- matches %>% 
  group_by(id) %>% 
  filter(n() > 1) %>% 
  ungroup()
matches <- filter(matches, !id %in% multiples$id)
# remove ambiguous cases using location
matches <- multiples_2010 %>% 
  mutate(loc_match = coalesce(locx == locx_t & locy == locy_t, FALSE)) %>% 
  group_by(id) %>% 
  filter(sum(loc_match) == 1) %>% 
  ungroup() %>% 
  filter(loc_match) %>% 
  select(-loc_match) %>% 
  bind_rows(matches, .)
multiples_2010 <- filter(multiples_2010, !id %in% matches$id)
# remove ambiguous cases using comments
matches <- multiples_2010 %>% 
  mutate(com_match = coalesce(comments == comments_t, FALSE)) %>% 
  group_by(id) %>% 
  filter(sum(com_match) == 1) %>% 
  ungroup() %>% 
  filter(com_match) %>% 
  select(-com_match) %>% 
  bind_rows(matches, .)
multiples_2010 <- filter(multiples_2010, !id %in% matches$id)
# remove ambiguous cases using weight
matches <- multiples_2010 %>% 
  mutate(wgt_match = coalesce(weight == wgt, FALSE)) %>% 
  group_by(id) %>% 
  filter(sum(wgt_match) == 1) %>% 
  ungroup() %>% 
  filter(wgt_match) %>% 
  select(-wgt_match) %>% 
  bind_rows(matches, .)
multiples_2010 <- filter(multiples_2010, !id %in% matches$id)
# remove released
matches <- multiples_2010 %>% 
  filter(!str_detect(str_to_lower(comments_t), "releas|escape|handle")) %>% 
  group_by(id) %>% 
  filter(n() == 1) %>% 
  ungroup() %>% 
  bind_rows(matches, .)
multiples_2010 <- filter(multiples_2010, !id %in% matches$id)
# chose one from multiples
matches <- multiples_2010 %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  bind_rows(matches, .)
# prepare the poop table records
poop_2010 <- matches %>% 
  mutate(year = year(date), 
         vial_number = 1) %>% 
  select(squirrel_id, trapping_id, year, vial_number, poop_id, poop_time, 
         comments)

# combine problems
problems <- bind_rows(problem_2006, problem_2007, problem_2009, problem_2010)
write_csv(problems, "output/poop_no-matches.csv")
multiples <- bind_rows(multiples_2009, multiples_2010) %>% 
  select(names(problems) %>% c("trapping_id", "squirrel_id") %>% one_of())
write_csv(multiples, "output/poop_duplicates.csv")

# combine poop
poop <- bind_rows(poop_2006, poop_2007, poop_2009, poop_2010) %>% 
  filter(!poop_id %>% tolower() %>% str_detect("no"))
# split out record with two ids in poop_id field
poop <- poop %>% 
  filter(str_detect(poop_id, ",")) %>% 
  mutate(poop_id = str_split(poop_id, ","),
         poop_id = map(poop_id, trimws),
         poop_id = map(poop_id, ~ data_frame(poop_id = .) %>% 
                         mutate(vial_number = row_number()))) %>% 
  select(-vial_number) %>% 
  unnest() %>% 
  select(one_of(poop %>% names)) %>% 
  bind_rows(poop %>% filter(!str_detect(poop_id, ",")), .) %>% 
  mutate(squirrel_id = as.integer(squirrel_id),
         trapping_id = as.integer(trapping_id),
         vial_number = as.integer(vial_number),
         poop_time = as.integer(poop_time))
write_csv(poop, "output/poop_external.csv")

# combine db and external
read_csv("output/poop_db.csv") %>% 
  bind_rows(poop) %>% 
  mutate(squirrel_id = as.integer(squirrel_id),
         trapping_id = as.integer(trapping_id),
         vial_number = as.integer(vial_number),
         poop_time = as.integer(poop_time)) %>% 
  write_csv("output/poop.csv")
