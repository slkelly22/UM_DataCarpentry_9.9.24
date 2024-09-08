# Data Carpentry
# Episodes 4, 5, 6

# Session 4: Data Wrangling with dplyr
library(tidyverse)
library(here)

interviews <- read_csv(here("data", "SAFI_clean.csv"), na = "NULL")
interviews
view(interviews)

# select
select(interviews, village, no_membrs, months_lack_food)
interviews[c("village", "no_membrs", "months_lack_food")]
select(interviews, village:respondent_wall_type)

# filter
filter(interviews, village == "Chirodzo")
# and
filter(interviews, village == "Chirodzo", rooms > 1, no_meals > 2)
# same but with ampersand instead of comma
filter(interviews, village == "Chirodzo" & rooms > 1 & no_meals > 2)
# or
filter(interviews, village == "Chirodzo" | village == "Ruaca")

# Pipes to combine dplyr functions
# intermediate objects, nested functions, or pipes
# intermediate objects
interviews2 <- filter(interviews, village == "Chirodzo")
interviews_ch <- select(interviews2, village:respondent_wall_type)
# nested 
interviews_ch <- select(filter(interviews, village == "Chirodzo"), village:respondent_wall_type)
# piping
interviews_ch <- interviews %>% 
  filter(village == "Chirodzo") %>% 
  select(village:respondent_wall_type)
interviews_ch
# EXERCISE # dim: 33, 3
interviews %>% 
  filter(memb_assoc == "yes") %>% 
  select(affect_conflicts, liv_count, no_meals)

# mutate
interviews %>% #save over object or else can't see it or print Inf
  mutate(people_per_room = no_membrs / rooms) %>% 
  print(Inf)# sk; can add round if I want
view(interviews)
# does assoc membership affect that ratio? First, remove missing values
interviews %>% # sk added
  count(memb_assoc)
# use filter and then mutate; print to see change (or change object name)
interviews  %>% 
  filter(!is.na(memb_assoc)) %>% 
  mutate(people_per_room = no_membrs / rooms) %>% 
  print(Inf)
# EXERCISE (note to people: don't overwrite the interviews df)
exercise <- interviews %>% 
  mutate(total_meals = no_membrs * no_meals) %>% 
  filter(total_meals > 20) %>% 
  select(village, total_meals)

# Select, Apply, Combine with group_by and summarize
interviews %>% 
  group_by(village) %>% 
  summarize(mean_no_membrs = mean(no_membrs))
# can group_by multiple columns
interviews %>% 
  group_by(village, memb_assoc) %>% 
  summarize(mean_no_membrs = mean(no_membrs))
# with ungroup at end
interviews %>% 
  group_by(village, memb_assoc) %>% 
  summarize(mean_no_membrs = mean(no_membrs)) %>% 
  ungroup()
# excluding those who did not specify whether they were members
interviews %>% 
  filter(!is.na(memb_assoc)) %>% 
  group_by(village, memb_assoc) %>% 
  summarize(mean_no_membrs = mean(no_membrs))
# adding another value in the summarize function
interviews %>% 
  filter(!is.na(memb_assoc)) %>% 
  group_by(village, memb_assoc) %>% 
  summarize(mean_no_membrs = mean(no_membrs), 
            min_membrs = min(no_membrs))
# Let's arrange to have smallest household first
interviews %>% 
  filter(!is.na(memb_assoc)) %>% 
  group_by(village, memb_assoc) %>% 
  summarize(mean_no_membrs = mean(no_membrs), 
            min_membrs = min(no_membrs)) %>% 
  arrange(min_membrs)
# or descending
interviews %>% 
  filter(!is.na(memb_assoc)) %>% 
  group_by(village, memb_assoc) %>% 
  summarize(mean_no_membrs = mean(no_membrs), 
            min_membrs = min(no_membrs)) %>% 
  arrange(desc(min_membrs))
# Counting
interviews %>% 
  count(village)
# sort within count
interviews %>% 
  count(village, sort = TRUE)

# EXERCISE - how many households have an average of 2 meals per day? 3 meals?
interviews %>% 
  count(no_meals)
# EXERCISE 
interviews %>% 
  group_by(village) %>% 
  summarize(village_mean = mean(no_membrs), 
            village_max = max(no_membrs), 
            village_min = min(no_membrs))

# Session 5: Data Wrangling with tidyr
View(interviews)

interviews %>% 
  select(key_ID) %>% 
  distinct() %>% 
  count() # 131

interviews %>% 
  select(instanceID) %>% 
  distinct() %>% 
  count() # 131

# Pivoting Wider
interviews_items_owned <- interviews
# separate_longer_delim
interviews_items_owned <- interviews_items_owned %>% 
  separate_longer_delim(items_owned, delim = ";") #631 rows now
# some didn't own anything so we want to replace that NA value
interviews_items_owned <- interviews_items_owned %>% 
  replace_na(list(items_owned = "no_listed_items"))
# creating a logical vector that tells you if someone had that particular item
interviews_items_owned <- interviews_items_owned %>% 
  mutate(items_owned_logical = TRUE)
# then we switch back from long to wide
interviews_items_owned <- interviews_items_owned %>% 
  pivot_wider(names_from = items_owned, 
              values_from = items_owned_logical) # see all those NAs
              #values_fill = list(items_owned_logical = FALSE)
# now we fix those NAs will values_fill argument
interviews_items_owned <- interviews_items_owned %>% 
  pivot_wider(names_from = items_owned, 
              values_from = items_owned_logical, 
              values_fill = list(items_owned_logical = FALSE))

dim(interviews_items_owned) #back to 131 rows

interviews_items_owned %>% 
  filter(bicycle) %>% 
  group_by(village) %>% 
  count(bicycle)

interviews_items_owned %>% 
  group_by(village) %>% 
  count(bicycle)

interviews_items_owned %>% 
  select(-no_listed_items) %>% 
  mutate(number_items = rowSums(select(., bicycle:car))) %>% 
  group_by(village) %>% 
  summarize(mean_items = mean(number_items))
