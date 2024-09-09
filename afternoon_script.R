# UM Data Carpentry (9.9.24)
# Afternoon Session / SK

library(tidyverse)
library(here)
interviews <- read_csv(here("data", "SAFI_clean.csv"), na = "NULL")

# Core Tidyverse Functions
# select - grabs variables / columns
# filter - grabs rows based on criteria
# mutate - create a new variable based on current variables
# group_by and summarize - allows you to group by certain categories and then summarize by those groups
# count - count categorical data
# pipe %>%

# dplyr
select(interviews, village, no_membrs, months_lack_food) # using Tidy

interviews[c("village", "no_membrs", "months_lack_food")] # using Base R

select(interviews, village:respondent_wall_type)

# filter
filter(interviews, village == "Chirodzo")

# AND: , & 
# OR: |

filter(interviews, village == "Chirodzo", rooms > 1, no_meals > 2)
filter(interviews, village == "Chirodzo" | village == "Ruaca")

# Combine dplyr functions
# Create intermediate objects
interviews2 <- filter(interviews, village == "Chirodzo")
interviews_ch <- select(interviews2, village:respondent_wall_type)
rm(interviews2)

# nested function
interviews_ch <- select(filter(interviews, village == "Chirodzo"), village:respondent_wall_type)

# Tidyverse %>%
# Native R Pipe: |>

chirodzo_interviews <- interviews %>% 
  filter(village == "Chirodzo") %>% 
  select(village:respondent_wall_type)

# Exercise: Use %>%, subset the interviews data to include interviews where respondents were members of an irrigation association (memb_assoc) and retain only the columns affect_conflicts, liv_count, and no_meals

interviews %>% 
  filter(memb_assoc == "yes") %>% 
  select(affect_conflicts, liv_count, no_meals)

# Mutate - create a new variable based on variables you already have

interviews <- interviews %>% 
  mutate(people_per_room = round(no_membrs / rooms))

interviews %>% 
  filter(!is.na(memb_assoc)) %>% 
  mutate(people_per_room = round(no_membrs / rooms)) %>% 
  print(Inf)

# group_by and summarize
interviews %>% 
  group_by(village) %>% 
  summarize(mean_no_membrs = mean(no_membrs))

interviews %>% 
  group_by(village, memb_assoc) %>% 
  summarize(mean_no_membrs = mean(no_membrs)) %>% 
  ungroup()

interviews %>% 
  filter(!is.na(memb_assoc)) %>% 
  group_by(village, memb_assoc) %>% 
  summarize(mean_no_membrs = mean(no_membrs))

# arrange
interviews %>% 
  filter(!is.na(memb_assoc)) %>% 
  group_by(village, memb_assoc) %>% 
  summarize(mean_no_membrs = mean(no_membrs),
            min_membrs = min(no_membrs)) %>% 
  arrange(desc(min_membrs))

# count
interviews %>% 
  count(village)
?count
args(count)

args(round)

interviews %>% 
  count(village, sort = TRUE)

#EXERCISE: How many households had an average of two meals per day? 
interviews %>% 
  count(no_meals)

# tidyr
interviews %>% 
  select(key_ID) %>% 
  distinct() %>% 
  count()

interviews %>% 
  select(instanceID) %>% 
  distinct() %>% 
  count()

interviews_items_owned <- interviews %>% 
  separate_longer_delim(items_owned, delim = ";") %>% 
  replace_na(list(items_owned = "no_listed_items")) %>% 
  mutate(items_owned_logical = TRUE) %>% 
  pivot_wider(names_from = items_owned, 
              values_from = items_owned_logical, 
              values_fill = list(items_owned_logical = FALSE))

interviews_items_owned %>% 
  filter(bicycle) %>% 
  group_by(village) %>% 
  count(bicycle)

table(interviews_items_owned$car)

interviews_items_owned %>% 
  select(-no_listed_items) %>% 
  mutate(number_items = rowSums(select(., bicycle:car))) %>% 
  group_by(village) %>% 
  summarize(mean_items = mean(number_items))

# Pivot Longer
interviews_long <- interviews_items_owned %>% 
  pivot_longer(cols = bicycle:car, 
               names_to = "items_owned", 
               values_to = "items_owned_logical")

# Exercise Together
items_owned_village <- interviews_long %>% 
  filter(items_owned_logical) %>% 
  group_by(village) %>% 
  count(items_owned)

# make new dataset that we can plot
interviews_plotting <- interviews %>% 
  separate_longer_delim(items_owned, delim = ";") %>% 
  replace_na(list(items_owned = "no_listed_items")) %>% 
  mutate(items_owned_logical = TRUE) %>% 
  pivot_wider(names_from = items_owned, 
              values_from = items_owned_logical, 
              values_fill = list(items_owned_logical = FALSE)) %>% 
  #new code
  separate_longer_delim(months_lack_food, delim = ";") %>% 
  mutate(months_lack_food_logical = TRUE) %>% 
  pivot_wider(names_from = months_lack_food, 
              values_from = months_lack_food_logical, 
              values_fill = list(months_lack_food_logical = FALSE)) %>% 
  # summary stats
  mutate(number_months_lack_food = rowSums(select(., Jan:May))) %>% 
  mutate(number_items = rowSums(select(., bicycle:car)))

# Export our new dataset
write_csv(interviews_plotting, file = "data_output/interviews_plotting.csv")












