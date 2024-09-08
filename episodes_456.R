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

# Episode 5: Data Wrangling with tidyr
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

table(interviews_items_owned$bicycle)

interviews_items_owned %>% 
  select(-no_listed_items) %>% 
  mutate(number_items = rowSums(select(., bicycle:car))) %>% 
  group_by(village) %>% 
  summarize(mean_items = mean(number_items))

# pivoting longer
interviews_long <- interviews_items_owned %>% 
  pivot_longer(cols = bicycle:car, 
               names_to = "items_owned", 
               values_to = "items_owned_logical")

dim(interviews_long) #2358

# Exercise: do together
interviews_long %>% 
  filter(items_owned_logical) %>% 
  group_by(village) %>% 
  count(items_owned)
items_owned

# cleaning out data and getting ready to plot
interviews_plotting <- interviews %>% 
  separate_longer_delim(items_owned, delim = ";") %>% 
  replace_na(list(items_owned = "no_listed_items")) %>% 
  mutate(items_owned_logical = TRUE) %>% 
  pivot_wider(names_from = items_owned, 
              values_from = items_owned_logical, 
              values_fill = list(items_owned_logical = FALSE)) %>% 
  #new content
  separate_longer_delim(months_lack_food, delim = ";") %>% 
  mutate(months_lack_food_logical = TRUE) %>% 
  pivot_wider(names_from = months_lack_food, 
              values_from = months_lack_food_logical, 
              values_fill = list(months_lack_food_logical = FALSE)) %>% 
  # summary stats
  mutate(number_months_lack_food = rowSums(select(., Jan:May))) %>% 
  mutate(number_items = rowSums(select(., bicycle:car)))

write_csv(interviews_plotting, file = "data_output/interviews_plotting.csv")

# Episode 6: Data Visualization with ggplot2

interviews_plotting %>% 
  ggplot()

interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items))

interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items)) + geom_point()

interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items)) + geom_point(alpha = 0.3)

interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items)) + geom_jitter(alpha = 0.2, width = 0.2, height = 0.2, color = "blue")

interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items)) + geom_jitter(aes(color = village), alpha = 0.2, width = 0.2, height = 0.2)

interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items, color = village)) + geom_jitter(alpha = 0.2, width = 0.2, height = 0.2)

interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items, color = village)) + geom_count()

interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items, size = no_membrs, color = village)) + geom_point()

# Exercise
interviews_plotting %>% 
  ggplot(aes(x = village, y = rooms, color = respondent_wall_type)) + geom_point() + geom_jitter()

# Boxplots
interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type, y = rooms)) + geom_boxplot()

interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type, y = rooms)) + geom_boxplot() + geom_point()

interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type, y = rooms)) + geom_boxplot(alpha = 0) + geom_jitter(color = "tomato", width = 0.2, height = 0.2)

# Exercises

# Barplots
interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type)) + geom_bar()

interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type)) + geom_bar(aes(fill = village))

interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type)) + geom_bar(aes(fill = village), position = "dodge")

percent_wall_type <- interviews_plotting %>% 
  filter(respondent_wall_type !="cement") %>% 
  count(village, respondent_wall_type) %>% 
  group_by(village) %>% 
  mutate(percent = (n/sum(n)) * 100) %>% 
  ungroup()

percent_wall_type %>% 
  ggplot(aes(x = village, y = percent, fill = respondent_wall_type)) + geom_bar(stat = "identity", position = "dodge")

# Adding labels and titles
percent_wall_type %>% 
  ggplot(aes(x = village, y = percent, fill = respondent_wall_type)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Proportion of wall type by village", fill = "Type of Home in Wall", x = "Village", y = "Percent")

# Faceting - changes x and removes fill 
percent_wall_type %>% 
  ggplot(aes(x = respondent_wall_type, y = percent)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Proportion of wall type by village", x = "Wall Type", y = "Percent") + facet_wrap(~village)

# Adding theme and removing back grid
percent_wall_type %>% 
  ggplot(aes(x = respondent_wall_type, y = percent)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Proportion of wall type by village", x = "Wall Type", y = "Percent") + facet_wrap(~village) + theme_bw() + theme(panel.grid = element_blank())

# Items - tricky code here creating the new df
percent_items <- interviews_plotting %>% 
  group_by(village) %>% 
  summarize(across(bicycle:no_listed_items, ~ sum(.x) / n() * 100)) %>% 
  pivot_longer(bicycle:no_listed_items, names_to = "items", values_to = "percent")

percent_items %>% 
  ggplot(aes(x = village, y = percent)) + geom_bar(stat = "identity", position = "dodge") + facet_wrap(~ items) + theme_bw() + theme(panel.grid = element_blank())

# Exercise - look at two different themes

# Customization
# In case we skip the items plot, let me see if we can customize the facet_wrap wall type plot

# increased font size
percent_wall_type %>% 
  ggplot(aes(x = respondent_wall_type, y = percent)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Proportion of wall type by village", x = "Wall Type", y = "Percent") + facet_wrap(~village) + theme_bw() + theme(panel.grid = element_blank(), text = element_text(size = 14))
# angle the x axis
percent_wall_type %>% 
  ggplot(aes(x = respondent_wall_type, y = percent)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Proportion of wall type by village", x = "Wall Type", y = "Percent") + facet_wrap(~village) + theme_bw() + theme(panel.grid = element_blank(), text = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))

# saving a theme
grey_theme <- theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 45, hjust = 0.5, vjust = 0.5), axis.text.y = element_text(color = "grey20", size = 12), text = element_text(size = 14), plot.title = element_text(hjust = 0.5))

# applying that theme to my plot
plot <- percent_wall_type %>% 
  ggplot(aes(x = respondent_wall_type, y = percent)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Proportion of wall type by village", x = "Wall Type", y = "Percent") + facet_wrap(~village) + grey_theme

# saving a plot with ggsave
ggsave("fig_output/percent_wall_type.png", plot, width = 15, height = 10)
