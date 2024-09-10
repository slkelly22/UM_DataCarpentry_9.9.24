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

## ggplot2
### Note: I'm compiling the code the day after teaching Data Carpentry to replicate what I covered yesterday b/c 
### I accidentally closed my syntax file without saving the last episode but I still generally remember what I 
### covered and what I skipped

interviews_plotting %>% 
  ggplot()

interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items))

# you'll see a plot once you add the geom
interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items)) + geom_point()

interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items)) + geom_point(alpha = 0.3)

interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items)) + geom_jitter()

# default jitter value is 40% so we can change with width/height arguments
interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items)) + geom_jitter(alpha = 0.3, width = 0.2, height = 0.2)

# now to add color
interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items)) + geom_jitter(color = "blue", alpha = 0.3, width = 0.2, height = 0.2)

# colors in R
colors()

# to add color based on a variable in your df
interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items)) + geom_jitter(aes(color = village), alpha = 0.3, width = 0.2, height = 0.2)

# can also add color in the main aes 
interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items, color = village)) + geom_jitter(alpha = 0.3, width = 0.2, height = 0.2)

# geom_count
interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items, color = village)) + geom_count()

# boxplot
interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type, y = rooms)) + geom_boxplot()

# let's make the boxplot transparent and add back the points with geom_jitter
interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type, y = rooms)) + geom_boxplot(alpha = 0) + geom_jitter(alpha = 0.3, color = "tomato", width = 0.2, height = 0.2)

# what if we changed the boxplot to a violin plot? 
interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type, y = rooms)) + geom_boxplot(alpha = 0) + geom_violin(alpha = 0.3, color = "tomato", width = 0.2, height = 0.2)

# barplots
interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type)) + geom_bar()

# we can fill the bars with counts from each village; this is a stacked bar chart
interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type)) + geom_bar(aes(fill = village))

# to do a side-by-side barchart you add position
interviews_plotting %>% 
  ggplot(aes(x = respondent_wall_type)) + geom_bar(aes(fill = village), position = "dodge")

# these are counts, but what if we wanted to compare proportions? we need to create a new dataframe
percent_wall_type <- interviews_plotting %>% 
  filter(respondent_wall_type != "cement") %>% 
  count(village, respondent_wall_type) %>% 
  group_by(village) %>% 
  mutate(percent = (n/sum(n))* 100)

# we'll now use the percent_wall_type dataframe to plot
percent_wall_type %>% 
  ggplot(aes(x = village, y = percent, fill = respondent_wall_type)) + geom_bar(stat = "identity", position = "dodge")

# adding labels and titles
percent_wall_type %>% 
  ggplot(aes(x = village, y = percent, fill = respondent_wall_type)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Proportion of wall type by village", fill = "Type of wall in home", x = "Village", y = "Percent")

# faceting
percent_wall_type %>% 
  ggplot(aes(x = respondent_wall_type, y = percent)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Proportion of wall type by village", x = "Wall Type", y = "Percent") + facet_wrap(~village)

# to change the theme and remove the grid
percent_wall_type %>% 
  ggplot(aes(x = respondent_wall_type, y = percent)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Proportion of wall type by village", x = "Wall Type", y = "Percent") + facet_wrap(~village) + theme_bw() + theme(panel.grid = element_blank())

# to create your own custom theme that you can apply to plots
SK_theme <- theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 45, hjust = 0.5, vjust = 0.5), 
                  axis.text.y = element_text(color = "grey20", size = 12), 
                  text = element_text(size = 16), 
                  plot.title = element_text(hjust = 0.5))

# try out your theme by adding it to our last plot
percent_wall_type %>% 
  ggplot(aes(x = respondent_wall_type, y = percent)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Proportion of wall type by village", x = "Wall Type", y = "Percent") + facet_wrap(~village) + theme_bw() + theme(panel.grid = element_blank()) + SK_theme
