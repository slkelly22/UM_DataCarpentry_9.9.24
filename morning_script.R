# UM Data Carpentry (9.9.24)
# Morning Session / HR

#dir.create("data")
#dir.create("data_output")
#dir.create("fig_output")

download.file(  "https://raw.githubusercontent.com/datacarpentry/r-socialsci/main/episodes/data/SAFI_clean.csv",  "data/SAFI_clean.csv", mode = "wb"  )

#install.packages("tidyverse")
#install.packages("here")

area_hectares <- 1.0
2 * area_hectares

#this is a comment

area_hectares <- 2.5

b <- sqrt(24)
round(3.14159)
round(3.14159, digits = 2)
round(3.14159, 2)
round(digits = 2, x = 3.14159)

hh_members <- c(3,7,10,6)
respondent_wall_type <- c("muddaub", "burntbricks", "sunbricks")
respondent_wall_type
length(respondent_wall_type)
typeof(hh_members)
typeof(respondent_wall_type)
str(hh_members)
str(respondent_wall_type)

# Can you add on instead of writing it from scratch? yes
possessions <- c("bicycle", "radio", "television")
possessions <- c(possessions, "mobile_phone") # adds to the end
possessions
possessions <- c("car", possessions) # adds to the front
possessions

respondent_wall_type <- c("muddaub", "burntbricks", "sunbricks")
respondent_wall_type[2]
respondent_wall_type[c(3,2)]
more_respondent_wall_type <- respondent_wall_type[c(1,2,3,2,1,3)]

# Conditional subsetting
# TRUE will select the elements of the same index
# FALSE will not select the elements of the same index

hh_members[c(TRUE, FALSE, TRUE, TRUE)]

# & AND
# | OR

hh_members[hh_members < 4 | hh_members > 7]
hh_members[hh_members <= 4 | hh_members >= 7]

# == is used for equals

possessions
possessions[possessions == "car"| possessions == "bicycle"]

possessions %in% c("car", "bicycle") # returns a logical vector
possessions %in% c("car", "bicycle")

# Missing Data
rooms <- c(2, 1, 1, NA, 7)
max(rooms) # returns NA
max(rooms, na.rm = TRUE)
mean(rooms)
mean(rooms, na.rm = TRUE)
# is.na()
# na.omit()
# complete.cases()

sum(is.na(rooms))
sum(!is.na(rooms))

# Fifteen minute break

library(tidyverse)
library(here)

interviews <- read_csv(here("data", "SAFI_clean.csv"), na = "NULL")
interviews
head(interviews)
tail(interviews)
view(interviews)
class(interviews)

interviews[1, 1] # first row, first column
interviews[1, 6]
interviews[1:3, 7]
interviews[3, ]
interviews[, -1:-2]

# Factors
respondent_floor_type <- factor(c("earth", "cement", "cement", "earth"))
nlevels(respondent_floor_type)
levels(respondent_floor_type)

respondent_floor_type # current order: cement earth
respondent_floor_type <- factor(respondent_floor_type, levels = c("earth", "cement"))
respondent_floor_type

respondent_floor_type <- fct_recode(respondent_floor_type, brick = "cement") # ?
levels(respondent_floor_type)

respondent_floor_type_ordered <- factor(respondent_floor_type, ordered = TRUE)
respondent_floor_type_ordered

# converting factors
as.character(respondent_floor_type)
as.numeric(respondent_floor_type)

# renaming factors
memb_assoc <- interviews$memb_assoc
memb_assoc <- as.factor(memb_assoc)
memb_assoc
plot(memb_assoc) # some folks were stuck here b/c the import data didn't have NULL in all caps

memb_assoc <- interviews$memb_assoc
memb_assoc[is.na(memb_assoc)] <- "undetermined"
memb_assoc
memb_assoc <- as.factor(memb_assoc)
plot(memb_assoc)

# Formatting Dates
str(interviews)
library(lubridate)
# lubridate function ymd()
dates <- interviews$interview_date
str(date)
dates
interviews$day <- day(dates)
interviews$month <- month(dates)
interviews$year <- year(dates)

char_dates <- c("7/31/2012", "8/9/2014", "4/30/2016")
str(char_dates)
dates <- as_date(char_dates, format = "%m/%d/%Y")
str(dates)
