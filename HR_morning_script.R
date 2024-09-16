dir.create("data")
dir.create("data_output")
dir.create("fig_output")

download.file(  "https://raw.githubusercontent.com/datacarpentry/r-socialsci/main/episodes/data/SAFI_clean.csv",  "data/SAFI_clean.csv", mode = "wb"  )

install.packages("tidyverse")
installed.packages("tidyverse") #how to check if the package is installed

install.packages("here")          

3+8
12/7


area_hectares <- 1.0 #assigning value 

2 * area_hectares

#this is a comment, anything written to the right of the # symbol will not run when the command is executed 

area_hectares <- 2.5

area_hectares * 2.47


b <- sqrt(24)

round(3.14159)

round(3.14159, digits = 2)

round(3.1459, 2)

round(digits = 2, x = 3.14159)


# Vectors and data types 

hh_members <- c(3, 7, 10, 6)

respondent_wall_type <- c("mudduab", "burntbricks", "sunbricks")

respondent_wall_type

length(hh_members)
length(respondent_wall_type)


typeof(hh_members)

str(respondent_wall_type)

possessions <- c("bicycle", "radio", "televison")
possessions <- c(possessions, "mobile_phone") #add to the end of the vector

possessions <- c("car", possessions) #add to the start of the vector


#subsetting vectors by using brackets 

respondent_wall_type <- c("muddaub", "burntbricks", "sunbricks")
respondent_wall_type[2]

respondent_wall_type[c(3,2)]

more_respondent_wall_type <- respondent_wall_type[c(1, 2, 3, 2, 1, 3)]
more_respondent_wall_type

#conditional subsetting 
#TRUE will select the elements of the same index (the spot it appears in the vector)
##FALSE will not select the elements of the same index 

# hh_members 3 7 10 6 
# respondent_wall_type mudduab burntbricks sunbricks 
respondent_wall_type[c(TRUE, FALSE, FALSE)]

hh_members[c(TRUE, FALSE, TRUE, TRUE)]

new_object <- hh_members[c(TRUE, FALSE, TRUE, TRUE)]


# & AND (both conditions are true)
# | #OR (at least one of the conditions is true)

hh_members[hh_members < 4 | hh_members > 7]

hh_members[hh_members >= 4 & hh_members <=7]

# == is used for "equals" 

possessions[possessions == "car" | possessions == "bicycle"]


#The function %in% lets us test if any of the elements of a search vector(left side)
# is in the target vector (on the right side)

possessions %in% c("car","bicycle")

#Missing Data

rooms <- c(2, 1, 1, NA, 7)

max(rooms)
max(rooms, na.rm = TRUE)

mean(rooms)
mean(rooms, na.rm = TRUE)


#is.na() 
#na.omit()
#complete.cases() 
# ! means not

sum(is.na(rooms))

sum(!is.na(rooms))

rooms <- c(1, 2, 1, 1, NA, 3, 1, 3, 2, 1, 1, 8, 3, 1, NA, 1)
rooms_no_na <-  rooms[!is.na(rooms)]

rooms_omit <- na.omit(rooms)

median(rooms, na.rm = TRUE)
rooms_above_2 <- rooms_no_na[rooms_no_na > 2]
lengths(rooms_above_2)

#starting with data 

library(tidyverse)
library(here)

interviews <- read_csv(
  here("data", "SAFI_clean.csv"),
  na = "NULL"
)

interviews

head(interviews) #shows first six rows
tail(interviews) #shows the last 6 rows
view(interviews) #creates spreadsheet to view 


class(interviews)
str(interviews)

# [rows, column]
# first element in the first column of the tibble 
interviews[1,1]


# first element in the 6th column of the tibble 
interviews[1, 6]

#first three elements in the 7th column 
interviews[1:3,7]

#the 3rd row of the tibble 
interviews[3, ]


#equivalent to head_interviews <- head(interviews)
head_interviews <- interviews[1:6, ]

# the whole tibble except for the first column 
interviews[, -1:-2]

#Factors 

respondent_floor_type <- factor(c("earth", "cement", "cement", "earth"))

levels(respondent_floor_type)
nlevels(respondent_floor_type)

respondent_floor_type # current order 

respondent_floor_type <- factor(respondent_floor_type,
                                levels = c("earth", "cement"))
respondent_floor_type #after reordering


respondent_floor_type <- fct_recode(respondent_floor_type, brick = "cement")
levels(respondent_floor_type)


respondent_floor_type_ordered <- factor(respondent_floor_type,
                                        ordered = TRUE)
respondent_floor_type_ordered #after setting as ordered factor

#converting factors 

as.character(respondent_floor_type) #converting into character 
as.numeric #converting into numeric 

#Renaming factors 

#created a vector from the data frame column "memb_assoc"
memb_assoc <- interviews$memb_assoc

#convert it into a factor 
memb_assoc <- as.factor(memb_assoc)

#see what it looks like 
memb_assoc

plot(memb_assoc)

#recreate the vector 
memb_assoc <- interviews$memb_assoc 

##replace the missing data with "undetermined" 
memb_assoc[is.na(memb_assoc)] <- "undetermined"

#convert it into a factor 
memb_assoc <- as.factor(memb_assoc)
#see what it looks like
memb_assoc

#plot! 
plot(memb_assoc)

#renaming plot labels 
memb_assoc <- fct_recode(memb_assoc,
                         No = "no",
                         Undetermined = "undetermined",
                         Yes = "yes")

#Reorder levels (NOTE: we have to use the new names)

memb_assoc <- factor(memb_assoc, 
                     levels = c("No", "Yes", "Undetermined"))

plot(memb_assoc)

#Formatting Dates 

str(interviews)
library(lubridate)

# lubridate function ymd()

dates <- interviews$interview_date
str(dates)


interviews$day <- day(dates)
interviews$month <- month(dates)
interviews$year <- year(dates)

interviews

char_dates <- c("7/31/2012","8/9/2014", "4/30/2016")
str(char_dates)

#convert into a date vector 
new_dates <- as_date(char_dates, format = "%m/%d/%Y")

str(new_dates)
str(char_dates)



