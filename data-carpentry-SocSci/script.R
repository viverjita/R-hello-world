# download.file("https://ndownloader.figshare.com/files/11492171",
 #             "data/SAFI_clean.csv", mode = "wb") # this is the dataset I'm working with.
# r_length <- 10 #assigning variables
# r_width <- 5
# r_area <- r_length*r_width
# b <- sqrt(r_area)
# c <- round(b)
# d <- round(b, digits=2) # using a function and its arguments
# ----------------
# 2021-08-09 following the tutorial in https://datacarpentry.org/r-socialsci/01-intro-to-r/index.html
hh_members <- c(3,7,10,6)
respondent_wall_type <- c("muddaub", "burntbricks", "sunbricks")
length(hh_members)
length(respondent_wall_type)
class(hh_members)
class(respondent_wall_type)
str(hh_members)
str(respondent_wall_type)
possessions <- c("bicycle", "radio", "television")
possessions <- c(possessions, "mobile_phone") #add to the end of the vector
possessions <- c("car", possessions) #add to the beginning of the vector
possessions
typeof(possessions)
possessions_vehicle <- possessions[c(1,2)]
possessions_gadget <- possessions[c(3,4,5)]
hh_members > 5
hh_members [hh_members>5]
hh_members [hh_members> 5& hh_members<7]
possessions_gadget[possessions_gadget=="radio"]
possessions %in% c("car", "bicycle")
rooms <- c(2,1,1,NA,7)
mean(rooms) # because there's missing data, the answer is NA
mean(rooms, na.rm = TRUE) # ignore the missing values
max(rooms) # because there's missing data, the answer is NA
max(rooms, na.rm = TRUE)
rooms[!is.na(rooms)] # extract those elements which are not missing values
sum(is.na(rooms)) # count the no. of missing values
na.omit(rooms)
rooms[complete.cases(rooms)]
typeof(rooms)
rooms <- c(1, 2, 1, 1, NA, 3, 1, 3, 2, 1, 1, 8, 3, 1, NA, 1)
rooms <- rooms[complete.cases(rooms)]
median(rooms)
length(rooms [rooms>2])
# ---------------------------
# reading the dataset into R
library(tidyverse)
library(here) # for relative file paths

interviews <-  read_csv(here("data", "SAFI_clean.csv"), na="NULL") # dataype: tibble
view(interviews)
class(interviews)

#inspecting data frames
dim(interviews)
nrow(interviews)
ncol(interviews)
head(interviews)
tail(interviews)
names(interviews)
str(interviews)
summary(interviews) # provides summary statistics of each of the columns
glimpse(interviews)

#indexing and subsetting data frames
interviews [1,1]
interviews [1,6]
interviews [[1]] ## first column of the tibble, as a vector
interviews [1] ## first column of the tibble, as a tibble
interviews [1:5, 3] ## first 5 rows in the 3rd column
interviews [3, ]
head_interviews <- head(interviews)
interviews [6:1] ## change order of the first six columns
interviews [1:6]
interviews ["village"]
interviews$village ## this command autocompletes column names!

# Exercise
interviews_100 <- interviews[100,]
interviews_last <- interviews [nrow(interviews),] ## pulls out the last row of the dataset
tail(interviews)
n_row <- nrow(interviews)
interviews_middle <- interviews [median(1:n_row),]
interviews_head <- interviews [-(7:n_row),] ## negative sign for counting in reverse
# -------------
## factors
respondent_floor_type <- factor(c("earth", "cement"))
levels(respondent_floor_type)
nlevels(respondent_floor_type)
test_levels <- factor(c("low", "medium", "high"))
test_levels <- factor(test_levels, levels = c("low", "medium", "high")) # specifying the order of the categories
test_levels_ordered <- factor(test_levels, ordered = TRUE) # making sure that the levels are ordered for analysis in R
levels(respondent_floor_type)[3] <- "brick" # adding a level to the factor
as.character(respondent_floor_type)
memb_assoc <- interviews$memb_assoc
memb_assoc <- as.factor(memb_assoc)
levels(memb_assoc)
plot(memb_assoc) # but, this plot doesn't show the missing/ unrecorded data
memb_assoc[is.na(memb_assoc)] <- "undetermined"
memb_assoc <- as.factor(memb_assoc)
levels(memb_assoc)
plot(memb_assoc)
memb_assoc
levels(memb_assoc) # WHY ISN'T THIS SHOWING THE LEVELS? 
