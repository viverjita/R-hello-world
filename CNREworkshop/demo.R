heights <- c(2, 4, 4, NA, 6)
mean(heights) # doesnot give a result because of the missing data NA
mean(heights, na.rm= T)
max(heights) # doesnot give a result because of the missing data NA
heights [!is.na(heights)]
mean(heights[!is.na(heights)])
# find the missing data NA
which(is.na(heights)) # which indices are TRUE?
# ---
# loading the survey data 
download.file(url = "https://ndownloader.figshare.com/files/2292169",
              destfile = "data_raw/portal_data_joined.csv")
library(tidyverse)
survey <- read_csv("data_raw/combined.csv")
head(survey)
view(survey) # open the survey in a window
str(survey) # structure of the tibble. summary of different columns and what their types are
dim(survey) # gets the dimensions of the tibble
nrow(survey)
ncol(survey)
colnames(survey) # gives the names of the columns i.e., variables
rownames (survey)
summary(survey)
# data_frame[row_index, column_index]
survey[1, 10]
survey[10, ] # reference row only
survey[, 10] # reference column or survey[10] or survey [[10]]
survey[1:3, 5:6]
# get the first column as a vector
survey[, -1] # shows the whole dataframe except the first column
survey[, -1:3] # donot show the first three columns
survey["species_id"] # gives a tibble
survey[["species_id"]] # gives a vector
survey$species_id # gives a vector
survey$sex <- factor(survey$sex)
summary(survey$sex)
sex <- factor(c("male", "female", "female", "male", "male"))
nlevels(sex) #no. of levels
sex <- factor(sex, levels=c("male", "female")) # reordering factors
as.character(sex)# convert factor
year_fct <- factor(c(1990, 1993, 1977, 1998, 1990))
as.numeric(year_fct) # gives the factor "numbers"
as.numeric(as.character(year_fct)) # convert to character first, and then to numeric
# preferred way of conversion
as.numeric(levels(year_fct))[year_fct]
plot(survey$sex) # but this has the NA data mixed in
sex <- survey$sex
levels(sex)
sex <- addNA(sex)
levels(sex)
levels (sex)[3] <- "undertermined" #renaming a factor. NA -> undetermined
plot(sex)
# working with dates
library(lubridate)
my_date <- ymd("2015-01-01")
my_date <- dmy("01-12-2020")
my_date <- ymd("2021/11/15")
str(my_date)
ymd(paste(survey$year, survey$month, survey$day, sep = "-"))
# make a new column in the dataset
survey$date=ymd(paste(survey$year, survey$month, survey$day, sep = "-")) #can a variable be inserted at a specific location?
# deadling with the dates that failed to parse
missing_date <- survey[is.na(survey$date), c("year", "month", "day")]
missing_date # haha! there's no 31st day in the month

# --- using dplyr
# SELECT() selects columns
select(survey, plot_id, species_id, weight) # useful for saving the dataframe in a different order
select(survey, -record_id, -species_id) # select by excluding certain variables
select(survey, month:species_id)


# FILTER() filters rows 
filter(survey, year==1995)


# using select and filter together -- NOT the most efficient way (clutters workspace with temporary variables)
surveys2 <- filter(survey, weight<5)
surveys_sml <- select(surveys2, species_id, sex, weight) 
# another option: nesting functions
surveys_sml <- select(filter(survey, weight < 5), species_id, sex, weight)

# PIPE: piping the results of the first function into the second. Shortcut: Ctrl + Shift + M
surveys_sml <- survey %>% filter (weight < 5) %>% select(species_id, sex, weight) # filter and then select

# mutate 
survey %>% mutate(weight_kg = weight/1000) %>% head()

survey_wt <- survey %>% filter(!is.na(weight)) %>% mutate(weight_kg = weight/1000) %>% head()
survey_hf <- survey %>% filter(!is.na(hindfoot_length)) %>%  mutate (hindfoot_cm=hindfoot_length/10)

# group_by()
survey %>% group_by(sex) %>% summarize(mean_weight = mean(weight, na.rm = TRUE))
survey_group <- survey %>% 
  filter(!is.na(weight), !is.na(sex)) %>%  
  group_by(sex, species_id) %>% 
  summarize(mean_weight=mean(weight, na.rm = TRUE)) %>% arrange(desc(mean_weight))

# count()
survey %>% count(sex)
survey %>% group_by(sex) %>% summarise(count=n()) # option 2 # count assigns to the column n
survey %>% count(sex, species) %>% arrange(species, desc(n))

surveys_gw <- survey %>% 
  filter(!is.na(weight)) %>% 
  group_by(plot_id, genus) %>% 
  summarize(mean_weight = mean(weight))  
# make the result WIDER
survey_wide <- surveys_gw %>% 
  pivot_wider(names_from = genus, values_from = mean_weight, values_fill = 0)
# make the result LONGER
survey_long <- survey_wide %>% 
  pivot_longer(col = -plot_id, values_to = "mean_weight") #including changing the name of the column

#exporting the data
write_csv(survey_wide, file="data/surveys_wide.csv")
# --- using ggplot
library(tidyverse)
surveys_complete <- survey %>%
  filter(!is.na(weight),           # remove missing weight
         !is.na(hindfoot_length),  # remove missing hindfoot_length
         !is.na(sex))                # remove missing sex


## Extract the most common species_id
species_counts <- surveys_complete %>%
  count(species_id) %>% 
  filter(n >= 50)

## Only keep the most common species
surveys_complete <- surveys_complete %>%
  filter(species_id %in% species_counts$species_id)


write_csv(surveys_complete, file = "data/surveys_complete.csv")
theme_set(theme_classic())
surveys_plot <- ggplot(data=surveys_complete, mapping = aes(x=weight, y=hindfoot_length)) # can write without the labels: data =, mapping = 
surveys_plot+
  geom_point()
# change the transparency
surveys_plot+
  geom_point(alpha=0.1)
# what goes inside the aesthetic: only the data. Outside: tweaks
ggplot(surveys_complete,
        aes(weight, hindfoot_length))+
  geom_point(alpha=0.1, aes(color=species_id))
ggplot(surveys_complete,
       aes(species_id, weight))+ # visualizing categories
  geom_jitter(alpha=0.3, color="tomato")+
  geom_boxplot(alpha=0.1)

# visualizing time series data
yearly_counts <- surveys_complete %>% count(year, genus)
ggplot(data=yearly_counts, aes(x=year, y=n, color=genus))+
  geom_line()

#using pipes
yearly_counts_graph <- yearly_counts %>% ggplot(aes(x=year, y=n, color=genus))+
  geom_line()
yearly_counts_graph

#make a plot for each genus. Using FACETS
yearly_counts %>% ggplot(aes(x=year, y=n))+
  geom_line()+
  facet_wrap(facets=vars(genus))

year_sex_counts <- surveys_complete %>% 
  count(year, genus, sex)
ggplot(data = year_sex_counts, mapping = aes(x=year, y=n, color=sex))+
  geom_line()+
  facet_wrap(facets=vars(genus))

ggplot(data = year_sex_counts, mapping = aes(x=year, y=n, color=sex))+
  geom_line()+
  facet_wrap(facets=vars(genus))+
  theme(axis.text.x = element_text(color="grey20", size =12), strip.text = elemt_text(face="italic"),
        text = element_text(size - qr)

ggsave("my_nice_plot.png")


