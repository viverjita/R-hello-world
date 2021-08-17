# Load the gapminder package
library(gapminder)

# Load the dplyr package
library(dplyr)

# Look at the gapminder dataset
gapminder

# filter() by row
gapminder %>% 
  filter(year==2007)
gapminder %>% 
  filter(country=="United States")
gapminder %>% 
  filter(year==2007, country=="India")

# arrange() -- sorting by ascending/ descending
gapminder %>%
  filter(year==2002) %>% 
  arrange(desc(gdpPercap))
gapminder %>% 
  filter(year==1952) %>% 
  arrange(lifeExp)

# mutate() -- change variable based on other variables, or add a variable based on existing variables
gapminder %>% 
  mutate(gdp= gdpPercap * pop) %>% 
  filter(year == 2007) %>% 
  arrange(desc(gdp))
gapminder %>% 
  mutate(lifeExp=lifeExp*12)
gapminder %>% 
  mutate(lifeExpMonths=12*lifeExp) 
gapminder %>% 
  filter(year == 2007) %>% 
  mutate(lifeExpMonths=12*lifeExp) %>% 
  arrange(desc(lifeExpMonths))
