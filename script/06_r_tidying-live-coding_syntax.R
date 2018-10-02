# data tidying live code ------------------------------------------------------

# first steps with a dataset --------------------------------------------------

# actual first step - load the data
library(gapminder)
library(tidyverse)
library(foreign)

# exporting data for stata

write_dta(gap_data, "gapminder.dta")

# the below works because the gapminder library loads a dataset called gapminder
gap_data <- gapminder

# first steps - taking a look at the data
# a quick overview
glimpse(gap_data)

#the data itself
View(gap_data)

# a statistical summary
summary(gap_data)

# mutate -----------------------------------------------------------------------
gap_data %>% 
  mutate(gdp = gdpPercap * pop)

# filtering --------------------------------------------------------------------
# first lets filter the data for only 1972
gap_data %>% 
  filter(year == 1972)

# now for years after 1972
gap_data %>% 
  filter(year > 1972)

# and now for 1997 but only for Europe
gap_data %>% 
  filter(year == 1977 & continent == "Europe")


# if and else ------------------------------------------------------------------
# nope

# working on groups of data ----------------------------------------------------
gap_data %>% 
  group_by(continent) %>% 
  filter(lifeExp == max(lifeExp))
  
gap_data %>% 
  group_by(continent, year) %>% 
  filter(lifeExp == max(lifeExp))  
  
# select -----------------------------------------------------------------------
gap_data %>% 
  select(country, year, lifeExp)

gap_data %>% 
  select(-continent)

# spread and gather ------------------------------------------------------------
# first we're going to use the three-variable dataset from before

gap_data_wide <- gap_data %>% 
  select(country, year, lifeExp) %>% 
  spread(year, lifeExp)

gap_data_wide %>% 
  gather("year", "lifeExp", 2:13)

# it's now ordered by country not year, but that's fine

# aggregation ------------------------------------------------------------------
gap_data %>% 
  summarise(lifeExp = mean(lifeExp))

gap_data %>% 
  group_by(continent) %>% 
  summarise(lifeExp = mean(lifeExp))

gap_data %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarise(total_pop = sum(pop))


# working with strings ---------------------------------------------------------
gap_data <- gap_data %>% 
  unite("country_continent", c(country, continent))

gap_data %>% 
  mutate(country_continent = as.factor(country_continent))

gap_data <- gap_data %>% 
  separate(country_continent, into = c("country", "continent"), sep = "_")

# reordering variables ---------------------------------------------------------
gap_data %>% 
  arrange(pop)

gap_data %>% 
  arrange(desc(pop))

# window operators -------------------------------------------------------------
gap_data %>% 
  group_by(country) %>%
  mutate(gdplag = lag(gdpPercap))

gap_data %>% 
  group_by(country) %>%
  mutate(gdp_change = gdpPercap - lag(gdpPercap))
