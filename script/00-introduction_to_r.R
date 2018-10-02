
# getting started ---------------------------------------------------------

# play around in R

5 + 6

bens_result <- 10 * 6.7

"bens_result"

# load packages

library(tidyverse)

# read in some data

data(mtcars)


# look at data

glimpse(mtcars)

summary(mtcars)


# simple summary statistics

mean_horse_power <- 
  mtcars %>% 
  summarise(horse_power = mean(hp))

mtcars %>%
  mutate(cyl_disp = cyl + disp) %>% 
  summarise(mean_cyl_disp = mean(cyl_disp))


# looking at the pipe

mtcars %>% 
  filter(cyl == 6) %>% 
  summarise(mean = mean(hp))


summarise(filter(mtcars, cyl == 6), mean = mean(hp))

cyl_6 <- filter(mtcars, cyl == 6)
result <- summarise(cyl_6, mean = mean(hp))
result
