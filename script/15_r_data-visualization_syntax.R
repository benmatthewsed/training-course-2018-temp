# Working with Admin Data: Data Visualization -------------------
# B Matthews, L Forrest and Z Feng
# November 2017, ADRC-S, University of Edinburgh

# Notes ------------------------------------------------------------------------
# The data sets provided for training purposes in this course are synthetic. 
# No records in the synthetic data correspond to real individuals. The synthetic
# data have been created from summaries and  relationships between the variables
# in the original data. The aim is to make the results you will get from analysing
# synthetic data similar to what might be found from the original. The staff who 
# created the synthetic data have tried to make them appear similar to the original
# data, even to the extent that the original data are not clean.

# But you must realise that none of the results from them are real and we cannot
# guarantee that every analysis of these data will give the same answers as the 
# original. For these reasons you will be asked to sign a form to agree to the 
# following restrictions:  
 
# 1) Neither the synthetic data nor results based on them must be copied and 
# removed from the  computers used for the course.
# 2) No results you obtain on this course should be reported or mentioned, 
# even casually, to people outside the course less they think that your conclusions
# are true.

# Please note that the methods you will use in this exercise are not 
# necessarily the approach that you will take once you are more experienced.

# clearing workspace

# press Ctrl-shift-f10

# load the packages ------------------------------------------------------------
library(tidyverse)
library(Cairo)

# load the data ----------------------------------------------------------------
load("M:/Working with Admin Data Course/synthetic-data/FALSE_DATA_2017_003-census-education.RData")

# in this session we want to build on the composite grmmar of graphics logic 
# we've just been talking about. So first take some time to plan out
# what variables you're most interested in and what relationships between
# variables you want to explore. Think of each level of the grammar - 
# which geometric objects are most relevant? How can you highlight the comparisons
# you're most interested in?

# Whilst this session is aimed to get you to work on what you find most intersting,
# some example code is included below.

# and don't forget the cheat sheets!

# once you're happy with the plots you've made, skip to the end and save the results


# example plots ---------------------------------------------------

linked_df %>%   
  group_by(examS4_grp) %>% 
  ggplot(aes(x = examS4_grp)) +
  geom_bar()

# how does this plot break down by sex?
# geom_point might be a better option here
linked_df %>% 
  group_by(examS4_grp, sex0) %>% 
  count() %>% 
  filter(!is.na(sex0)) %>% 
  ggplot(aes(x = examS4_grp, y = n, colour = sex0)) +
  geom_point()

# and now look at tenure
linked_df %>% 
  group_by(examS4_grp, newten) %>% 
  count() %>% 
  ggplot(aes(x = examS4_grp, y = n, colour = newten)) +
  geom_point()

# seems like it would be helpful to look at these figures by proportion
linked_df %>% 
  group_by(examS4_grp, newten) %>% 
  count() %>% 
  group_by(examS4_grp) %>%
  mutate(proportion = n / sum(n)) %>% 
  ggplot(aes(x = examS4_grp, y = proportion, colour = newten)) +
  geom_point()

# perhaps the proportions would be more helpful
# we can calculate these with another call to mutate
linked_df %>% 
  group_by(examS4_grp, freemeal) %>% 
  count() %>% 
  group_by(examS4_grp) %>% 
  mutate(proportion = n / sum(n)) %>% 
  ggplot(aes(x = examS4_grp, y = proportion, colour = as.factor(freemeal))) +
  geom_point()

# getting to grips with ggplot2: facets and alpha ------------------------------



# produce a plot with facets
linked_df %>% 
  group_by(examS4_grp, freemeal, newten) %>% 
  count() %>% 
  ggplot(aes(x = examS4_grp, y = n, colour = as.factor(freemeal))) +
  geom_point() +
  facet_wrap(~ newten, scales = "free_y")

# an example of a continuous variable
linked_df %>% 
  ggplot(aes(x = sum_duration)) +
  geom_histogram() 

# geom_density is another nice example
linked_df %>% 
  ggplot(aes(x = sum_duration)) +
  geom_density() 


# how about looking at duration by gender
linked_df %>% 
  ggplot(aes(x = sum_duration, fill = gender)) +
  geom_density() 

# not very helpful as the two density plots get in the way of each other!
# we can either change the transparency with alpha or facet


linked_df %>% 
  ggplot(aes(x = sum_duration, fill = gender)) +
  geom_density(alpha = 0.4) 

linked_df %>% 
  ggplot(aes(x = sum_duration, fill = gender)) +
  geom_density() +
  facet_wrap( ~ gender)

# remember Cleveland's hierarchy - why is changing the transparency a better option
# here?

# an example of a scatterplot
# there's not many variables here that really make sense to scatterplot, but we
# can get an idea of how to construct these are made

linked_df %>% 
  ggplot(aes(x = slsdobmt, y = sum_duration)) +
  geom_point()


# seems likely that there's overplotting here - try changing the alpha again
linked_df %>% 
  ggplot(aes(x = slsdobmt, y = sum_duration)) +
  geom_point(alpha = 0.2)

# we can then add a regression line summarising this relationship
# let's use a linear regression by specifying method = "lm"
linked_df %>% 
  ggplot(aes(x = slsdobmt, y = sum_duration)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")

# as we expect, there's not relationship there - that's good!

# say we think this relationship between birthmonth and exclusion duration
# might vary by council area. We can add a facet to the above plot to check this
# out

linked_df %>% 
  ggplot(aes(x = slsdobmt, y = sum_duration)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ councilarea)

# to avoid "Aberdeen First!" let's reorder this plot by the number of children
# in each council area. We can call fct_reorder from within our call to ggplot2
# to reorder council area by synid, using the function NROW (which counts the
# number of rows - i.e. children - in each group). The .desc = TRUE options
# tells ggplot2 we want to start with the council area with the most children

linked_df %>% 
  ggplot(aes(x = slsdobmt, y = sum_duration)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ fct_reorder(councilarea, synid, fun = NROW, .desc = TRUE))
              
# this kind of small multiples approach can be really helpful when looking at data
# across administrative units, to try to spot potential changes in practice


# exporting your plots ---------------------------------------------------------

# we can export plots using ggsave(). By default this uses the last plot in the viewing
# pane, but we should be more explicit.
# First, assign the plot you want to save as an object with a sensible name

ca_duration_mnth_plot <- linked_df %>% 
  ggplot(aes(x = slsdobmt, y = sum_duration)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ fct_reorder(councilarea, synid, fun = NROW, .desc = TRUE))

# then pass this object to ggsave, as well as the filename.  load the library(Cairo)
# and specify type = "cairo" for ailised plots on Windows - we want this!
# make sure to save this in a sensible place based on our folder structure

ggsave(ca_duration_mnth_plot, file = "exclusion-duration-month-council.png", type = "cairo")

# open the file and see what it looks like. You can change the dimensions with
# the arguments width = X, height = Y, in the ggsave call

# checking package versions ----------------------------------------------------
sessionInfo()