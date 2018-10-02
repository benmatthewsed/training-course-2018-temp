# Working with Admin Data: Indexing and Joining -------------------------------------
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


# load the packages ------------------------------------------------------------
library(tidyverse)

# load data --------------------------------------------------------------------
# read in the dataset you made yesterday, or the .RData file called exclusions day two

load("M:/Working with Admin Data Course/synthetic-data/FALSE_DATA_2017_003-exclusions-day_2.RData")

# How many variables are there in the file? What types of these variables are? 
# It is also a good practice to tabulate categorical variables and check the 
# summary statistics for continuous variables.  


# remind outselves what variables are there ------------------------------------
glimpse(exclusions)

# Visually inspect the data using View(). Move down to person 90 (synid=90) you will find 
# that the first three records have identical start dates (and finish dates as well). 

# checking data for person 90
View(exclusions)

# so this confirms the structure of the dataset - by exclusion *reason* not
# exclusion event. We need to be aware of this when calculating summary statistics
# as we might double-count exclusions with multiple reasons!

# Finding errors ---------------------------------------------------------------

# Each excluion should have a unique start and finish date. But it might not!

# First write syntax for creating spell index and then use them to identify 
# potential errors in date variables. Create first index by person ID and start 
# date (name id_date_ind). This is a variable which is assigned the value 1 for 
# the first exclusion spell defined by person ID and startdate for an individual,
# value 2 for the second exclusion in the same individual, and so on. Then create
# another index by person ID, startdate and finishdate (name it  id_date_ind2).

exclusions <- exclusions %>% 
  group_by(synid, startdate) %>% 
  mutate(id_date_ind = row_number()) %>% 
  group_by(synid, finishdate) %>% 
  mutate(id_date_ind2 = row_number())

# Write syntax to list cases where the two index variables disagree. 
# The expectation is that these two variables agree for all exclusions. 
# this gives us a vector of TRUE/FALSE showing whether the startdate indicators
# match the finishdate indicators. We can use this to see where the two
# variables disagree by only selecting cases where this flag == FALSE

# Again we'll use mutate for this task. Our logical operator == will tell us
# whether each one of the id_date_ind is the same as the matching id_date_ind2.
# We can then filter only where our flag is FALSE.
# If there is a disagreement, then listing the observations where the two 
# variables differ will provide information on which persons’ records have 
# errors. For example, if two index variables you just created differ for a 
# record with a person ID 100, display the records for person ID=100. You may 
# see that they have identical start dates but somehow have different finish dates.


exclusions <- exclusions %>% 
  mutate(flag_identical = id_date_ind == id_date_ind2)

exclusions %>% 
  filter(flag_identical == FALSE)

# it'd be helpful to at the other records for just these two children
exclusions %>% 
  filter(synid == 2993 | synid == 5757)

 
# This could be due to recording errors and it is usually difficult to know which 
# finish date is correct without additional information. You can either keep one 
# record assuming this is the correct one or delete both records assuming both are
# not trustworthy. However, we have a small sample and also the difference between
# two finish dates are not very big. You can keep the first record and drop the 
# second one. Now write syntax to drop the case where two index variables differ. 

exclusions <- exclusions %>% 
  filter(flag_identical == TRUE)

# multiple reasons -------------------------------------------------------------

# it'd be helpful to know when there are multiple reasons per exclusion event
# we can make a flag for this using mutate, and the duplicated() function
# which will give TRUE if there are duplicate values for synid



# flag for multiple reasons
exclusions <- exclusions %>% 
  group_by(synid, startdate, finishdate) %>% 
  mutate(multiple_reasons = duplicated(synid))

# note that this only flags TRUE from the second of the multiple reasons
# we can exclude the duplicate reasons by either filtering on
# multiple reasons == FALSE

exclusions %>% 
  filter(multiple_reasons == FALSE)

# or by aggregating the dataset and giving a variable which counts the number 
# of reasons. Summarise performs the same job as filtering here, by collapsing
# the dataset into one row per exclusion event, but with a column listing the
# number of reasons rather than showing whether multiple reasons were present

exclusions %>% 
  group_by(flag, startdate, intaltprov, noprovdays, id_date_ind,
           id_date_ind2, flag_identical) %>% 
  # we have to group by all these variables otherwise summarise will drop them
  summarise(reason_n = n()) %>% 
  arrange(desc(reason_n))


# count number of exclusions that are due to multiple reasons ------------------

# now we want to count the exclusions attirubtable to multiple reasons
# we can use our multiple reasons flag from before. First,
# count the number of cases with each 'level' of the multiple reasons flag
# (i.e. TRUE/FALSE), then divide each of these two numbers by the total number 
# of cases

exclusions %>% 
  group_by(multiple_reasons) %>% 
  count() %>% 
  ungroup() %>% # we have to ungroup() to divide by the total number of cases
  mutate(proportion = n / sum(n))

# calculating the exclusion number ---------------------------------------------
# as we think about merging this dataset with the school census we'll want
# to create a variable which lists each person's exclusion number

# the first thing we need to do is make the exclusions dataset one row per
# event, not one row per reason

exclusions <- exclusions %>% 
  filter(multiple_reasons == FALSE)


# then calculate a variable with the row_number for each person
exclusions <- exclusions %>% 
  group_by(synid) %>% 
  mutate(excl_number = row_number())

# merging with school census ---------------------------------------------------

# read in school census
load("M:/Working with Admin Data Course/synthetic-data/FALSE_DATA_2017_003-school_census-day_2.RData")

# remind ourselves what's in the school census
glimpse(school_census)

# join the two datasets together 

# to comine the datasets we can use the left_join function (see the 
# data wrangling cheat sheet for a helpful illustration) and list the index
# variables as by and flag. Left join matches exclusions to children in the census
# so if there were exclusions which didn't match children these would not be joined
# Even though flag is the same in both datasets we include it as a linking variable
# so R doesn't make two copies of it in the linked dataset

linked_school_cen_excl <- left_join(school_census, exclusions, by = c("synid", "flag"))

# don't worry about the warning - it's telling us that the flag variables
# were of different types in the original two datasets. We don't care about that!

# create a variable showing whether a person has an exclusion ------------------

# we can use mutate with an ifelse statement to make this variable
# ifelse takes three arguments: 1. the logical condition to evaluate,
# 2. what to do if TRUE and 3. what to do if FALSE
# Here our condition is e.g. if the exclusion number is not missing
# What to do if TRUE is value "Exclusion" and if FALSE "No_exclusion"

linked_school_cen_excl <- linked_school_cen_excl %>% 
  mutate(excl_flag = ifelse(!is.na(excl_number), "Exclusion", "No_exclusion"))


# setting exclusion number to zero for those with no exclusions
# we can just adapt our code from yesterday on using replace to change
# the variable excl_number, when excl_number is NA to 0

linked_school_cen_excl %>% 
  mutate(excl_number = replace(excl_number, is.na(excl_number), 0))


# convert from multiple exclusions to single -----------------------------------

# we now want to convert the dataset so that we have one record per child

# one way to do this is for each person to count the number of records per child.
# we group_by the variables in the school census (flag, synid, 
# gender, freemeal, prop_abs_grp, examS4_grp) and excl_flag in order to carry them
# along when we summarise. Save this as a new object

linked_sch_cen_excl_per_id <- linked_school_cen_excl %>% 
  group_by(flag, synid, gender, freemeal, prop_abs_grp, examS4_grp, excl_flag) %>% 
  summarise(n_exclusions = sum(excl_flag == "Exclusion")) %>% 
  ungroup() # it's polite to leave it ungrouped!

# saving the dataset -----------------------------------------------------------

# we want to use our linked data later in the course. Save this in a sensible folder
# with a suitable name (remember our session on workflow!)

# we'll use our syntax as before and save in a specific object to export so we can
# still work on the data in 

#save(linked_sch_cen_excl_per_id, file = "FALSE_DATA_2017_003-school_census_exclusion-day_3.RData")


# we can then count by exclusion and gender
gen_excl_xtab <- linked_school_cen_excl %>% 
  group_by(flag, synid, gender, freemeal, prop_abs_grp, examS4_grp, excl_flag) %>% 
  summarise(n_exclusions = sum(excl_flag == "Exclusion")) %>% 
  group_by(gender, excl_flag) %>% 
  count() %>% 
  filter(!is.na(gender))

# now we want to perform a chi-squared test to see if these counts are different
# than those we would expect by chance

# here's where things get awkward.

# a lot of modelling functions in R don't work so well with %>% so we have
# to change tack and use the $ operator to select the column from our
# summary dataframe to call the chi-square test

model1 <- chisq.test(gen_excl_xtab$n)

# and then print the model results

print(model1)

# EXTRA TASKS testing relationship between gender and multiple exclusions ------

linked_school_cen_excl %>% 
  group_by(flag, synid, gender, freemeal, prop_abs_grp, examS4_grp, excl_flag) %>% 
  summarise(n_exclusions = sum(excl_flag == "Exclusion")) %>% 
  group_by(synid, gender, n_exclusions) %>% 
  filter(!is.na(gender))

# 15.	Extra task if you have time: write syntax to check whether exclusion reasons
# differ between male and female students. You can skip this question. 

# to do this you'll need the exclusions dataset in reason format - restart your R
# session and repeat to before we turn the dataset one row per exclusion (line 146
# in the reference script) and then load in the school_census too.
# Join these files together

linked <- left_join(exclusions, school_census, by = c("synid", "flag"))

linked %>% 
  group_by(gender, incidenttype) %>% 
  count()


# 16.	Extra task if you have time: What is the average number of exclusions for 
# males? How about females? Is there a significant difference in terms of number 
# of exclusions between male and female students?

# remove the dataframes from question 15 and run the whole script again to the 
# start of the extra tasks

linked_sch_cen_excl_per_id %>% 
  group_by(gender) %>% 
  summarise(mean_ex = mean(n_exclusions))


# checking package versions ----------------------------------------------------
sessionInfo()
