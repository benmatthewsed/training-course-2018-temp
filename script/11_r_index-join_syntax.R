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



# Instructions ------------------------------------------------------------

# In the exercise you will be asked to write syntax that performs some indexing 
# and joining tasks. At the end of the practical you will answer a few questions 
# and produce files for further exercises. One of main objectives of this exercise
# is to create a variable on number of exclusions which will be used in risk 
# factor analysis for becoming NEET.


# load the packages ------------------------------------------------------------
library(tidyverse)

# 1. load data --------------------------------------------------------------------
# read in the dataset you made yesterday, or the .RData file called exclusions day two

exclusions <- read_csv("synthetic-data/FALSE_DATA_2018_exclusions_day1.csv")

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

# 2. Check year of exclusion from start date -----------------------------------

# (NB this is the same process as in the dates and times session)

# to extract the year from startdate (note that R has helpfully imported this
# as a date variable for us) we can use the year() function from the lubridate
# package. As we're making a new variable we use mutate() as well.

# if the data had been imported as a different format we could convert to
# date using lubridate::ymd(), myd(), dmy() etc. depending on the format

exclusions %>% 
  mutate(startyear = lubridate::year(startdate))

# there shouldn't be any cases after 2011 - let's check this
exclusions %>% 
  mutate(startyear = lubridate::year(startdate)) %>% 
  group_by(startyear) %>% 
  count()

# Uh-oh! there's post-2011 cases shouldn't be there. Let's drop them and save the
# result

# NOTE - census day was 27th March 2011. We can use logical operators
# to filter for any dates after the census date, but you'll need to tell R
# that our reference date is in date format

exclusions <- exclusions %>% 
  filter(startdate < lubridate::ymd("2011-03-27"))


# Finding errors ---------------------------------------------------------------

# Each exclusion should have a unique start and finish date. But it might not!

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

# We're going to resolve some of these problems now.

# 3. convert to wide dataset by exclusion reason --------------------------

# first create a variable for reason number for each person, start date and
# end date

exclusions <- 
  exclusions %>% 
  group_by(synid, 
           startdate, finishdate) %>% 
  mutate(reason_n = row_number()) %>% 
  ungroup()


# 4. spread to wide -------------------------------------------------------

# then use spread() to reshape into a wide format using the reason_n and
# incidentype variables. This will give one row per exclusion. We need
# to remove the id_date variables though, as these are not unique for each
# exclusion

exclusions_wide <- 
exclusions %>% 
  select(flag:incidenttype, noprovdays, intaltprov, reason_n) %>% 
  spread(reason_n, incidenttype)

# we can rename this variables from `1` (note the backticks `` to reason_1 etc)

exclusions_wide <- 
exclusions_wide %>% 
  rename(reason_1 = `1`, reason_2 = `2`, reason_3 = `3`)
  

# 5. checking the transformation ------------------------------------------

# each exclusion episode should be characterised by identical startdate and 
# finishdate. Create a new variable for each person and startdate that counts
# the number of finishdates, then see if all people have 1 for this variable

exclusions_wide %>% 
  group_by(synid, startdate) %>% 
  mutate(finishdate_n = row_number()) %>% 
  filter(finishdate_n > 1)

exclusions_wide %>% 
  filter(synid == 5757)

# 6. replace reason_2 for the second exclusion record --------------------

# replace the variables of the second exclusion reason for the record 
# of the person identified above
# make an assumption that penalty is likely to be longer for multiple reasons
# by keeping finishdate that gives a longer duration
# (ignore noprovdays here)


exclusions_wide <- 
exclusions_wide %>% 
  arrange(synid, startdate, finishdate) %>% 
  mutate(reason_2 = if_else(synid == 5757 &
                              startdate == lubridate::ymd("2008-02-19"),
                            dplyr::lag(reason_1), reason_2))



# 7. remove the duplicate record ------------------------------------------

exclusions_wide <- 
exclusions_wide %>% 
  filter(synid != 5757 |
           finishdate != lubridate::ymd("2008-02-20"))

# NB you can check this is successful because the number of cases should
# decrease by one. 



# 8. make multi-reason exclusion variable --------------------------------

# any exclusion which has a reason_2 listed must have multiple
# exclusions, so we can make this variable using if_else and selecting
# is not missing for reason_2 with !is.na(reason_2)

exclusions_wide <- 
exclusions_wide %>% 
  mutate(multi_reason_exclusion = if_else(!is.na(reason_2),
                                          "Multiple reasons",
                                          "Single reason"))



# 9. making multiple exclusions per child variable ------------------------

exclusions_wide <- 
exclusions_wide %>% 
  group_by(synid) %>% 
  mutate(exclusion_number = row_number())

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
