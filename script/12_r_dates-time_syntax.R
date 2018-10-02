# Working with Admin Data: Dates and Times -------------------------------------
# B Matthews, L Forrest, B Nowok and  Z Feng
# October 2018, ADRC-S, University of Edinburgh

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


# Exercise instructions ---------------------------------------------------

# In the exercise you will be asked to write syntax that performs date handling
# tasks. One of main objectives of this exercise is to create a variable on 
# duration of exclusions which will be used in risk factor analysis for becoming
# NEET.



# load packages - we need lubridate as well this time
library(tidyverse)
library(lubridate)

# 1. load the data ----------------------------------------------------------------
# read in the exclusions dataset you made on Tuesday again 
load("")

# errors in finish dates -------------------------------------------------------

# Q3 check whether there are errors in finish dates
# Error in recording of finish date occurs if the finish dates differ 
# for episodes have identical start dates 
# this is not picked up when using the duplicate function 
# in the indexing and joining session
# it will be difficult to decide which finish date is correct. There are at least three 
# options here:
# 1. move the finish date to the middle of two finish dates (average)
# 2. delete the records, assuming this is not accurate 
# 3. randomly pick one record, assuming the difference is small and won't affect
# the modelling/analysis results
# let's take option 3 and keep the first record

# an finish date index - for each person and startdate count the number of rows
exclusions %>% 
  group_by(synid, startdate) %>% 
  mutate(finishdate_n = row_number()) %>% 
  arrange(desc(finishdate_n)) 

# dropping cases that are equal to 1 for finishdate_n
exclusions %>% 
  group_by(synid, startdate) %>% 
  mutate(finishdate_n = row_number()) %>% 
  arrange(desc(finishdate_n)) %>% 
  filter(finishdate_n < 2)

# Q2 Check year of exclusion from start date -----------------------------------

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
  filter(startdate < ymd("2011-03-27"))

# Q6 School holidays should not be counted when report exclusions --------------
# to start we want to create the month() variable as before, but with month()
# not year() and again to check the distribution

exclusions <- exclusions %>% 
  mutate(startmonth = month(startdate))

exclusions %>% 
  group_by(startmonth) %>% 
  count()

# seems odd that there are dates during the school holidays!
# For now we can keep this in. What would you do if you found this during
# a 'live' analysis?


# ADVANCED ---------------------------------------------------------------------
# lubridate offers a specific operator to test whether a value falls within
# a given interval - %within%. Let's see how this works
# let's arbitrarily say that the school holidays in 2009 fell between
# 2009-06-28 and 2009-08-18. We can create an interval object summer_09
# which holds this duration by passing two date objects to interval()

summer_09 <- interval(ymd("2009-06-28"), ymd("2009-08-18"))

# we can then use mutate() and %within% to show if an exclusion falls within
# this range

exclusions %>% 
  mutate(hols_09 = ifelse(startdate %within% summer_09, 1, 0)) %>%
  filter(hols_09 == 1)


# Q4 Days of the week --------------------------------------------------------
# we can use the wday() command from lubridate to find the day of the week,
# again using mutate() to create a new variable. the label = TRUE option
# gives us the output as the day name rather than number.

exclusions <- exclusions %>% 
  mutate(startdow = lubridate::wday(startdate, label = TRUE))

# if the start date is sunday, add one day

# to perform this we need two extra operations: a new version of ifelse
# and the days() command from lubridate.
# We can set this up in the usual way by using mutate to construct a new variable
# where we add one to startdate if the day of the week is Sunday, and if it's not
# sunday returning the original start date. Rather than just adding one to startdate
# we add startdate + days(1) so R knows to add one day to the date and so will
# work with month ends and so on. Confusingly, we also need to use a new if/else
# command - if_else. This is stricter than the usual ifelse in that it makes
# the output variables be the same type as the inputs - in this case it gives
# use a date column as output rather than converting to a numeric value (which
# we don't want). ifelse and if_else still trip me up so don't worry
# if this is confusing! The key is knowing if you want the output in the same
# format as the input. See ??if_else for more info.

# NB this might take a second to run!

exclusions %>% 
  mutate(startdate = if_else(startdow == "Sun", startdate + days(1), startdate))

# and we can do the same for Saturday, this time adding two days. Let's save this
# result
exclusions <- exclusions %>% 
  mutate(startdate = if_else(startdow == "Sun", startdate + days(1), startdate)) %>% 
  mutate(startdate = if_else(startdow == "Sat", startdate + days(2), startdate))

# we should move the finish dates on as well
exclusions <- exclusions %>% 
  mutate(finishdate = if_else(startdow == "Sun", finishdate + days(1), finishdate)) %>% 
  mutate(finishdate = if_else(startdow == "Sat", finishdate + days(2), finishdate))


# calculating duration ---------------------------------------------------------


# It's good practice to check what format the variable is though so let's do
# that first using dplyr::glimpse

dplyr::glimpse(exclusions)

# we can take a look using View()

# Visually inspect the data using View(). Move down to person 90 (synid=90) you will find 
# that the first three records have identical start dates (and finish dates as well). 

# checking data for person 90
View(exclusions)

# so this confirms the structure of the dataset - by exclusion *reason* not
# exclusion event. We need to be aware of this when calculating summary statistics
# as we might double-count exclusions with multiple reasons!


# checking for duplcated start dates

# it'd be helpful to know when there are multiple reasons per exclusion event
# we can make a flag for this using mutate, and the duplicated() function
# which will give TRUE if there are duplicate values for synid



# flag for multiple start dates by person
exclusions <- 
exclusions %>% 
  group_by(synid, startdate) %>% 
  mutate(multiple_start = duplicated(synid)) %>% 
  ungroup()

# flag for multiple start and end dates by person
exclusions <- 
  exclusions %>% 
  group_by(synid, startdate, finishdate) %>% 
  mutate(multiple_start_end = duplicated(synid)) %>% 
  ungroup()



# 6. comparing multiple start and end dates -------------------------------

# Compare two duplicate indicators. Do the two indicators agree? If not, 
# browsing the data visually and inspect the observations to check what is going
# on.

# We can compare these two indicators using the count() function in dplyr

exclusions %>% 
  count(multiple_start, multiple_start_end)

# this shows that there's a case where a person's exclusions have the same start
# dates but different end dates

# let's look at only these exclusions using filter() and View()

exclusions %>% 
  filter(multiple_start != multiple_start_end) %>% 
  View()

# this shows us that it's person 5757, so we can look at all their exclusions

exclusions %>% 
  filter(synid == 5757) %>% 
  View()



# let's remove the exclusion which had the shortest duration.
# it is difficult to know which is correct one. However, it seems that this was
# an exclusion due to multiple reasons and we assume that the punishment is 
# severe and longer. Thus let's replace the one that had shorter duration.
# Conveniently, this is the one where multiple_start and multiple_start_end
# don't match, so we can use this as a logical condition to if_else
# to make the new variable

exclusions <- 
exclusions %>% 
  group_by(synid) %>% # for each person
  mutate(finishdate_lead = if_else(multiple_start != multiple_start_end,
                                   dplyr::lag(finishdate),
                                   finishdate))

# Now drop the original finishdate variable and rename the new finishdate_lead
# as finishdate

exclusions <- 
exclusions %>% 
  select(-finishdate) %>% 
  rename(finishdate = finishdate_lead)


# count number of exclusions that are due to multiple reasons ------------------

# multiple reasons -------------------------------------------------------------

# it'd be helpful to know when there are multiple reasons per exclusion event
# we can make a flag for this using mutate, and the duplicated() function
# which will give TRUE if there are duplicate values for synid



# flag for multiple reasons
exclusions <- exclusions %>% 
  group_by(synid, startdate, finishdate) %>% 
  mutate(multiple_reasons = duplicated(synid))

# note that this only flags TRUE from the second of the multiple reasons
# we can exclude the duplicate reasons by filtering on
# multiple reasons == FALSE

exclusions %>% 
  filter(multiple_reasons == FALSE)




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


# For now we only want to keep one record per exclusion. First, let's create
# a variable which counts the reason number

exclusions <- 
exclusions %>% 
  group_by(synid, 
           startdate, finishdate) %>% 
  mutate(reason_n = row_number()) %>% 
  ungroup()

# then we can keep only the highest reason_n for each exclusion - this gives
# us the number of reasons per exclsion. We should then also drop the
# incident type variable. We can do tthis by filtering by the condition
# max(reason_n) for each person, startdate and finishdate

exclusions <- 
exclusions %>% 
  group_by(synid, startdate, finishdate) %>% 
  filter(reason_n == max(reason_n)) %>% 
  ungroup() %>% 
  select(-incidenttype)


# 10. Calculating duration ------------------------------------------------

# to calculate the length of exclusion we can just substract the startdate
# from the finishdate. R will recognize this as a time object and print the number
# of days with "days" next to it.


exclusions <- exclusions %>% 
  mutate(excl_duration = finishdate - startdate)

# let's check for missing values
exclusions %>% 
  mutate(excl_duration = finishdate - startdate) %>% 
  filter(is.na(excl_duration))

# okay, there are nine records with missing duration, and we can see that they
# all have missing finish dates. 


# what's the average duration? 
exclusions %>% 
  mutate(excl_duration = finishdate - startdate) %>% 
  summarise(mean_duration = mean(excl_duration))

# ah, that's not right. Better remove the NAs

exclusions %>% 
  mutate(excl_duration = finishdate - startdate) %>% 
  summarise(mean_duration = mean(excl_duration, na.rm = TRUE)) # we need to remove
# the NAs here - we can do this by adding na.rm = TRUE to the mean function

# finding the minimum and maximum durations ------------------------------------
# we can just put these in the same call to summarise. It's simpler now to move
# the filtering out of the NAs to a separate step to avoid writing na.rm = TRUE
# every time

exclusions %>% 
  ungroup() %>% 
  mutate(excl_duration = finishdate - startdate) %>% 
  filter(!is.na(excl_duration)) %>% 
  summarise(mean_duration = mean(excl_duration),
            min_duration = min(excl_duration),
            max_duration = max(excl_duration)) 
  

# does exclusion because of multiple reasons have a longer duration? ----------
exclusions %>% 
  group_by(reason_n) %>% 
  filter(!is.na(excl_duration)) %>% 
  summarise(mean_duration = mean(excl_duration))
  
# calculating the cumulative duration of exclusions ----------------------------
# we have two options here. If all we want is the sum of exclusion days we can
# aggregate using group_by and summarise, remembering to remove missing values
# convert this variable to numeric afterwards (as R will 'helpfully' code this
# as a time variable). (The second option is in advanced below)

exclusions %>% 
  group_by(synid) %>% 
  filter(!is.na(excl_duration)) %>% 
  summarise(sum_duration = sum(excl_duration)) %>% 
  mutate(sum_duration = as.numeric(sum_duration))

# this gives us a dataset with one value per person. This is the dataset we'd
# like to link, so let's assign this to a new object (remember to keep the flag
# variable)
exclusions_link <- exclusions %>% 
  group_by(synid, flag) %>% 
  filter(!is.na(excl_duration)) %>% 
  summarise(sum_duration = sum(excl_duration)) %>% 
  mutate(sum_duration = as.numeric(sum_duration))




# save the data ----------------------------------------------------------------
save(exclusions_link, 
     file = here::here("synthetic-data", "FALSE_DATA_cumulative_exclusions.RData"))

# ADVANCED ---------------------------------------------------------------------

# The second option to calculate exclusion duration introduces us to window operators. 
# R has the cumulative function cumsum() which will happily calculate 
# the cumulative lenth of exclusion for us for each person and add this to
# each exclusion event, maintaining our structure of one record per event.
# unfortunately we can't use cumsum on a date object (as is out excl_duration variable)
# so we'll first have to convert this to an integer. We can do this with an
# initial call to as.numeric


exclusions %>% 
  mutate(excl_duration = as.numeric(excl_duration)) %>% 
  group_by(synid) %>% 
  filter(!is.na(excl_duration)) %>% 
  mutate(cum_duration = cumsum(excl_duration)) %>% View

# checking package versions ----------------------------------------------------
sessionInfo()
