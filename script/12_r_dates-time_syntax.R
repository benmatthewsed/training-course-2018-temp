# Working with Admin Data: Dates and Times -------------------------------------
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

# load packages - we need lubridate as well this time
library(tidyverse)
library(lubridate)

# load the data ----------------------------------------------------------------
# read in the exclusions dataset you made on Tuesday again 
load("M:/Working with Admin Data Course/synthetic-data/FALSE_DATA_2017_003-exclusions-day_2.RData")

# reshape ----------------------------------------------------------------------

# we need to make a dataset where the exclusions are unique
# we did this by aggregation before, but we can also do this by spreading the
# dataset into a wide dataset by incident type. To do this first
# create an incident number id as on Tuesday (if necessary) and then
# use this number to spread with incident type
exclusions <- exclusions %>% 
  group_by(synid, startdate, finishdate) %>% 
  mutate(id_date_ind = row_number()) %>% 
  spread(id_date_ind, incidenttype) %>% 
  ungroup() # remember to ungroup!

# NB the new variables are integers surrounded by ``. These allow R to use
# invalid characters as variable names - you're not allowed to start variable
# names with numbers

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
  mutate(finish_date_n = row_number()) %>% 
  arrange(desc(finish_date_n)) 

# dropping cases that are equal to 1 for finish_date_n
exclusions %>% 
  group_by(synid, startdate) %>% 
  mutate(finish_date_n = row_number()) %>% 
  arrange(desc(finish_date_n)) %>% 
  filter(finish_date_n < 2)

# multiple reasons flag --------------------------------------------------------

# we can also use this wide dataset to generate a variable for multiple reasons
# if the second reason column (`2`) is not missing then there were multiple 
# reasons for that exclusion, and then dropping the reason variables.
# we can do this with an ifelse statement: if `2` is not missing (NB takes two
# logical operators to check if a value is not missing), our new variable has
# the value "multiple_reasons". If `2` is missing then the new variable has value
# "single_reason".

# we performed this task earlier on, but it's good to know multiple ways of
# achieving the same end

# EXTRA - we could also code this the other way round, so that if `2` is missing
# the TRUE value is single_reason and the FALSE value is multiple_reasons

exclusions %>% 
  mutate(multi_reason = ifelse(!is.na(`2`), "multiple_reasons", "single_reason"))


# and now drop the `1`, `2` and `3` variables.
exclusions <- exclusions %>% 
  mutate(multi_reason = ifelse(!is.na(`2`), "multiple_reasons", "single_reason")) %>% 
  select(-`1`:-`3`)

# Q5 Check year of exclusion from start date -----------------------------------

# to extract the year from startdate (note that R has helpfully imported this
# as a date variable for us) we can use the year() function from the lubridate
# package. As we're making a new variable we use mutate() as well.

# if the data had been imported as a different format we could convert to
# date using lubridate::ymd(), myd(), dmy() etc. depending on the format

exclusions %>% 
  mutate(start_year = lubridate::year(startdate))

# there shouldn't be any cases after 2011 - let's check this
exclusions %>% 
  mutate(start_year = lubridate::year(startdate)) %>% 
  group_by(start_year) %>% 
  count()

# Uh-oh! there's post-2011 cases shouldn't be there. Let's drop them and save the
# result
exclusions <- exclusions %>% 
  mutate(start_year = lubridate::year(startdate)) %>% 
  filter(start_year <= 2011)

# Q6 School holidays should not be counted when report exclusions --------------
# to start we want to create the month() variable as before, but with month()
# not year() and again to check the distribution

exclusions <- exclusions %>% 
  mutate(start_month = month(startdate))

exclusions %>% 
  group_by(start_month) %>% 
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


# Q7 Days of the week --------------------------------------------------------
# we can use the wday() command from lubridate to find the day of the week,
# again using mutate() to create a new variable. the label = TRUE option
# gives us the output as the day name rather than number.

exclusions <- exclusions %>% 
  mutate(start_dow = lubridate::wday(startdate, label = TRUE))

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
  mutate(start_date = if_else(start_dow == "Sun", startdate + days(1), startdate))

# and we can do the same for Saturday, this time adding two days. Let's save this
# result
exclusions <- exclusions %>% 
  mutate(start_date = if_else(start_dow == "Sun", startdate + days(1), startdate)) %>% 
  mutate(start_date = if_else(start_dow == "Sat", startdate + days(2), startdate))

# we should move the finish dates on as well
exclusions <- exclusions %>% 
  mutate(finish_date = if_else(start_dow == "Sun", finishdate + days(1), finishdate)) %>% 
  mutate(finish_date = if_else(start_dow == "Sat", finishdate + days(2), finishdate))


# calculating duration ---------------------------------------------------------
# to calculate the length of exclusion we can just substract the startdate
# from the finishdate. R will recognize this as a time object and print the number
# of days with "days" next to it

exclusions <- exclusions %>% 
  mutate(excl_duration = finish_date - start_date)

# let's check for missing values
exclusions %>% 
  mutate(excl_duration = finish_date - start_date) %>% 
  filter(is.na(excl_duration))

# okay, there are nine records with missing duration, and we can see that they
# all have missing finish dates. 


# what's the average duration? 
exclusions %>% 
  mutate(excl_duration = finish_date - start_date) %>% 
  summarise(mean_duration = mean(excl_duration))

# ah, that's not right. Better remove the NAs

exclusions %>% 
  mutate(excl_duration = finish_date - start_date) %>% 
  summarise(mean_duration = mean(excl_duration, na.rm = TRUE)) # we need to remove
# the NAs here - we can do this by adding na.rm = TRUE to the mean function

# finding the minimum and maximum durations ------------------------------------
# we can just put these in the same call to summarise. It's simpler now to move
# the filtering out of the NAs to a separate step to avoid writing na.rm = TRUE
# every time

exclusions %>% 
  ungroup() %>% 
  mutate(excl_duration = finish_date - start_date) %>% 
  filter(!is.na(excl_duration)) %>% 
  summarise(mean_duration = mean(excl_duration),
            min_duration = min(excl_duration),
            max_duration = max(excl_duration)) 
  

# does exclusion because of multiple reasons have a longer duration? ----------
exclusions %>% 
  group_by(multi_reason) %>% 
  filter(!is.na(excl_duration)) %>% 
  summarise(mean_duration = mean(excl_duration))
  
# calculating the cumulative duration of exclusions ----------------------------
# we have two options here. If all we want is the sum of exclusion days we can
# aggregate using group_by and summarise, remembering to remove missing values
# convert this variable to numeric afterwards.

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





# Q11 joining the school census and exclusion file -----------------------------

# we now want to join the file we just created to the linked school census file
# we created in the previous session. Let's load this file

load("M:/Working with Admin Data Course/synthetic-data/FALSE_DATA_2017_003-school_census_exclusion-day_3.RData")

# we can use left_join as before to link the two datasets together

linked_edu <- left_join(linked_sch_cen_excl_per_id, exclusions_link, by = c("synid", "flag"))

# if everything's gone right we shouldn't have anyone with a duration value
# who doesn't have an exclusion
linked_edu %>% 
  ungroup() %>% 
  filter(is.na(sum_duration)) %>% 
  count(n_exclusions)

# 12.	Answer the following 2 questions: 
# Is the duration of exclusion associated with gender? 


# we probably need to replace the length of exclusion for those with no exclusions
# with zero

linked_edu %>% 
  mutate(sum_duration = as.numeric(sum_duration)) %>%
  replace_na(list(sum_duration = 0)) %>% 
  group_by(gender) %>% 
  summarise(mean_duration = mean(sum_duration))

# Is the duration of exclusion and number of exclusion correlated?
# again, we need to convert the duration to numeric and then
# replace the missing data with zeros (if we do this again, perhaps
# we should assign this to a new variable...)
# and then use summarise to produce the correlation
# by passing the two variable names to cor()

linked_edu %>% 
  mutate(sum_duration = as.numeric(sum_duration)) %>%
  replace_na(list(sum_duration = 0)) %>% 
  summarise(correlation = cor(n_exclusions, sum_duration))


  

# save the data ----------------------------------------------------------------
save(linked_edu, file = "FALSE_DATA_2017_003-school_census_exclusion-day_3_v2.RData")

# link to the census data ------------------------------------------------------
load("M:/Working with Admin Data Course/synthetic-data/FALSE_DATA_2017_003-census-day_2.RData")

# join to the census dataset with the census data, making sure to keep all children
# in the census data (there aren't any children with school data who don't have
# census data)

linked_df <- left_join(census, linked_edu, by = c("synid", "flag"))

# we can test if there are children with school data but not census data
# by using full_join instead of left_join - see the cheat sheet for more info.

full_join(linked_edu, census, by = c("synid", "flag"))

# save the dataset for use later in the course
save(linked_df, file = "FALSE_DATA_2017_003-census-education.RData")

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
