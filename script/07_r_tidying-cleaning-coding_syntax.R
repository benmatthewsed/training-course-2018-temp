# Working with Admin Data: Data Tidying, Cleaning and Coding -------------------
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

# press Ctrl-shift-f10 to clear the workspace and rest the R session
# doing this periodically and re-running code lets you make sure you have all the
# important information in your code (reproducible) not in the workspace (not
# reproducible)

# load the packages ------------------------------------------------------------
# we're going to use the tidyverse package, so we need to load this
library(tidyverse)

# load the data ----------------------------------------------------------------
# start with the school census and read this in to an object called census

census <- read_csv("M:/Working with Admin Data Course/synthetic-data/false_data_2017_003_census.csv")

# summarising the dataset ------------------------------------------------------

# first take a few minutes to see what's in the dataset

# glimpse will give us high-level information about all the variables
glimpse(census)

# summary takes a more statistical approach
summary(census)

# hmmm, our dataset has imported all the variables as characters
# we might want these as factors (it's easier to see what values they take this way)
# to convert all character vectors to factors we can use a conditional mutate -
# mutate_if. If a vector meets the condition is.character (first argument)
# it's converted to a be as.factor (the second argument).
# the map() function lets us apply a function to all the variables in the dataset
# if we pass levels() to map() [map(levels)] R will print out the levels for each
# variable in the dataset, printing NULL for numeric variables
# this is an example of iteration as we discussed in the workflow session

census %>% 
  mutate_if(is.character, as.factor) %>% 
  map(levels)

# summarize will give us a more informative output now
# (note that we need to convert to factor again now because we didn't assign this
# anywhere). If we copy/paste this again though we'll need to assign this to keep
# our code DRY

census %>% 
  mutate_if(is.character, as.factor) %>% 
  summary()

# So what do we need to do here?
# 1. rename the variable names
# 2. look at outliers
# 3. relabel the factor levels for ecop1
# 4. look at no_sibs_grp - this has been imported as numeric

# initial exploration ----------------------------------------------------------

# Note the number of records in the dataset, how many variables there are, 
# the type of variables, how many unique values there are per variable, 
# the range of values, whether there is missing data. What does each variable record? 

#  for example, look at the minimum and maximum values of agep0. All the children
# in our dataset should be between 6 and 9!

# we might want to take a closer look at the age variable. We know that agep0
# gives the person's age on census night 2001. We can then perform a rough check
# on the accuracy of this variable by substracting the person's birth year from
# from 2001 (we assume that birth year is accurate because the SLS sampling
# frame is based on accurate recording of DOB)

# mutate -----------------------------------------------------------------------

# We can create a new variable using mutate(). This is an unbelieveably helpful
# command that we're going to use A LOT. mutate() takes the name of the
# new variable = some condition which describes the new variable. Here we want
# a new variable which calculates the person's age by 2001 - their birth year

census %>% 
  mutate(calc_age = 2001 - slsdobyr)

# note that we haven't assigned this result to the census object, so it just
# prints to the console.

# once we've created our own age variable we can calculate the difference between
# this calculated variable and agep0:

census %>% 
  mutate(calc_age = 2001 - slsdobyr)%>% 
  mutate(age_diff = calc_age - agep0)

# There might be some discrepancies here because the census was taken in late April
# so differences of 1 between the two ages are to be expected. Instead, we can
# filter to only look for differences of greater than 1

census %>% 
  mutate(calc_age = 2001 - slsdobyr)%>% 
  mutate(age_diff = calc_age - agep0)%>% 
  filter(age_diff > 1)

# We can see from the console that there are 147 rows which meet this criterion.
# We might err the other way though, so better add another condition:
# a difference greater than one OR a difference of less than -1

census %>% 
  mutate(calc_age = 2001 - slsdobyr)%>% 
  mutate(age_diff = calc_age - agep0)%>% 
  filter(age_diff > 1 | age_diff < -1)


# We can use select() and arrange() to take a closer look at our derived variable
census %>% 
  mutate(calc_age = 2001 - slsdobyr)%>% 
  mutate(age_diff = calc_age - agep0)%>% 
  filter(age_diff > 1 | age_diff < -1) %>% 
  select(age_diff, calc_age, agep0) %>% 
  arrange(desc(age_diff))

# this suggests two things to look at - the discrepancies in age and ages coded
# -999 - this is missing in SLS. We'll come to the discrepancies later on.
# For now, decide what to do with these erroneous values: delete them? 
# Code them as "error"? Recode them using the derived values?
# There isn't really a right answer here - but make sure that you're
# clear in your documentation what you've done and why!


# missing data -----------------------------------------------------------------

# Because we know that for agep0 -999 is missing we can happily recode this
# to be missing in a way that R understands.

# the replace() function provides a handy way to do this. This takes three
# arguments: 1. a vector (the variable in which we want to replace the values),
# 2. a way to identify which values to replace (either by position or in our case
# because agep0 equals -999) and 3. what to replace the specified value with
# - here NA, R's way of noting missing values

census %>% 
  mutate(agep0 = replace(agep0, agep0 == -999, NA))

# We might also want to recode the factors which have levels of "Missing" to have
# this value as NA. We can do this by changing all the factors with a value of 
# Missing to NA using the same structure as above:

census %>% 
  mutate(ademh0 = replace(ademh0, ademh0 == "Missing", NA))


# ADVANCED!
  
# Or we can use a conditional mutate to change Missing to NA for *all* factor
# variables. This takes the same structure of replace(variables, condition, new value)
# but uses the dot (.) assignment to change all the variables which meet the condition
# set by mutate_if - that the variable is a factor (is.factor). This . notation
# comes up a lot so it's useful to know about! Finally, this call to replace is
# wrapped in a call to factor() which lets R know we want to remove "Missing" from
# the list of potential levels for the variables we're changing (try omitting 
# the factor() call to see what happens). Because we're sending a call with
# multiple options to mutate_if we have to tell R hwo to interpret this by putting
# the whole factor(replace()) call within funs()

census %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.factor, funs(factor(replace(., . == "Missing", NA))))


census %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.factor, funs(factor(replace(., . == "Missing", NA))))

# There would be circumstances where replacing these missing values are
# undesirable though. By default R drops NAs from analyses and plots. You might
# want to explore these a bit more before removing them. We're going to talk more
# on Thursday about missing data

# To get a grasp of the structure of your missing data, try the aggr() function
# from library(VIM). It produces an awesome plot of missingness!


# changing variable type -------------------------------------------------------

# Sometimes we want to change the way that R has coded a variable. This is
# because default options for different variable types differ. For example,
# here R has coded birth month as an integer. It'd be more helpful for us to
# have this as a factor with the name of the month rather than the month
# number. First we can change the type with a call to as.factor within mutate
# and then change levels with recode

census %>% 
  mutate(slsdobmt = as.factor(slsdobmt)) %>% 
  mutate(slsdobmt= recode(slsdobmt,
                              "1" = "Jan",
                              "2" = "Feb",
                              "3" = "Mar",
                              "4" = "Apr",
                              "5" = "May",
                              "6" = "Jun",
                              "7" = "Jul",
                              "8" = "Aug",
                              "9" = "Sep",
                              "10" = "Oct",
                              "11" = "Nov",
                              "12" = "Dec"))


# aggregation ------------------------------------------------------------------

# Before we move on to some more tidying, it's worth having a look at aggregation
# and grouping. These summaries will be invaluable when working out what's happened
# to your dataset when you make a change!

# To illustrate how these functions work, say
# we want to know the mean age of children in the data. We can find this 
# out using summarise(). Again this prints the result to the console:

census %>% 
  summarise(mean_age = mean(agep0))

# a specialised version of summarise is count() which gives the number of cases

census %>% 
  count()

# count(), and summary functions in general, are most helpful when used to examine
# different groups...

# group_by ---------------------------------------------------------------------

# Say we want to find the mean age for boys and girls. We can add a call to group_by
# to perform the next action for each group in sex0 and then use the same 
# summarise call as before

# let's start just by sending a call to group_by
census %>% 
  group_by(sex0)

# at the top of the console output it's listed Groups:   sex0 [3]. This tell us
# the dataset is grouped by sex0 which has three levels (NB one of these is Missing)

census %>% 
  group_by(sex0) %>% 
  summarise(mean_age = mean(agep0))

# we can also add a standard deviation column in the same call to summarise
# if we're interested in that too
census %>% 
  group_by(sex0) %>% 
  summarise(mean_age = mean(agep0),
            sd_age = sd(agep0))

# and also a count using n() which returns the number of cases
census %>% 
  group_by(sex0) %>% 
  summarise(mean_age = mean(agep0),
            sd_age = sd(agep0),
            n = n())

# (NB summarise(n = n()) gives the same result as count(), but we can't call
# count() within another call to summarise)

# say we want to see the mean age by sex and tenure? We can just add another
# variable to group_by and then pass the same calls as before

census %>% 
  group_by(sex0, newten) %>% 
  summarise(mean_age = mean(agep0),
            sd_age = sd(agep0),
            n = n())

# say we want to calculate the proportion of children in different local authorities
# with different tenure arrangements. We can calculate the numbers of children
# in different councils with different tenure status with:
census %>% 
  group_by(newten, councilarea) %>% 
  count()

# this is like saying for each tenure and council area count the number of children
# if we add a call to mutate we can calculate the proportion - for each council area
# divide the number of children in each tenure by the total children in that council area.
# we need to add another group_by call here because to calculate the proportion
# we're performing the action for each council area only, not each council
# are and tenure (try adding newten to the second group_by call and see what happens)

census %>% 
  group_by(newten, councilarea) %>% 
  count() %>% 
  group_by(councilarea) %>% 
  mutate(proportion = n / sum(n)) %>% 
  arrange(councilarea)



# changing factor levels -------------------------------------------------------

# we can recode multiple variables levels this way. Let's recode the ethnic group
# variable to make it a bit simpler, by tunring the different White codes into "White",
# the Black, Asian and Minority Ethnic into BAME and combining NCR (non-resident students)
# with missing. (This is the same call as when recoding month names before)

census %>% 
  mutate(ethnic_grp = recode(ethgrp0,
                          "African" = "BAME",
                          "Any Mixed Background" = "BAME",
                          "Bangladeshi" = "BAME",
                          "Black Scottish or Other Black" = "BAME",
                          "Caribbean"  = "BAME",
                          "Chinese" = "BAME",
                          "Indian" = "BAME",
                          "Missing" = "Missing",
                          "NCR (non-resident students)" = "Missing",
                          "Other Ethnic Group" = "BAME",
                          "Other South Asian" = "BAME",
                          "Other White" = "White",
                          "Other White British" = "White",
                          "Pakistani" = "BAME",
                          "White Irish" = "White",
                          "White Scottish" = "White"))
  
# this is a whole load of typing though, so we can use fct_collapse instead
# to only specify the new groups once

census %>% 
  mutate(ethnic_grp = fct_collapse(ethgrp0,
                             BAME = c("African", "Any Mixed Background", "Bangladeshi",
                                      "Black Scottish or Other Black", "Caribbean",
                                      "Chinese", "Indian", "Other Ethnic Group", 
                                      "Other South Asian", "Pakistani"),
                             White = c("Other White", "Other White British",
                                       "White Irish", "White Scottish"),
                             Missing = c("Missing", "NCR (non-resident students)"))) %>% 
  count(ethnic_grp)

# we can also just bung a bunch of factors together when we have lots using fct_lump
# this splits up factors into handy categories based on the frequency of observations
# making sure that the Other group created is smaller than the largest factor level
census %>%
  mutate(ethnic_grp = fct_lump(ethgrp0)) %>%
  count(ethnic_grp)

# this only gives us two groups! We may want a bit more nuance than that, so we can
# select the number of groups we'd like by adding an argument n = e.g. 5
census %>%
  mutate(ethnic_grp = fct_lump(ethgrp0, n = 5)) %>%
  count(ethnic_grp)


# factor reorder ---------------------------------------------------------------
# one potentially helpful feature of factors in R is that they have an order.
# This isn't so much help though if the order is messed up!
# let's take a look at the help1 variable:

census %>% 
  mutate_if(is.character, as.factor) %>% 
  count(help1)

# we have factor levels that should be structured based on number of hours a week
# but R has imported these in a bonkers order.
# We can used fct_relevel to change this

census <- census %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(hours_help = fct_relevel(help1,
                                  "(1) No",
                                  "(2) Yes, 1-19 hours a week",
                                  "(5) Yes, 20-34 hours a week",
                                  "(6) Yes, 35-49 hours a week",
                                  "(4) Yes, 50+ hours a week")) 

census %>% 
  count(hours_help)

# note that the levels we didn't specify - No code required and NA - end up at
# the end of the factor

# recoding ademh0, crh0 and sex0 -----------------------------------------------
# We're going to use these skills to tidy up some other factor levels now.

# 8.	recode ademh0 as there are low numbers of households with 3 and 4 employed
#adults in household. Create the following groups: 0, 1, 2, 3+ employed adults, 
#and recode missing group to . Label the new variable and add value labels.

# we can use fct_collapse and mutate for this, remembering that
# the new values go on the left hand side of the =

census <- census %>% 
  mutate(ademh0 = fct_collapse(ademh0,
                               "0" = "0",
                               "1" = "1",
                               "2" = "2",
                               "3+" = "3",
                               "3+" = "4",
                               NULL = "Missing"))

# 9.	recode crsh0 as there are low numbers of households with multiple numbers 
# of carers. Dichotimise to 0 carers v 1 or more carers, and recode missing group 
# to NA Label the new variable and add value labels.

# this works in the same fashion as before

census <- census %>% 
  mutate(crsh0 = fct_collapse(crsh0,
                               "0 carers in household" = "0 carers in household",
                               "1+ carers in household" = c("1 carer in household",
                                                            "2 carers in household",
                                                            "3 carers in household",
                                                            "4 carers in household"),
                               NULL = "Missing"))


# 11.	sex0 has missing data category. Recode to a dichotomous variable and and 
# recode the missing group to NA Label the new variable and add value labels.

# this is a job for fct_recode not factor collapse, as we only want to change
# one level of the factor

census <- census %>% 
  mutate(sex0 = fct_recode(sex0, NULL = "Missing"))

census %>% 
  count(sex0)


# deriving the NEET variable ---------------------------------------------------
# Some of the labels are a bit confusing. Assume that 8:(7)Economically active 
# but unemployed = NEET. Economically inactive codes that are not students should 
# also be coded as NEET ie 16, 18-20 but not 17: (16) Economically inactive: student

# to recode we can use fct_collapse as before. (Hint, we can see the levels
# of ecop1 using group_by and count).

# Hint - to convert the factor level to NA we need to assign it to NULL

census <- census %>% 
  mutate(neet = fct_collapse(ecop1,
                             non_neet = c("(1) Economically active (excluding full-time students): In employment: Employee, part-time", 
                                          "(2) Economically active (excluding full-time students): In employment: Employee, full-time",
                                          "(4) Economically active (excluding full-time students): In employment: Self-employed with employees, full-time",
                                          "(5) Economically active (excluding full-time students): In employment: Self-employed without employees, part-time",
                                          "(6) Economically active (excluding full-time students): In employment: Self-employed without employees, full-time",
                                          "(8) Economically active full-time students: In employment: Employee, part-time",
                                          "(9) Economically active full-time students: In employment: Employee, full-time",
                                          "(10) Economically active full-time students: In employment: Self-employed with employees, part-time",
                                          "(11) Economically active full-time students: In employment: Self-employed with employees, full-time",
                                          "(12) Economically active full-time students: In employment: Self-employed without employees, part-time",
                                          "(13) Economically active full-time students: In employment: Self-employed without employees, full-time",
                                          "(14) Economically active full-time students: Unemployed: Seeking work and available to start in 2 weeks or waiting to st",
                                          "(16) Economically inactive: Student"),
                             neet = c("(17) Economically inactive: Looking after home or family",
                                      "(18) Economically inactive: Long-term sick or disabled",
                                      "(19) Economically inactive: Other",
                                      "(7) Economically active (excluding full-time students): Unemployed: Seeking work and available to start in 2 weeks or wa"),
                             NULL = c("(-88) No code required")))


# save this variable to the census object and then save the whole dataset for use
# the rest of the week


# exploring the school census --------------------------------------------------
school_census <- read_csv("M:/Working with Admin Data Course/synthetic-data/FALSE_DATA_2017_003_school_census_ver02_v3.csv")

# glimpse will give us high-level information about all the variables
glimpse(school_census)

# summary takes a more statistical approach
summary(school_census)

# recoding prop_abs_grp
#in the school_census dataset reorder the levels of prop_abs_grp and 
# examS4_grp so they run numerically low to high

school_census <- school_census %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(prop_abs_grp = fct_relevel(prop_abs_grp,
                                    "< 5",
                                    ">= 5 & <10",
                                    ">= 10 & < 20",
                                    ">= 20"),
         examS4_grp = fct_relevel(examS4_grp,
                                  "0",
                                  "1-3",
                                  "4-6",
                                  ">6"))



# recoding type of variable in exclusions --------------------------------------
load("M:/Working with Admin Data Course/synthetic-data/false_data_2017_003_exclusions_ver02.RData")

# note that this is a different file format from before! and we didn't assign
# this object to anything here - R reads in .Rdata files without assignment.

# the first thing I suggest is to give this a more informative name, like "exclusions"

exclusions <- syndati


# we can find out a bit more about this object using str(). This gives information
# about the structure of the object
str(exclusions)

# try this with the census data
str(census)

# notice that census has two extra classes: tbl_df and tbl.
# census is a different kind of object, a tibble not a dataframe
# the differences don't matter a huge amount for us right now
# (you can read about them here - http://r4ds.had.co.nz/tibbles.html)
# other than that tibbles have nicer printing options to the console. 
# let's change syndati to a tibble using as.tibble

exclusions <- as.tibble(exclusions)

# glimpse will give us high-level information about all the variables
glimpse(exclusions)

# summary takes a more statistical approach
summary(exclusions)

# noprovdays has been imported as a character vector. Let's change that using
# mutate and as.numeric
exclusions <- exclusions %>% 
  mutate(noprovdays = as.numeric(noprovdays))

# we can then change our unsightly -999 values to be NA using na_if. this takes
# replaces any values of x that are equal to y with NA in form na_if(x, y)

exclusions <- exclusions %>% 
  mutate(noprovdays = na_if(noprovdays, -999))

# it'd probably be worth converting the incidenttype and intaltprov variables
# to factors as well, as before

exclusions <- exclusions %>% 
  mutate_if(is.character, as.factor)

  
  
# Summary ----------------------------------------------------------------------
  
# so we've now cleaned up our factor levels, handled our missing data and
# converted strings to numeric. Let's save these files as our processed files
# for use later in the course. Use sensible names and save them in a sensible
# place in the folder structure. Normally I'd suggest that we save our files
# as .csv as this is the simplest and most durable type. However this would
# strip away some of our hard work, so let's save them as R objects.

save(census, file = "FALSE_DATA_2017_003-census-day_2.RData")
save(school_census, file = "FALSE_DATA_2017_003-school_census-day_2.RData")
save(exclusions, file = "FALSE_DATA_2017_003-exclusions-day_2.RData")

# ADVANCED - spread and gather -------------------------------------------------


# create a wide dataset.
# first calculate a reason number for each person, startdate and finishdate
# then use spread() to turn the dataset wide with the columns being the values
# of this reason number variable and the observations being the incidenttype

# save this as another object (suitably named)

excl_wide <- syndati %>% 
  group_by(synid, startdate, finishdate) %>% 
  mutate(reason_n = row_number()) %>%
  spread(reason_n, incidenttype) %>% 
  rename(reason_1 = `1`,
         reason_2 = `2`,
         reason_3 = `3`)

glimpse(excl_wide)

# now convert to long with gather, creating new variables called
# "reason_n" and "incident_type", gathering columns 7 to 9


exclusions <- excl_wide %>% 
  gather("reason_n", "incident_type", 7:9)

glimpse(exclusions)

# wait what? the number of observations has tripled!
# have a look at the dataset and see what that's happened

View(exclusions)

# hint - you might have to scroll down a wee bit

# converting wide to long has added lots of NAs where there were extra reasons
# we can change this behaviour in the call to gather by adding na.rm = TRUE

excl_wide %>% 
  gather("reason_n", "incident_type", 7:9, na.rm = TRUE)

# or we can just filter out those which aren't missing by combining the operators
# ! and is.na (which checks whether a value is NA)

exclusions <- exclusions %>% 
  filter(!is.na(incident_type))


# working with strings ---------------------------------------------------------

# for this exercise we're going to introduce a problem in our dataset which we'll
# then fix. Run the code below - what is it doing?

#messing up the data

census <- census %>% 
  mutate(councilarea = ifelse(agep0 > 8, recode(councilarea,
                                                `Clackmannanshire` = "Clackmananshire"), councilarea),
         councilarea = ifelse(agep0 < 7 & councilarea == "Aberdeen City", 
                              str_to_lower(councilarea), councilarea))

# looking at our factor levels
census %>% 
  group_by(councilarea) %>% 
  count()

# uh oh! There are a few things we might want to change here
# first, Aberdeen City has been listed as aberdeen city a bunch of times
# we should change this so that it's in title case. The
# stringr package has a convenient function for this: str_to_title
# because we're changing a variable we put this in a call to mutate
# and just change all the strings to title case

census <- census %>% 
  mutate(councilarea = str_to_title(councilarea))

# if we wanted we could change all the strings to lower case with
# str_to_lower

census %>% 
  mutate(councilarea = str_to_lower(councilarea))

# or to uppercase with str_to_upper
census %>% 
  mutate(councilarea = str_to_upper(councilarea))

# you may also have spotted another mistake - an inconsistent spelling of 
# Clackmannanshire. We can change this using the recode() function with mutate
# Again recode() takes three arguments: the vector we want to recode, the old value
# and then the new value (don't get these mixed up)
census <- census %>% 
  mutate(councilarea = recode(councilarea,
                              "Clackmananshire" = "Clackmannanshire"))

# separate and unite -----------------------------------------------------------``

# hmmm, the reason_1 variable we created above would be more helpful if it was an integer.
# one way to do this is to separate this vector at the "_" separator. The separate
# function takes the arguments col (column to split), into (new columns) and 
# the character to separate at.
# NB c() lets you combine multiple values into a single list and so pass two
# arguments to the col = option


exclusions %>% 
  separate(col = reason_n, into = c("temp", "reason_n"), sep = "_")

# nice! this has created two variables, one called temp and another one called
# reason_n which just contains the number of reasons.
# we can get rid of the temp variable which we don't want

exclusions %>% 
  separate(col = reason_n, into = c("temp", "reason_n"), sep = "_") %>% 
  select(-temp)

# and then convert reason_n to numeric rather than character with 
# the as.numeric function and save to the the whole thing to the object "exclusions2"

exclusions2 <- exclusions %>% 
  separate(col = reason_n, into = c("temp", "reason_n"), sep = "_") %>% 
  select(-temp) %>% 
  mutate(reason_n = as.numeric(reason_n))

# looks great!

# proving that there's more than one way to skin a cat, we could also just have
# deleted the reason variable and calculated the reason number ourselves
# by saying that for each person and incident (measured by startdate and finishdate)
# we want to generate the number of rows i.e. the number of reasons:

exclusions2 <- exclusions2 %>% 
  group_by(synid, startdate, finishdate, noprovdays) %>% 
  mutate(reason_n_2 = row_number())


# we can test that these two variables are the same using logical operators
# here all.equal compares all the values of the reason_n and reason_n_2
# vectors (this only gives us one output rather than comparing with == which tests
# every element of these two vectors)
# NB the $ operator lets us select a particular column from a dataframe
all.equal(exclusions2$reason_n, exclusions2$reason_n_2)

# this prints the result of the test to console.

# we can also make this test using summarise

exclusions2 %>% 
  ungroup() %>% 
  summarise(reason_test = all.equal(reason_n, reason_n_2))

# which returns a dataframe containing a variable reason_test with a single value
# This uses the aggregation skills we learned earlier but returns the result
# of the test as a dataframe rather than a summary table. Neat!

# we can drop the second reason exclusions object now using rm()

# BE CAREFUL THOUGH because if you remove an object from your workspace, it's gone

rm(exclusions2)

# arranging by order -----------------------------------------------------------

# it's handy to arrange things by vraiable order as we work with the dataset
# and later on when we visualize. Let's try some examples.

# which is the earliest exlcusion in the dataset?
# we can find out by ordering the startdate (this will print the result to the
# console)

exclusions %>% 
  arrange(startdate)

# how about the latest? we can wrap a call to desc() - short for descending -
# in the call to arrange
exclusions %>% 
  arrange(desc(startdate))


# now find our what the longest exclusion was
exclusions %>% 
  arrange(desc(noprovdays))


# EVEN MORE ADVANCED EXERCISES - unite -----------------------------------------

# We had some practice with separate and unite before, but we're going
# to look at this in some more detail now.

# join the council and council code variables
census %>% 
  unite("council", c("councilarea", "ctydis0"))

# this gives a rather ugly join with the two fields separated by an underscore
# let's change this to be separated with a properly-spaced hyphen

census <- census %>% 
  unite("council", c("councilarea", "ctydis0"), sep = " - ")

# and what if we want to reverse this? we can go the other way with separate()

census <- census %>% 
  separate(council, c("councilarea", "ctydis0"), sep = " - ")

# Aside - the reason that we have to use "" around council when uniting
# but don't when separating is because with unite we're making a new variable
# which doesn't have a name already. We can all an existing variable
# without these quotation marks... except when we're passing two variables
# to the command in a list as in c("councilarea", "ctydis0"). 
# This is a result of the way the function is written, so whether you need "" or 
# not depends on what you're doing ¯\_(ツ)_/¯


# some of the variables have numbers in parentheses before the content, like help0
# take a look at help0 now

# this is pretty annoying, so let's remove these as before, using ") " as our split
# string, and making a new column containing the parenthetical number which we can
# just delete

census %>% 
  separate(help1, c("junk", "help"), sep = ") ")

# UH-OH! This has produced a horrible error!
# Incorrectly nested parentheses in regexp pattern. (U_REGEX_MISMATCHED_PAREN)
# regex uses . ^ $ * + ? { } [ ] \ | ( and ) to modify how the pattern matching
# is performed. This means we can use ) directly as part of a string split and 
# so to resolve this error we have to 'escape' the character using \\.

census %>% 
  separate(help1, c("junk", "help"), sep = "\\) ") %>% 
  select(-junk)

# we can see more about using stringr for regular expression here
# http://www2.stat.duke.edu/~cr173/Sta523_Fa15/regex.html


# advanced topics - working with if and else -----------------------------------

# ifelse statements are handy ways to perform actions when certain conditions are met
# say we want to make a new age variable where everyone born in 1994 is classed
# as young and everyone else is not young.
# we can use the syntax ifelse(condition, action if true, action if false) with
# mutate

census %>% 
  mutate(age_grp = ifelse(slsdobyr == 1994, "young", "not young"))

# the case_when() function allows us to string together different conditions
# here we can designate those born in 1994 as young, those born in 1991 as old
# and everyone else as neither young nor old. Note the tilde (~) assignment here!

census %>% 
  mutate(age_grp = case_when(
    slsdobyr == 1994 ~ "young",
    slsdobyr == 1991 ~ "old",
    TRUE ~ "neither young nor old")) %>%  glimpse

# use ifelse or case_when to construct a variable for whether council area
# is Edinburgh or Glasgow or rest of Scotland


# checking package versions ----------------------------------------------------
sessionInfo()