# Working with Admin Data: Modelling -------------------------------------
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
library(margins)
library(knitr)
library(broom)

# load the data ----------------------------------------------------------------
load("M:/Working with Admin Data Course/synthetic-data/FALSE_DATA_2017_003-census-education.RData")

# selecting variables for analysis ---------------------------------------------

# by now you should have a good idea which models you want to include in your
# analysis. Refresh yourself with your descriptive work from yesterday.

# for a paper you normally need a descriptive 'Table 1'which shows how the variables
# included in your model relate to your outcome variable. We can use kable
# from the knitr package to quickly convert a dataframe to an html table
# eg

linked_df %>% 
  group_by(newten, neet) %>% 
  count() %>% kable

# this is one of the situations we may want data in a wide format
linked_df %>% 
  group_by(newten, neet) %>% 
  count() %>% 
  spread(neet, n) %>% kable

# In descriptive analysis are there any low numbers in cells? If so, output will
# not be released as it won't pass SDC! You may need to aggregate some groupings 

# While this is an okay solution to print to console, this kable setup isn't very satisfying
# though, as you have to do all the data wrangling
# yourself to calculate the summary outputs (although at least you know
# exactly what you've got in your table this way!)

# there are a wealth of other packages in R to make tables, discussion of which
# can be found here - https://community.rstudio.com/t/output-nice-looking-formatted-tables/1084/7

# The best solution for you will depend on the rest of your workflow.
# For example, many of the packages listed above are optimised for html and LaTeX
# which might not be what you're looking for.

# exporting tables to Word -----------------------------------------------------

# the easiest way to export a table to Word is to render the document in 
# Markdown.

# for an introduction to Markdown in R see here - http://rmarkdown.rstudio.com/articles_intro.html
# Markdown is it's own language which deserves more time than we can devote to it
# here. RMarkdown allows you to write documents in Markdown in R and combine them
# with 'chunks' of R code. As a result you can write your text and R code
# together. Neat! For a crash introduction, open a new markdown document
# from the RStudio File menu and then set output format as Word. Copy the contents
# of this script file into an r 'chunk' - that is, between lines which say
# ```{r }
#
# ```
# (you can just overwrite the code in the ```{r cars} chunk which is in the 
# document by default). Then press Ctrl + Shift + K to knit the .doc. This will
# be saved into your working directory

# modelling --------------------------------------------------------------------

# statistical modelling in R is a vast topic in its own right, and we're only
# going to scratch the surface this afternoon.
# 


# we can fit a logit model with the glm() function
# note that this function has a different structure to the tidyverse
# fucntions we've been working with, with the formula coming first
# and the data second
# fit a logit model with neet as the response and sex, crsh, ademh, newten and
# duration sum as IVs. Remember cases with NAs get dropped - do we need to recode
# any variables? 

# if we do need to recode, is this script file the best place to do it?
# think of the workflow session! 

logit_model <- glm(neet ~ sex0 + crsh0 + ademh0 + newten + sum_duration, data = linked_df,
                   family = binomial(link = "logit"))

# the classic way of looking at the model results is with summary()
summary(logit_model)

plot(logit_model)

# have a think about what the model is showing here - what's the reference category
# of the neet variable?



# for most models we can use functions from broom to make tidy model results
# which exist in a dataframe rather than as a model object.
# why do this? 
# If we have the results in a tidy format we can use the same tools we've learnt
# already to work with (e.g. visualise) model results. This can help when e.g. comparing results from 
# multiple models. The best place to learn about this is in the 
# Model chapters of R for Data Science - http://r4ds.had.co.nz/model-intro.html
# or this video explanation here - https://www.youtube.com/watch?v=cU0-NrUxRw4

# ADVANCED: The modelr() package can help to fit models as part of the
# 'tidy' workflow we've been learning, although its focus is more on resampling
# methods and cross-validation than typical social science statistics.
# You can read more at https://github.com/tidyverse/modelr


# tidy presents coefficient summary for variables in the model
tidy(logit_model)

# augment gives the model coefficients for each person included in the model
augment(logit_model)

# this is helpful for plotting residuals etc.


# glance provides a single row of summary statistics
glance(logit_model)


# comparing nested models ------------------------------------------------------
# we can see if sex0 is an important variable to include in the model by
# comparing the model above to a model with sex removed
# Two things to note - 1. we have to filter out cases that have missing for sex0
# even though this isn't included in the second model, otherwise the model isn't
# being run on the same data (R automatially drops missing cases from the dataset)
# 2. if we filter out cases with missing sex as before and pipe the result into
# the glm() function we have to replace the data = linked_df call with a .
# This tells R that we want to use the results of the previous pipe in the data
# command.

logit_no_sex <- linked_df %>% 
                   filter(!is.na(sex0)) %>% 
                   glm(neet ~ crsh0 + ademh0 + newten + sum_duration, data = .,
                   family = binomial(link = "logit"))

# we can then test the model fit with anova(model1, model2, test = LRT)
anova_res <- anova(logit_model, logit_no_sex, test = "LRT")

# and if we wanted we can convert this result into a dataframe using broom::tidy
tidy(anova_res)

# ADVANCED - margins ()---------------------------------------------------------

# calculating marginal effects is less advanced in R than in Stata.
# However, the margins() command, loaded with library(margins) allows 
# calculation of marginal effects. See this site for an introduction
# https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html
# This is a recent package though so be aware that it may change!

# point the margins command at a model object and it will provide a dataframe
# with the original data and the fitted values

results_df <- margins(logit_model)

# a'la Stata, average marginal effects can be plotted using plot() on the resultant
# dataframe
plot(results_df)




