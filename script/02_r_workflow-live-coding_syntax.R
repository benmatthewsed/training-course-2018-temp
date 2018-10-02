# The Workflow - live coding

# installing packages
# install the package "tidyverse"

install.packages("tidyverse")

# loading packages
library (tidyverse)


# loading data - an R file
load("M:/Working with Admin Data Course/synthetic-data/false_data_2017_003_exclusions_ver02.RData")

# a csv file
school_census <- read_csv("M:/Working with Admin Data Course/synthetic-data/false_data_2017_003_school_census.csv")

# an .xls
census <- readxl::read_excel("M:/Working with Admin Data Course/synthetic-data/false_data_2017_003_census.xlsx", skip = 0, na = "*")


# saving a file
write_csv(syndati, "M:/Working with Admin Data Course/synthetic-data/false_data_2017_003_exclusions_duplicate.csv", na = "NA")

# normally it's best to save files as .csv - small size, most simple format, human-
# readable, almost no chance of obsolesence, works with any program
