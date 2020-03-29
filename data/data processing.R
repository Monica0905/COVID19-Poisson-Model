# COVID-19 Generalized Linear Model
# Data Processing

# Setup -----
library(data.table)
library(dplyr)

# ------------------------------------------------------------------------------
# Data Name: covid_confirmed_usafacts.csv
# Path: data/raw/usa-facts/03-25-2020/

# Load -----
dat <- fread("data/raw/usa-facts/03-25-2020/covid_confirmed_usafacts.csv")

# Formatted in a cumulative trend 
head(dat, 5)
glimpse(dat)

# Drop V69 column (3/26/2020)
dat <- dat[, -69]

dat <- dat %>%
  mutate(total_confirmed = sum(3/25/2020)

