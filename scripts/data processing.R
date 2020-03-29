# COVID-19 Generalized Linear Model
# Data Processing

# Setup -----
if(!requireNamespace("dplyr"))
  install.packages("dplyr", repos = "https://cloud.r-project.org")
if(!requireNamespace("data.table"))
  install.packages("data.table", repos = "https://cloud.r-project.org")
if(!requireNamespace("here"))
  install.packages("here", repos = "https://cloud.r-project.org")
require(data.table)
require(dplyr)
library(here)

# ------------------------------------------------------------------------------
# Data Name: covid_confirmed_usafacts.csv
# Path: data/raw/case data/usa-facts/03-27-2020/

# Load -----
here::here()
dat <- fread("data/raw/case data/usa-facts/03-27-2020/covid_confirmed_usafacts_03272020.csv")

# Formatted in a cumulative trend 
head(dat, 5)
glimpse(dat)

# Drop V69 column (3/26/2020)
dat <- dat[, -69]

