# COVID-19 Poisson Model Project
# NYU A3SR
# Created On:  04/03/2020
# Modified On: 04/06/2020 ------------------------------------------------------

# Auto Data Download and Cleaning
# Data Source: USA FACTS
# Website URL: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/

# Description ------------------------------------------------------------------
# 
# Automatically downloads data files from USA FACTS and generates a cleaned data 
# file in long format. 
# 
# Notes ------------------------------------------------------------------------
# 1. This script is heavily commented to clarify the logic flow. Comments in 
# English and Chinese are both welcome.
# 2. The margin of this script is set to 80.
# 3. After updating the code, please leave a comment with name and date next to 
# changes. 
# e.g.: # Tong_04032020
title: "data-cleaning-us-census"
subtitle: "The COVID-19 Possion Model Project"
output: 
  html_document:
    df_print: paged
    theme: paper
    highlight: kate
    toc: true
    toc_float: 
      collapsed: false
      smooth_scroll: false
    toc_depth: 3
    number_sections: false
    code_folding: hide
---

# Dependencies

```{r setup}
require(readxl)
require(readr)
require(dplyr)
require(reshape2)
```

# Metropolitan data

```{r metro}
# Read metropolitan data
metro_raw <- read_xls("../data/raw/census data/metropolitan/list1_Sep_2018.xls", skip = 2)

# Get the FIPS codes for all the counties from the case data
county_fips <- read_csv("../data/processed/usa-facts/03-29-2020/daily_new_cases_03292020.csv") %>%
  select(county_fips) %>%
  filter(county_fips != 0)

# Clean the metro data
metro <- metro_raw %>%
  filter(!is.na(`FIPS State Code`), !is.na(`FIPS County Code`)) %>%
  mutate(county_fips = as.numeric(paste0(`FIPS State Code`, `FIPS County Code`)),
         metro = as.numeric(`Metropolitan/Micropolitan Statistical Area`=="Metropolitan Statistical Area"),
         micro = as.numeric(`Metropolitan/Micropolitan Statistical Area`=="Micropolitan Statistical Area"),
         central = as.numeric(`Central/Outlying County`=="Central"),
         outlying = as.numeric(`Central/Outlying County`=="Outlying")) %>%
  select(county_fips, metro, micro, central, outlying)

# Merge with the county FIPS
# Those counties not in the above metro data are coded as 0 in the "metro", "micro", "central", and "outlying" variables
metro <- left_join(county_fips, metro) %>%
  mutate(metro = ifelse(is.na(metro), 0, metro),
         micro = ifelse(is.na(micro), 0, micro),
         central = ifelse(is.na(central), 0, central),
         outlying = ifelse(is.na(outlying), 0, outlying))

# Save data
write.csv(metro, file = "../data/processed/census data/metropolitan.csv", row.names = F)
```


# Population and population structure

```{r}
# Read data
pop_raw <- read_csv("../data/raw/census data/population/cc-est2018-alldata.csv")

# Add the 5-digit county fips code
pop_raw <- pop_raw %>%
  mutate(county_fips = as.numeric(paste0(STATE, COUNTY)))

# Rename the variables to be lower cases
colnames(pop_raw) <- tolower(colnames(pop_raw))

# Population of all ages
pop_allage <- pop_raw %>%
  # Keep the latest year and the total age group
  # YEAR 11 = 7/1/2018 population estimate
  # AGEGRP 0 = Total
  filter(year == 11 & agegrp == 0)

# Population of different age groups
pop_eachage <- pop_raw %>%
  filter(year == 11 & agegrp != 0) %>%
  select(county_fips, agegrp, tot_pop) %>%
  dcast(formula = county_fips ~ agegrp, value.var = c('tot_pop'))

colnames(pop_eachage)[-1] <- c("pop_age_0_4", "pop_age_5_9", "pop_age_10_14", "pop_age_15_19",
                           "pop_age_20_24", "pop_age_25_29", "pop_age_30_34", "pop_age_35_39",
                           "pop_age_40_44", "pop_age_45_49", "pop_age_50_54", "pop_age_55_59",
                           "pop_age_60_64", "pop_age_65_69", "pop_age_70_74", "pop_age_75_79",
                           "pop_age_80_84", "pop_age_85")

# Add a variable of elderly population (older than 60)
pop_eachage$pop_elder <- rowSums(pop_eachage[, c("pop_age_60_64", "pop_age_65_69", "pop_age_70_74", "pop_age_75_79", "pop_age_80_84", "pop_age_85")])

# Combine all the population data
pop <- full_join(pop_allage, pop_eachage, by = "county_fips") %>%
  # Compute the percentage of elderly population (in %)
  mutate(pct_pop_elder = pop_elder / tot_pop * 100) %>%
  select(county_fips, tot_pop, pop_elder, pct_pop_elder,
         tot_male:hnac_female, pop_age_0_4:pop_age_85)
```



# Land area and population density

```{r}
# Land area
# LND110210D - Land area in square miles 2010

area_raw <- read_xls("../data/raw/census data/population/land area/LND01.xls")

area <- area_raw %>%
  filter(substr(STCOU, 3, 5) != "000") %>%
  mutate(county_fips = as.numeric(STCOU)) %>%
  select(county_fips, land_area = LND110210D)


# Population 2010 
# Year 1 = 4/1/2010 Census population

pop2010 <- pop_raw %>%
  filter(year == 1 & agegrp == 0) %>%
  select(county_fips, tot_pop)


# Compute density (population per square miles)

pop_dens <- full_join(area, pop2010, by = "county_fips") %>%
  mutate(pop_dens = tot_pop / land_area) %>%
  select(county_fips, pop_dens)

# Add the density data to the population dataset
pop <- full_join(pop, pop_dens, by = "county_fips") %>%
  select(county_fips, tot_pop, pop_dens, pop_elder:pop_age_85)

# Save the data
write.csv(pop, file = "../data/processed/census data/population.csv", row.names = F)
```



# Socioeconomic (education, employment, income, poverty)

```{r}
# Education

educ_raw <- read_xls("../data/raw/census data/education & economic/Education.xls", skip = 4)

educ <- educ_raw %>%
  mutate(county_fips = as.numeric(`FIPS Code`)) %>%
  select(county_fips, 
         pop_hs_less = `Less than a high school diploma, 2014-18`,
         pop_hs = `High school diploma only, 2014-18`,
         pop_college = `Some college or associate's degree, 2014-18`,
         pop_bs_more = `Bachelor's degree or higher, 2014-18`,
         pct_pop_hs_less = `Percent of adults with less than a high school diploma, 2014-18`,
         pct_pop_hs = `Percent of adults with a high school diploma only, 2014-18`,
         pct_pop_college = `Percent of adults completing some college or associate's degree, 2014-18`,
         pct_pop_bs_more = `Percent of adults with a bachelor's degree or higher, 2014-18`)


# Poverty and income

poverty_raw <- read_xls("../data/raw/census data/education & economic/PovertyEstimates.xls", skip = 4)

poverty <- poverty_raw %>%
  mutate(county_fips = as.numeric(FIPStxt)) %>%
  select(county_fips,
         pop_poverty = POVALL_2018,
         pct_pop_poverty = PCTPOVALL_2018,
         med_house_inc = MEDHHINC_2018)


# Employment

employ_raw <- read_xls("../data/raw/census data/education & economic/Unemployment.xls", skip = 4)

employ <- employ_raw %>%
  mutate(county_fips = as.numeric(FIPS)) %>%
  select(county_fips,
         unemploy_rate = Unemployment_rate_2018,
         pop_labor_force =  Civilian_labor_force_2018,
         pop_employed = Employed_2018,
         pop_unemployed = Unemployed_2018)
         
         
# Merge the three socioeconomic datasets together 
ses <- full_join(educ, poverty, by = "county_fips") %>%
  full_join(employ, by = "county_fips")


#Save the data
write.csv(ses, file = "../data/processed/census data/socioeconomics.csv", row.names = F)
```


