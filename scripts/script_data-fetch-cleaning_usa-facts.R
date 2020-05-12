# COVID-19 Poisson Model Project
# NYU A3SR
# Created On:  04/03/2020
# Modified On: 05/11/2020 ------------------------------------------------------

# Auto Data Download and Cleaning
# Data Source: USA FACTS
# Website URL: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/

# Description ------------------------------------------------------------------
# 
# Automatically downloads data files from USA FACTS and generates a cleaned data 
# file in long format. 
# 
# Notes ------------------------------------------------------------------------
# Comments in English and Chinese are both welcome.
# The margin of this script is set to 80.

# Dependencies -----------------------------------------------------------------
if(!requireNamespace("dplyr"))
  install.packages("dplyr", repos = "https://cloud.r-project.org")
if(!requireNamespace("data.table"))
  install.packages("data.table", repos = "https://cloud.r-project.org")
if(!requireNamespace("RCurl"))
  install.packages("RCurl", repos = "https://cloud.r-project.org")
if(!requireNamespace("stringr"))
  install.packages("stringr", repos = "https://cloud.r-project.org")
if(!requireNamespace("reshape2"))
  install.packages("reshape2", repos = "https://cloud.r-project.org")
require(data.table)
require(dplyr)
require(RCurl)
require(stringr)
require(reshape2)

# Load Data --------------------------------------------------------------------
# Data Source Link 
src_confirmed_cases <-
  "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"
src_confirmed_deaths <- 
  "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"
src_county_pop_2019_census <- 
  "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_county_population_usafacts.csv"

# If data source links are active, load data
# If not, stop
if (
  url.exists(src_confirmed_cases) &
  url.exists(src_confirmed_deaths) & 
  url.exists(src_county_pop_2019_census)
) {
  print("Downloading data...")
  confirmed_cases <- fread(src_confirmed_cases)
  deaths <- fread(src_confirmed_deaths)
  county_pop <- fread(src_county_pop_2019_census)
} else {
  stop("Check data source links")
}

# Create variables to store index of the last column (number of columns)
ncol_confirmed_cases <- ncol(confirmed_cases)
ncol_deaths <- ncol(deaths)

# Update Naming Format ---------------------------------------------------------
# Use lower_snack_case style

# Check if the first four column names are the same (valid)
column_name_valid <- ifelse(
  unique(colnames(confirmed_cases)[1:4] == colnames(deaths)[1:4]) == TRUE,
  yes = TRUE,
  no = FALSE
)
# If valid, continue; otherwise, stop
if (column_name_valid == TRUE) {
  confirmed_cases <- as_tibble(confirmed_cases) %>% 
    rename(
      county_fips = colnames(confirmed_cases)[1],
      county_name = colnames(confirmed_cases)[2],
      state_name  = colnames(confirmed_cases)[3],
      state_fips  = colnames(confirmed_cases)[4]
    ) %>%
    # Reorder variables
    subset( 
      select = c(1, # county_fips
                 4, # state_fips
                 2, # county_name
                 3, # state_name
                 5:ncol_confirmed_cases # date columns
                 )
    )
  deaths <- as_tibble(deaths) %>%
    rename(
      county_fips = colnames(deaths)[1],
      county_name = colnames(deaths)[2],
      state_name  = colnames(deaths)[3],
      state_fips  = colnames(deaths)[4]
    ) %>%
    subset(
      select = c(1, # county_fips
                 4, # state_fips
                 2, # county_name
                 3, # state_name
                 5:ncol_deaths # date columns
                 )
    )
  county_pop <- as_tibble(county_pop) %>%
    rename(
      county_fips = colnames(county_pop)[1],
      county_name = colnames(county_pop)[2], 
      state_name  = colnames(county_pop)[3],
      county_pop  = colnames(county_pop)[4]
    ) %>%
    mutate(
      county_pop_in_thou = county_pop * .001 # County population in thousands
    )
  print("Column names validated and updated.")
} else {
  stop("Column names do not match! (check line 66)")
}

# Set Data Parameter -----------------------------------------------------------
# Update: 05112020
# Check if tables have same county fips codes and names
all_fips_valid <- all(
  unique(confirmed_cases[, 1:2] == deaths[, 1:2])
) == TRUE
county_name_valid <- all(
  unique(confirmed_cases[, 3:4] == deaths[, 3:4])
) == TRUE

if (all_fips_valid == TRUE) {
  print("All fips values validated.")
  if (county_name_valid == TRUE) {
    print("County names validated.")
  } else {
    print("County names do not match. Auto-matching started...")
    # Determine which county row has different names
    diff_rows <- which(
      confirmed_cases$county_name != deaths$county_name
    )
    # Assign different county names in confirmed_cases to deaths
    deaths$county_name[diff_rows] <- confirmed_cases$county_name[diff_rows]
    print("Complete! County names matched.")
  }
} else {
  diff_county_fips <- which(
    confirmed_cases$county_fips != deaths$county_fips
  )
  diff_state_fips <- which(
    confirmed_cases$state_fips != deaths$state_fips
  )
  stop("Check fips difference!")
}

# Exclude non-county cases -----------------------------------------------------
# Determine which row does not contain "County, city, City, Area, Borough, or
# Parish".
non_county_rows <- which(
  str_detect(confirmed_cases$county_name, pattern ="(County|city|City|Area|Borough|Parish)") == FALSE
)
print(confirmed_cases$county_name[non_county_rows])
name_excludable_county <- c(
  "Statewide Unallocated",
  "Grand Princess Cruise Ship"
)
confirmed_cases <- confirmed_cases %>%
  filter(
    !county_name %in% name_excludable_county
  )
deaths <- deaths %>%
  filter(
    !county_name %in% name_excludable_county
  )
county_pop <- county_pop %>%
  filter(
    !county_name %in% name_excludable_county
  )

# Join Data --------------------------------------------------------------------
# Use confirmed_cases as main data table
confirmed_cases_county_pop <- confirmed_cases %>%
  left_join(county_pop) %>%
  select( # Reorder columns
    county_fips, 
    state_fips,
    county_name,
    state_name,
    county_pop,
    county_pop_in_thou,
    everything()
  )

# Calculate State Population using county_pop Data File ------------------------
fips_state <- data.frame(
  state_name = sort(unique(confirmed_cases$state_name)),
  state_fips = sort(unique(confirmed_cases$state_fips))
)
state_pop <- county_pop %>%
  group_by(state_name) %>%
  summarize(
    state_pop = sum(county_pop)
  ) %>% 
  mutate(
    state_pop_in_thou = state_pop * .001, 
  )
state_pop <- state_pop %>%
left_join(fips_state) %>%
select(
    state_fips,
    state_name,
    state_pop,
    state_pop_in_thou
)

# Change from Cumulative to Daily New Cases ------------------------------------
# IN confirmed_cases data table
primary_data <- confirmed_cases_county_pop[, -(1:6)]
supplementary_data <- confirmed_cases_county_pop[, 1:6]

ncol_primary <- ncol(primary_data)
ncol_supplementary <- ncol(supplementary_data)
N <- nrow(primary_data)

# Create primary_one_last_day to store daily cumulative except the most recent one
# To get daily new cases: county_daily_new_temp - primary_one_last_day
county_daily_new_temp <- primary_data
primary_one_last_day <- cbind(
  "initial" = 0, 
  primary_data[, -ncol_primary]
)
print("This will take a while, please do not hit the stop button.")
for (i in 1:N) {
  county_daily_new_temp[i, ] <- primary_data[i, ] - primary_one_last_day[i, ]
}
most_recent_total <- primary_data[, ncol_primary]
colnames(most_recent_total) <- "current_total"
daily_new_cases <- cbind(
  supplementary_data, 
  most_recent_total,
  county_daily_new_temp
)

# Reshape to Long Format ------------------------------------------------------
# Get the first day with cases confirmed (the first non-zero value within each 
# county).
ncol_daily_new_cases <- ncol(daily_new_cases)
daily_new_cases_long <- reshape2::melt(
  daily_new_cases, 
  id.vars = c("county_fips", 
              "county_name",
              "state_name",
              "county_pop",
              "county_pop_in_thou",
              "current_total"
              ),
  measure.vars = colnames(daily_new_cases)[8:ncol_daily_new_cases],
  variable.name = "date",
  value.name = "new_cases"
) %>% 
  arrange(
    county_fips, 
    date
  ) %>%
  group_by(
    county_fips
  ) %>% 
  mutate(
    cum_cases = cumsum(new_cases)
  ) %>%
  filter(
    cum_cases != 0
  ) %>%
  mutate(
    day = row_number(),
    new_day = day - 1 # Create a new_day column that counts from day 0
  ) 

# Save to Data Folder ----------------------------------------------------------
write.csv(
  daily_new_cases, 
  file = "../data/processed/daily_new_cases_usa_facts.csv"
)
write.csv(
  daily_new_cases_long,
  file = "../data/processed/daily_new_cases_long_usa_facts.csv"
)
write.csv(
  state_pop,
  file = "../data/processed/state_population_usa_facts.csv"
)

# Get update results -----------------------------------------------------------
print(
  paste(
    "The data files have been updated to: ",
    colnames(daily_new_cases)[ncol_daily_new_cases],
    sep = ""
  )
)

