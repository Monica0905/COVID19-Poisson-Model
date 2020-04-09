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

# Data Source Link -------------------------------------------------------------
src_confirmed_cases <-
  "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"
src_confirmed_deaths <- 
  "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"
src_county_pop_2019_census <- 
  "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_county_population_usafacts.csv"

# Load Data --------------------------------------------------------------------
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

# Create variables to store index of last column (number of columns)
ncol_confirmed_cases <- ncol(confirmed_cases)
ncol_deaths <- ncol(deaths)

# Update Naming Format ---------------------------------------------------------
# Use lower_snack_case style
if (TRUE) {
  # The pipe symbol (%>%) means AND
  confirmed_cases <- as_tibble(confirmed_cases) %>% 
    rename(
      county_fips = countyFIPS,
      county_name = "County Name",
      state_name = State,
      state_fips = stateFIPS
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
      county_fips = countyFIPS,
      county_name = "County Name",
      state_name = State,
      state_fips = stateFIPS
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
      county_fips = countyFIPS,
      county_name = "County Name", 
      state_name = State,
      county_pop = population
    ) %>%
    mutate(
      county_pop_in_thou = county_pop * .001 # County population in thousands
    )
}

# Set Data Parameter -----------------------------------------------------------
# 04082020[temportary]: Remove duplicates --------------------------------------------------
deaths <- deaths[1:(nrow(deaths)-1), ]

# Check if tables have same supplementary data columns
if (FALSE) { # Switch to TRUE to check uniformity
  table(
    confirmed_cases[, 1:4] == deaths[, 1:4]
  ) # Not uniformed
  # County names contain both upper and lower cases
  # Set confirmed_cases as standard, left join other data tables
  diff_rows <- which(
    confirmed_cases$county_name != deaths$county_name
  )
  print(confirmed_cases$county_name[diff_rows])
  print(deaths$county_name[diff_rows])
  
  table(
    confirmed_cases[, 1] == county_pop[, 1]
  ) # Not uniformed
  diff_rows <- which(
    confirmed_cases$county_fips != county_pop$county_fips
  )
  print(confirmed_cases$county_name[diff_rows])
  print(county_pop$county_name[diff_rows])
}

diff_rows <- which(
  confirmed_cases$county_name != deaths$county_name
)

if (diff_rows[1] != 0) {
  # Set county names in `deaths` to title format
  deaths$county_name[diff_rows] <- str_to_title(
    deaths$county_name[diff_rows]
  )
  # Locate rows with different county names
  diff_rows <- which(
    confirmed_cases$county_name != deaths$county_name
  )
  # Get those county fips
  fips_diff_rows <- confirmed_cases$county_fips[diff_rows]
  # Get those rows
  county_name_diff_rows <- confirmed_cases$county_name[diff_rows]

# 04032020: Dona Ana County in New Mexico has unreadable inputs ----------------
#           Broomfield County in Colorado has different name inputs
#           Matthews County in Virginia has different name inputs
  
  # Locate Dona Ana County in New Mexico using str_match()
  locate_dona_ana_county <- 
    which(
      str_match(
        county_name_diff_rows, 
        pattern = "Ana County$"
      ) == "Ana County"
    )
  # Rename Dona Ana County in New Mexico
  confirmed_cases$county_name[
    diff_rows[locate_dona_ana_county]
    ] <- "Dona Ana County"
  deaths$county_name[
    diff_rows[locate_dona_ana_county]
    ] <- "Dona Ana County"
  # Locate Broomfield County in Colorado using str_match
  locate_broomfield_county <- 
    which(
      str_match(
        county_name_diff_rows, 
        pattern = "^Broomfield"
      ) == "Broomfield"
    )
  # Rename Broomfield County in Colorado
  confirmed_cases$county_name[
    diff_rows[locate_broomfield_county]
  ] <- "Broomfield County"
  # Locate Matthews County in Virginia using str_match
  locate_matthews_county <- 
    which(
      str_match(
        county_name_diff_rows, 
        pattern = "^Mat"
      ) == "Mat"
    )
  # Rename Matthews County in Virginia
  deaths$county_name[
    diff_rows[locate_matthews_county]
  ] <- "Matthews County"
}

# Exclude statewide un-allocated cases (county_fips == 0,    county_pop == 0)
# Exclude Wade Hampton Census Area     (county_fips == 2270, county_pop == 0)
# Exclude Grand Princess Cruise Ship   (county_fips == 6000, county_pop == 0)
name_excludable_county <- c(
  "Statewide Unallocated",
  "Wade Hampton Census Area",
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



# 04082020: Update county_name in county_pop -----------------------------------
# Locate rows with different county names
diff_rows_county_pop <- which(
  confirmed_cases$county_name != county_pop$county_name
)
# Rename Dona Ana County in row 252
county_pop$county_name[252] <- 
  "Broomfield County"
# Rename Broomfield County in row 1803
county_pop$county_name[1803] <-
  "Dona Ana County"

# Merge New York City un-allocated to New York County
# IN confirmed_cases
locate_NYC_unallocated <- 
  which(
    confirmed_cases$county_name == "New York City Unallocated"
  )
locate_NY_county <- 
  which(
    confirmed_cases$county_name == "New York County"
  )
confirmed_cases[locate_NY_county, -(1:4)] <- 
  confirmed_cases[locate_NY_county, -(1:4)] + 
  confirmed_cases[locate_NYC_unallocated, -(1:4)]
# Delete NYC un-allocated row
confirmed_cases <- confirmed_cases[-locate_NYC_unallocated, ]

# IN deaths
locate_NYC_unallocated <- 
  which(
    deaths$county_name == "New York City Unallocated"
  )
locate_NY_county <- 
  which(
    deaths$county_name == "New York County"
  )
deaths[locate_NY_county, -(1:4)] <- 
  deaths[locate_NY_county, -(1:4)] + 
  deaths[locate_NYC_unallocated, -(1:4)]
# Delete NYC un-allocated row
deaths <- deaths[-locate_NYC_unallocated, ]

# IN county_pop
locate_NYC_unallocated_in_county_pop <- 
  which(county_pop$county_name == "New York City Unallocated")
county_pop <- county_pop[-locate_NYC_unallocated_in_county_pop, ]

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
    new_day = day - 1 # Tong, 04062020
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

