# Data Cleaning
# Data Source: usa-facts

# Dependencies -----------------------------------------------------------------
if(!requireNamespace("dplyr"))
  install.packages("dplyr", repos = "https://cloud.r-project.org")
if(!requireNamespace("data.table"))
  install.packages("data.table", repos = "https://cloud.r-project.org")
require(data.table)
require(dplyr)

# Load -------------------------------------------------------------------------
dat <- 
  fread("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv")

# Unify variable naming using lower_snack_case
confirmed_cases <- as_tibble(dat) %>%
  rename(
    county_fips = countyFIPS,
    county_name = "County Name",
    state_name = State,
    state_fips = stateFIPS
  ) %>%
  subset(
    select = c(1, 4, 2, 3, 5:72)
  )

# Create a table to describe variables
variable_name <- append(
  x = colnames(confirmed_cases)[1:4],
  values = "date"
)
variable_description <- c(
  "FIPS county code",
  "FIPS state code",
  "County name",
  "State name",
  "Date period 01/22/2020 to 03/29/2020, 68 days, column 5 to 72"
)


primary_data <- confirmed_cases[, -(1:4)]
supplementary_data <- confirmed_cases[, 1:4]
county_daily_new_temp <- primary_data
primary_one_last_day <- cbind("initial" = 0, primary_data[, -ncol(primary_data)])
for (i in 1:N) {
  county_daily_new_temp[i, ] <- primary_data[i, ] - primary_one_last_day[i, ]
}
most_recent_total <- primary_data[, ncol(primary_data)]
colnames(most_recent_total) <- "current_total"
daily_new_cases <- cbind(
  supplementary_data, 
  most_recent_total,
  county_daily_new_temp
)

# Save to data folder
write.csv(
  daily_new_cases, 
  file = "../data/processed/case data/usa-facts/03-29-2020/daily_new_cases_usa-facts.csv"
)