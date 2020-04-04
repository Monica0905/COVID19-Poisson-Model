# COVID-19 Poisson Model Project
# NYU A3SR
# Created On: 04/04/2020

# Convert HTML file to CSV for KFF State Policy Data
# Data Source: KFF
# Website URL: https://www.kff.org/report-section/state-data-and-policy-actions-to-address-coronavirus-sources/

# Dependencies
if(!requireNamespace("stringr"))
  install.packages("stringr", repos = "https://cloud.r-project.org")
if(!requireNamespace("textreadr"))
  install.packages("textreadr", repos = "https://cloud.r-project.org")
if(!requireNamespace("rebus"))
  install.packages("rebus", repos = "https://cloud.r-project.org")
if(!requireNamespace("data.table"))
  install.packages("data.table", repos = "https://cloud.r-project.org")
library(stringr)
require(textreadr)
library(rebus)
library(data.table)

# Load -------------------------------------------------------------------------
state_policy <- read_html(
  "../data/processed/state_policy_KFF.html",
  skip = 6,    # Skip top six info lines
  trim = TRUE, # Remove leading white spaces
  remove.empty = TRUE # Remove empty elements
)

# Extract state name strings ---------------------------------------------------
# State names are all in FULL UPPERCASE format
# Rule: 
# START with UPPERCASE letter and 
# NOT FOLLOWED by lowercase letter
pattern_state_name <- "^[:upper:](?![:lower:])"

# Create a vector to store state names
state_name <- str_subset(
  state_policy, 
  pattern = pattern_state_name
)
n_state_name <- length(state_name)

# Create a vector to store index of state names
index_state_name <- which(
  str_detect(
    state_policy,
    pattern = pattern_state_name
  ) == TRUE
)

# Subset state_policy by state_name and store results to a list ----------------
state_policy_df <- as.data.frame(state_policy)
policy_by_state <- list(0)
N <- length(state_policy)

trim_start <- index_state_name
n_trim_start <- length(trim_start)

trim_end <- index_state_name - 1
n_trim_end <- length(trim_end)
trim_end <- append(
  trim_end[2:n_trim_end],
  value = N)

for (i in 1:n_trim_start) {
  policy_by_state[[i]] = 
    state_policy_df[trim_start[i] : trim_end[i], ]
}

# Extract date and event strings -----------------------------------------------
# START with a digit (month) and 
# followed by a slash (punctuation) and 
# followed by ONE OR MORE digit(s)
pattern_date_event <- "^[:digit:][:punct:][:digit:]+"

# Subset policy_by_state list with data and event string patterns
# Create a list to store index of date and event strings in each state table
index_date_event_by_state <- list(0)
for (i in 1:n_state_name) {
  index_date_event_by_state[[i]] <- which(
    str_detect(
      policy_by_state[[i]], 
      pattern = pattern_date_event
    ) == TRUE
  )
  policy_by_state[[i]] <- 
    as.data.frame(policy_by_state[[i]])[index_date_event_by_state[[i]], ]
}

# Clean up tables in policy_by_state -------------------------------------------
for (i in 1:n_state_name) {
  # Delete white space at the beginning and the end of each string
  policy_by_state[[i]] <- str_trim(
    policy_by_state[[i]],
    side = "both"
  )
  # Delete extra punctuation
  policy_by_state[[i]] <- str_replace_all(
    policy_by_state[[i]],
    pattern = ";", 
    replacement = ""
  )
  policy_by_state[[i]] <- str_replace_all(
    policy_by_state[[i]],
    pattern = ":", 
    replacement = ""
  ) 
  policy_by_state[[i]] <- str_replace_all(
    policy_by_state[[i]],
    pattern = ",", 
    replacement = ""
  )  
}

# Split date and event in each row of strings ----------------------------------
index_date <- "[:digit:]+[:punct:][:digit:]+"
# Empty list to store date matrix
policy_date <- list(0)
# Empty list to store event matrix
policy_event <- list(0)
for (i in 1:n_state_name) {
  # Extract date strings and store them in matrix
  policy_date[[i]] <- str_extract_all(
    policy_by_state[[i]],
    pattern = index_date,
    simplify = TRUE
  )
  # Change column names 
  colnames(policy_date[[i]]) <- paste(
    "date", 
    c(1:ncol(policy_date[[i]])), 
    sep = ""
  )
  
  # Delete date strings in policy_by_state to leave only event strings
  policy_event[[i]] <- str_replace_all(
    policy_by_state[[i]],
    pattern = index_date,
    replacement = ""
  )
  # Trim white space
  policy_event[[i]] <- str_trim(
    policy_event[[i]], 
    side = "both"
  )
  # Convert to matrix
  policy_event[[i]] <- as.matrix(policy_event[[i]])
  # Change column names
  colnames(policy_event[[i]]) <- "event"
}

names(policy_date) <- state_name
names(policy_event) <- state_name

# Combine policy_date and policy_event for final output ------------------------
state_policy_output <- list(0)

for (i in 1:n_state_name) {
  state_policy_output[[i]] <- merge(
    x = policy_date[[i]], 
    y = policy_event[[i]], 
    by = "row.names",
    all = TRUE
  )
  state_policy_output[[i]] <- 
    state_policy_output[[i]][, 2:ncol(state_policy_output[[i]])]
}

# Convert state_policy_output list to a data frame and tidy up -----------------
state_policy_output_df <- rbindlist(state_policy_output, fill = TRUE)
state_policy_output_df <- subset(
  state_policy_output_df, 
  select = c(
    paste("date", c(1:8), sep = ""),
    "event"
  )
)

# Output -----------------------------------------------------------------------
write.csv(state_policy_output_df, 
          file = "../data/processed/state_policy_KFF.csv")
