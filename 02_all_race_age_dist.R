# Load necessary libraries
library(tidycensus)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Set your Census API key
census_api_key("6f6fae65578dfee3340f638a14e9ff0c8d9c01de")  # Replace with your API key

# Define the state and counties
state <- "CA"
counties <- c("San Francisco", "Alameda", "Contra Costa", "Marin", "San Mateo")

# Define tables for Black, White, and Asian populations
racial_tables <- list(
  "Black" = "B01001B",
  "White" = "B01001A",
  "Asian" = "B01001D",
  "Latino" = "B01001I"
)

# Function to load and clean data for a given race
load_clean_data <- function(race, table) {
  get_acs(
    geography = "county",
    state = state,
    county = counties,
    table = table,
    survey = "acs1",
    year = 2023
  ) %>%
    filter(!str_detect(variable, paste0(table, "_001"))) %>%  # Exclude total population row
    mutate(
      gender = case_when(
        str_detect(variable, "^B01001[A-Z]_0(0[3-9]|1[0-6])$") ~ "Male",
        str_detect(variable, "^B01001[A-Z]_0(1[8-9]|2[0-9]|3[0-1])$") ~ "Female",
        TRUE ~ "Other"  # Fallback
      ),
      age_group = case_when(
        variable %in% c(paste0(table, "_003"), paste0(table, "_018")) ~ "Under 5 years",
        variable %in% c(paste0(table, "_004"), paste0(table, "_019")) ~ "5 to 9 years",
        variable %in% c(paste0(table, "_005"), paste0(table, "_020")) ~ "10 to 14 years",
        variable %in% c(paste0(table, "_006"), paste0(table, "_021")) ~ "15 to 17 years",
        variable %in% c(paste0(table, "_007"), paste0(table, "_022")) ~ "18 and 19 years",
        variable %in% c(paste0(table, "_008"), paste0(table, "_023")) ~ "20 to 24 years",
        variable %in% c(paste0(table, "_009"), paste0(table, "_024")) ~ "25 to 29 years",
        variable %in% c(paste0(table, "_010"), paste0(table, "_025")) ~ "30 to 34 years",
        variable %in% c(paste0(table, "_011"), paste0(table, "_026")) ~ "35 to 44 years",
        variable %in% c(paste0(table, "_012"), paste0(table, "_027")) ~ "45 to 54 years",
        variable %in% c(paste0(table, "_013"), paste0(table, "_028")) ~ "55 to 64 years",
        variable %in% c(paste0(table, "_014"), paste0(table, "_029")) ~ "65 to 74 years",
        variable %in% c(paste0(table, "_015"), paste0(table, "_030")) ~ "75 to 84 years",
        variable %in% c(paste0(table, "_016"), paste0(table, "_031")) ~ "85 years and over",
        TRUE ~ "Other"  # Fallback
      ),
      race = race  # Add race as a column
    ) %>%
    group_by(race, gender, age_group) %>%  # Group by race, gender, and age group across all counties
    summarize(total = sum(estimate, na.rm = TRUE), .groups = "drop")  # Summarize the total estimates
}

# Load and clean data for each racial category
all_data <- bind_rows(
  lapply(names(racial_tables), function(race) {
    load_clean_data(race, racial_tables[[race]])
  })
)

# View the combined and summarized data
print(all_data)

# Optionally save the combined data to a CSV file
write_csv(all_data, "B01001_race_age_gender_combined.csv")
