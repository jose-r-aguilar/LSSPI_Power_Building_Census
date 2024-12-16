library(tidycensus)
library(dplyr)
library(tidyr)

# Set your Census API key
census_api_key("6f6fae65578dfee3340f638a14e9ff0c8d9c01de")  # Replace with your API key

# Define variables
counties <- c("San Francisco", "Alameda", "Contra Costa", "Marin", "San Mateo")
state <- "CA"
tables <- c("B25003", "B25003B", "B25003D", "B25003H", "B25003I")

# Fetch data for each table and county
fetch_acs_data <- function(table, counties, state) {
  map_dfr(counties, function(county) {
    get_acs(geography = "county",
            table = table,
            state = state,
            county = county,
            year = 2023,
            survey = "acs1") %>%
      mutate(County = county)
  })
}

# Combine data for all tables
data_list <- lapply(tables, fetch_acs_data, counties = counties, state = state)
combined_data <- bind_rows(data_list)

# Process data: Combine races, group by occupancy type, drop total
processed_data <- combined_data %>%
  mutate(
    Occupied_Type = case_when(
      str_detect(variable, "_002") ~ "Owner-Occupied",
      str_detect(variable, "_003") ~ "Renter-Occupied",
      TRUE ~ NA_character_
    ),
    Race_Category = case_when(
      str_detect(variable, "B25003B") ~ "Non-Latino",
      str_detect(variable, "B25003D") ~ "Non-Latino",
      str_detect(variable, "B25003H") ~ "Non-Latino",
      str_detect(variable, "B25003I") ~ "Latino",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Occupied_Type), !is.na(Race_Category)) %>%
  group_by(County, Occupied_Type, Race_Category) %>%
  summarize(
    Estimate = sum(estimate, na.rm = TRUE),
    MOE = sqrt(sum(moe^2, na.rm = TRUE))
  ) %>%
  ungroup()

# Create a separate data frame combining all counties
combined_county_data <- processed_data %>%
  group_by(Occupied_Type, Race_Category) %>%
  summarize(
    Combined_Estimate = sum(Estimate, na.rm = TRUE),
    Combined_MOE = sqrt(sum(MOE^2, na.rm = TRUE))
  ) %>%
  ungroup()

# Output final datasets
processed_data
combined_county_data

# Save to CSV
write.csv(processed_data, "acs1_tenure_separate_counties.csv", row.names = FALSE)
write.csv(combined_county_data, "acs1_tenure_combined_counties.csv", row.names = FALSE)

# Print a preview of the final data
head(processed_data)
head(combined_county_data)
