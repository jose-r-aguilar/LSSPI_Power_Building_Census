# Load necessary libraries
library(tidycensus)
library(dplyr)
library(stringr)
library(purrr)
library(readr)

# Set your Census API key
census_api_key("6f6fae65578dfee3340f638a14e9ff0c8d9c01de")  # Replace with your actual API key

# Define the state and counties
state <- "CA"
counties <- c("San Francisco", "Alameda", "Contra Costa", "Marin", "San Mateo")

# Define the ACS tables for each racial/ethnic group
tables <- c("B25140H", "B25140B", "B25140D", "B25140I")
group_labels <- c("White", "Black", "Asian", "Latino")

# Fetch ACS data for all racial/ethnic groups
housing_data <- map_dfr(tables, function(tbl) {
  get_acs(
    geography = "county",
    state = state,
    county = counties,
    table = tbl,
    survey = "acs1",
    year = 2023
  ) %>%
    mutate(race_ethnicity = case_when(
      tbl == "B25140H" ~ "White",
      tbl == "B25140B" ~ "Black",
      tbl == "B25140D" ~ "Asian",
      tbl == "B25140I" ~ "Latino"
    ))
})

# Aggregate values for total housing burden (owners + renters together)
housing_clean <- housing_data %>%
  mutate(
    burden_category = case_when(
      variable %in% c("B25140H_003", "B25140H_007", "B25140H_011",
                      "B25140B_003", "B25140B_007", "B25140B_011",
                      "B25140D_003", "B25140D_007", "B25140D_011",
                      "B25140I_003", "B25140I_007", "B25140I_011") ~ "Moderate (30-50%)",
      variable %in% c("B25140H_004", "B25140H_008", "B25140H_012",
                      "B25140B_004", "B25140B_008", "B25140B_012",
                      "B25140D_004", "B25140D_008", "B25140D_012",
                      "B25140I_004", "B25140I_008", "B25140I_012") ~ "Severe (≥50%)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(burden_category))  # Remove "Not Calculated" and irrelevant categories

# Aggregate values per county by summing estimates (owners + renters)
housing_summary_by_county <- housing_clean %>%
  group_by(GEOID, NAME, race_ethnicity, burden_category) %>%
  summarize(total_burdened = sum(estimate, na.rm = TRUE), .groups = "drop")

# Calculate total burdened households per race/ethnicity (across moderate & severe)
total_burdened_by_group <- housing_summary_by_county %>%
  group_by(GEOID, NAME, race_ethnicity) %>%
  summarize(total_burdened_group = sum(total_burdened, na.rm = TRUE), .groups = "drop")

# Merge total burdened households with burden categories
housing_summary_by_county <- housing_summary_by_county %>%
  left_join(total_burdened_by_group, by = c("GEOID", "NAME", "race_ethnicity")) %>%
  mutate(percentage = (total_burdened / total_burdened_group) * 100) %>%
  select(GEOID, NAME, race_ethnicity, burden_category, total_burdened, total_burdened_group, percentage)

# Summarized version combining all counties
housing_combined <- housing_summary_by_county %>%
  group_by(race_ethnicity, burden_category) %>%
  summarize(
    total_burdened = sum(total_burdened, na.rm = TRUE),
    total_burdened_group = sum(total_burdened_group, na.rm = TRUE)
  ) %>%
  mutate(percentage = (total_burdened / total_burdened_group) * 100) %>%
  select(race_ethnicity, burden_category, total_burdened, percentage)

# View cleaned data
print("Housing Burden Data by County:")
print(housing_summary_by_county)

print("Combined Housing Burden Data:")
print(housing_combined)

# Save to CSV files
write_csv(housing_summary_by_county, "Housing_Burden_by_County.csv")
write_csv(housing_combined, "Housing_Burden_Combined.csv")
