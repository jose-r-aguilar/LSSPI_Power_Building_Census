# Load necessary libraries
library(tidycensus)
library(dplyr)
library(stringr)

# Set your Census API key
census_api_key("6f6fae65578dfee3340f638a14e9ff0c8d9c01de")  # Replace with your API key

# Define the state and counties
state <- "CA"
counties <- c("San Francisco", "Alameda", "Contra Costa", "Marin", "San Mateo")

# Load the Citizenship Status data (B05003I)
citizenship_data <- get_acs(
  geography = "county",
  state = state,
  county = counties,
  table = "B05003I",
  survey = "acs1",
  year = 2023
)

# Extract and retain the county information
citizenship_data <- citizenship_data %>%
  mutate(county = str_extract(NAME, paste(counties, collapse = "|")))  # Extract county name from the NAME field

# Clean and process data using the correct variable mappings
citizenship_clean <- citizenship_data %>%
  filter(!str_detect(variable, "B05003I_001")) %>%  # Exclude the total population row
  mutate(
    age_category = case_when(
      variable %in% c("B05003I_004", "B05003I_006", "B05003I_007",
                      "B05003I_015", "B05003I_017", "B05003I_018") ~ "Under 18",
      variable %in% c("B05003I_009", "B05003I_011", "B05003I_012",
                      "B05003I_020", "B05003I_022", "B05003I_023") ~ "18 or Older",
      TRUE ~ NA_character_
    ),
    status = case_when(
      variable %in% c("B05003I_004", "B05003I_009", "B05003I_015", "B05003I_020") ~ "U.S. Born",
      variable %in% c("B05003I_006", "B05003I_011", "B05003I_017", "B05003I_022") ~ "Foreign-born, Naturalized U.S. citizen ",
      variable %in% c("B05003I_007", "B05003I_012", "B05003I_018", "B05003I_023") ~ "Foreign-born, Not a U.S. citizen",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_category), !is.na(status)) %>%
  group_by(county, age_category, status) %>%
  summarize(total = sum(estimate, na.rm = TRUE), .groups = "drop")

# Add combined totals for all counties
combined_county_totals <- citizenship_clean %>%
  group_by(age_category, status) %>%
  summarize(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
  mutate(county = "Combined Counties")

# Combine county-level and overall totals
final_citizenship_data <- bind_rows(citizenship_clean, combined_county_totals)

# Calculate percentages for each county and combined counties
final_citizenship_data <- final_citizenship_data %>%
  group_by(county, age_category) %>%
  mutate(percentage = (total / sum(total)) * 100) %>%
  ungroup()

# View results
print("County and combined citizenship percentages by age category and status:")
print(final_citizenship_data)

# Optionally save results to CSV
write.csv(final_citizenship_data, "correct_citizenship_status_percentages_by_age.csv", row.names = FALSE)
