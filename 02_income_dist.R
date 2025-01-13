# Load necessary libraries
library(tidycensus)
library(dplyr)
library(stringr)

# Set your Census API key
census_api_key("6f6fae65578dfee3340f638a14e9ff0c8d9c01de")  # Replace with your API key

# Define the state and counties
state <- "CA"
counties <- c("San Francisco", "Alameda", "Contra Costa", "Marin", "San Mateo")

# Define race codes for ACS table B19001
race_codes <- c(
  Latino = "B19001I",
  Black = "B19001B",
  Asian = "B19001D",
  White = "B19001H"
)

# Define income categories with explicit variable mappings
income_variable_map <- list(
  "Less than $34,999" = c("B19001I_002", "B19001I_003", "B19001I_004", "B19001I_005", "B19001I_006", "B19001I_007",
                          "B19001B_002", "B19001B_003", "B19001B_004", "B19001B_005", "B19001B_006", "B19001B_007",
                          "B19001D_002", "B19001D_003", "B19001D_004", "B19001D_005", "B19001D_006", "B19001D_007",
                          "B19001H_002", "B19001H_003", "B19001H_004", "B19001H_005", "B19001H_006", "B19001H_007"),
  "$35,000 to $74,999" = c("B19001I_008", "B19001I_009", "B19001I_010", "B19001I_011", "B19001I_012",
                           "B19001B_008", "B19001B_009", "B19001B_010", "B19001B_011", "B19001B_012",
                           "B19001D_008", "B19001D_009", "B19001D_010", "B19001D_011", "B19001D_012",
                           "B19001H_008", "B19001H_009", "B19001H_010", "B19001H_011", "B19001H_012"),
  "$75,000 to $124,999" = c("B19001I_013", "B19001I_014",
                            "B19001B_013", "B19001B_014",
                            "B19001D_013", "B19001D_014",
                            "B19001H_013", "B19001H_014"),
  "$125,000 to $199,999" = c("B19001I_015", "B19001I_016",
                             "B19001B_015", "B19001B_016",
                             "B19001D_015", "B19001D_016",
                             "B19001H_015", "B19001H_016"),
  "$200,000 or more" = c("B19001I_017",
                         "B19001B_017",
                         "B19001D_017",
                         "B19001H_017")
)

# Load and process data for each racial group
income_data <- lapply(names(race_codes), function(race) {
  get_acs(
    geography = "county",
    state = state,
    county = counties,
    table = race_codes[[race]],
    survey = "acs1",
    year = 2023
  ) %>%
    mutate(race = race)
}) %>%
  bind_rows()

# Extract and retain county information from the NAME column
income_data <- income_data %>%
  mutate(county = str_extract(NAME, paste(counties, collapse = "|")))

# Clean and categorize data explicitly
income_clean <- income_data %>%
  filter(!str_ends(variable, "_001")) %>%  # Exclude rows where the variable ends with "_001"
  mutate(
    income_category = case_when(
      variable %in% income_variable_map[["Less than $34,999"]] ~ "Less than $34,999",
      variable %in% income_variable_map[["$35,000 to $74,999"]] ~ "$35,000 to $74,999",
      variable %in% income_variable_map[["$75,000 to $124,999"]] ~ "$75,000 to $124,999",
      variable %in% income_variable_map[["$125,000 to $199,999"]] ~ "$125,000 to $199,999",
      variable %in% income_variable_map[["$200,000 or more"]] ~ "$200,000 or more",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(income_category)) %>%  # Filter out rows with NA income_category
  group_by(county, race, income_category) %>%
  summarize(total = sum(estimate, na.rm = TRUE), .groups = "drop")

# Calculate percentages by county
income_percentages <- income_clean %>%
  group_by(county, race) %>%
  mutate(percentage = (total / sum(total)) * 100) %>%
  ungroup()

# Create combined county data
combined_county_data <- income_clean %>%
  group_by(race, income_category) %>%
  summarize(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
  group_by(race) %>%
  mutate(percentage = (total / sum(total)) * 100) %>%
  ungroup() %>%
  mutate(county = "Combined Counties")

# Combine all data
final_income_data <- bind_rows(income_percentages, combined_county_data)

# Save results to CSV
write.csv(final_income_data, "racial_income_percentages.csv", row.names = FALSE)
