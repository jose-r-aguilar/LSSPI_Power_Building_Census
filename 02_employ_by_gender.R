# Load necessary libraries
library(tidycensus)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)  # Required for write_csv

# Set your Census API key
census_api_key("6f6fae65578dfee3340f638a14e9ff0c8d9c01de")  # Replace with your API key

# Define the state and counties
state <- "CA"
counties <- c("San Francisco", "Alameda", "Contra Costa", "Marin", "San Mateo")

# Load the Employment Status data (C23002I)
employment_data <- get_acs(
  geography = "county",
  state = state,
  county = counties,
  table = "C23002I",
  survey = "acs1",
  year = 2023
)

# Prepare the data with updated conventions
employment_clean <- employment_data %>%
  filter(
    variable %in% c(
      # Add variable codes corresponding to "Civilians at all ages - Employed and Unemployed"
      "C23002I_007", "C23002I_012", # Male Employed
      "C23002I_008", "C23002I_013", # Male Unemployed
      "C23002I_020", "C23002I_025", # Female Employed
      "C23002I_021", "C23002I_026"  # Female Unemployed
    )
  ) %>%
  mutate(
    county = str_to_title(NAME),  # Extract and format county name
    gender = case_when(
      str_detect(variable, "^C23002I_0(07|08|12|13)$") ~ "Male",
      str_detect(variable, "^C23002I_0(20|21|25|26)$") ~ "Female",
      TRUE ~ "Other"  # Fallback
    ),
    employment_status = case_when(
      str_detect(variable, "^(C23002I_007|C23002I_012|C23002I_020|C23002I_025)$") ~ "Employed",
      str_detect(variable, "^(C23002I_008|C23002I_013|C23002I_021|C23002I_026)$") ~ "Unemployed",
      TRUE ~ "Other"  # Fallback
    )
  ) %>%
  group_by(county, gender, employment_status) %>%  # Group by county, gender, and employment status
  summarize(total = sum(estimate, na.rm = TRUE), .groups = "drop")  # Summarize the total estimates

# Calculate percentages for each county
employment_percentages_by_county <- employment_clean %>%
  group_by(county) %>%
  mutate(
    percentage = (total / sum(total)) * 100  # Calculate percentage within each county
  ) %>%
  ungroup()

# Combine all counties into one dataset, calculate totals and percentages
employment_combined <- employment_clean %>%
  group_by(gender, employment_status) %>%
  summarize(total = sum(total), .groups = "drop") %>%
  mutate(
    percentage = (total / sum(total)) * 100  # Calculate percentage for combined counties
  )

# Save the data to CSV files
write_csv(employment_percentages_by_county, "C23002I_employment_percentages_by_county_v2.csv")  # Percentages by county
write_csv(employment_combined, "C23002I_employment_combined_v2.csv")  # Combined summary

# View the results
print("Employment Percentages by County:")
print(employment_percentages_by_county)

print("Combined Employment Data:")
print(employment_combined)
