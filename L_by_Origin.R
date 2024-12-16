# Load necessary libraries
library(tidycensus)
library(dplyr)
library(stringr)

# Set your Census API key
census_api_key("6f6fae65578dfee3340f638a14e9ff0c8d9c01de")  # Replace with your API key

# Define the state and counties
state <- "CA"
counties <- c("San Francisco", "Alameda", "Contra Costa", "Marin", "San Mateo")

# Load the Hispanic or Latino Origin data (C03001)
origin_data <- get_acs(
  geography = "county",
  state = state,
  county = counties,
  table = "C03001",
  survey = "acs1",
  year = 2023
)

# Prepare the data with updated conventions
origin_clean <- origin_data %>%
  filter(!str_detect(variable, "C03001_001")) %>%  # Exclude the total population row
  mutate(
    origin_category = case_when(
      variable == "C03001_002" ~ "Not Hispanic or Latino",
      variable == "C03001_003" ~ "Hispanic or Latino",
      variable == "C03001_004" ~ "Mexican",
      variable == "C03001_005" ~ "Puerto Rican",
      variable == "C03001_006" ~ "Cuban",
      variable == "C03001_007" ~ "Dominican (Dominican Republic)",
      variable == "C03001_008" ~ "Central American",
      variable == "C03001_009" ~ "South American",
      variable == "C03001_010" ~ "Other Hispanic or Latino",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(origin_category) %>%  # Group by origin category
  summarize(total = sum(estimate, na.rm = TRUE), .groups = "drop")  # Summarize the total estimates

# View the cleaned and summarized data
print(origin_clean)

# Optionally save the cleaned data to a CSV file
#write_csv(origin_clean, "C03001_origin_summary.csv")
