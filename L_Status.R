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

# Prepare the data with updated conventions
citizenship_clean <- citizenship_data %>%
  filter(!str_detect(variable, "B05003I_001")) %>%  # Exclude the total population row
  mutate(
    status = case_when(
      variable %in% c("B05003I_004", "B05003I_009", "B05003I_015", "B05003I_020") ~ "Native",
      variable %in% c("B05003I_005", "B05003I_010", "B05003I_016", "B05003I_021") ~ "Foreign-born",
      variable %in% c("B05003I_006", "B05003I_011", "B05003I_017", "B05003I_022") ~ "Naturalized U.S. citizen",
      variable %in% c("B05003I_007", "B05003I_012", "B05003I_018", "B05003I_023") ~ "Not a U.S. citizen",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(status) %>%  # Group by citizenship status
  summarize(total = sum(estimate, na.rm = TRUE), .groups = "drop")  # Summarize the total estimates

# View the cleaned and summarized data
print(citizenship_clean)

# Optionally save the cleaned data to a CSV file
#write_csv(citizenship_clean, "B05003I_citizenship_status_summary.csv")
