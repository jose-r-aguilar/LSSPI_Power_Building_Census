# Load necessary libraries
library(tidycensus)
library(dplyr)
library(stringr)

# Set your Census API key
census_api_key("6f6fae65578dfee3340f638a14e9ff0c8d9c01de")  # Replace with your API key

# Define the state and counties
state <- "CA"
counties <- c("San Francisco", "Alameda", "Contra Costa", "Marin", "San Mateo")

# Load the Household Income data (B19001I)
income_data <- get_acs(
  geography = "county",
  state = state,
  county = counties,
  table = "B19001I",
  survey = "acs1",
  year = 2023
)

# Prepare the data with updated conventions
income_clean <- income_data %>%
  filter(!str_detect(variable, "B19001I_001")) %>%  # Exclude the total population row
  mutate(
    income_category = case_when(
      variable == "B19001I_002" ~ "Less than $10,000",
      variable == "B19001I_003" ~ "$10,000 to $14,999",
      variable == "B19001I_004" ~ "$15,000 to $19,999",
      variable == "B19001I_005" ~ "$20,000 to $24,999",
      variable == "B19001I_006" ~ "$25,000 to $29,999",
      variable == "B19001I_007" ~ "$30,000 to $34,999",
      variable == "B19001I_008" ~ "$35,000 to $39,999",
      variable == "B19001I_009" ~ "$40,000 to $44,999",
      variable == "B19001I_010" ~ "$45,000 to $49,999",
      variable == "B19001I_011" ~ "$50,000 to $59,999",
      variable == "B19001I_012" ~ "$60,000 to $74,999",
      variable == "B19001I_013" ~ "$75,000 to $99,999",
      variable == "B19001I_014" ~ "$100,000 to $124,999",
      variable == "B19001I_015" ~ "$125,000 to $149,999",
      variable == "B19001I_016" ~ "$150,000 to $199,999",
      variable == "B19001I_017" ~ "$200,000 or more",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(income_category) %>%  # Group by income category
  summarize(total = sum(estimate, na.rm = TRUE), .groups = "drop")  # Summarize the total estimates

# View the cleaned and summarized data
print(income_clean)

# Optionally save the cleaned data to a CSV file
#write_csv(income_clean, "B19001I_household_income_summary.csv")
