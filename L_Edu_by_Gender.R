# Load necessary libraries
library(tidycensus)
library(dplyr)
library(stringr)

# Set your Census API key
census_api_key("6f6fae65578dfee3340f638a14e9ff0c8d9c01de")  # Replace with your API key

# Define the state and counties
state <- "CA"
counties <- c("San Francisco", "Alameda", "Contra Costa", "Marin", "San Mateo")

# Load the Educational Attainment data (B15002I)
education_data <- get_acs(
  geography = "county",
  state = state,
  county = counties,
  table = "B15002I",
  survey = "acs1",
  year = 2023
)

# Map variables to descriptive labels
education_clean <- education_data %>%
  filter(!str_detect(variable, "B15002I_001")) %>%  # Exclude the total population row
  mutate(
    gender = case_when(
      str_detect(variable, "^B15002I_0(0[3-9]|10)$") ~ "Male",
      str_detect(variable, "^B15002I_0(1[2-9])$") ~ "Female",
      TRUE ~ "Other"  # Fallback
    ),
    educational_attainment = case_when(
      variable %in% c("B15002I_003", "B15002I_012") ~ "Less than 9th grade",
      variable %in% c("B15002I_004", "B15002I_013") ~ "9th to 12th grade, no diploma",
      variable %in% c("B15002I_005", "B15002I_014") ~ "Regular high school diploma",
      variable %in% c("B15002I_006", "B15002I_015") ~ "GED or alternative credential",
      variable %in% c("B15002I_007", "B15002I_016") ~ "Some college, no degree",
      variable %in% c("B15002I_008", "B15002I_017") ~ "Associate's degree",
      variable %in% c("B15002I_009", "B15002I_018") ~ "Bachelor's degree",
      variable %in% c("B15002I_010", "B15002I_019") ~ "Graduate or professional degree",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(gender, educational_attainment) %>%  # Group by gender and educational attainment
  summarize(total = sum(estimate, na.rm = TRUE), .groups = "drop")  # Summarize the total estimates

# View the cleaned and summarized data
print(education_clean)

# Optionally save the cleaned data to a CSV file
#write_csv(education_clean, "B15002I_educational_attainment_summary.csv")

