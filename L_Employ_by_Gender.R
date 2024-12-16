# Load necessary libraries
library(tidycensus)
library(dplyr)
library(tidyr)
library(stringr)

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
  group_by(gender, employment_status) %>%  # Group by gender and employment status across all counties
  summarize(total = sum(estimate, na.rm = TRUE), .groups = "drop")  # Summarize the total estimates

# View the cleaned and summarized data
print(employment_clean)

# Optionally save the cleaned data to a CSV file
#write_csv(employment_clean, "C23002I_employment_status_summary.csv")
