# Load necessary libraries
library(tidycensus)
library(dplyr)
library(tidyr)
library(stringr)

# Set Census API key securely
census_api_key("6f6fae65578dfee3340f638a14e9ff0c8d9c01de")  # Replace with your API key


# Define the state and counties
state <- "CA"
counties <- c("San Francisco", "Alameda", "Contra Costa", "Marin", "San Mateo")

# Define tables and corresponding racial/ethnic categories
tables <- c("C23002B", "C23002D", "C23002H", "C23002I")
categories <- c("Black or African American", "Asian", "White (Non-Latino)", "Latino")

# Fetch data for all tables
employment_data <- purrr::map2_df(
  tables, categories,
  ~ get_acs(
    geography = "county",
    state = state,
    county = counties,
    table = .x,
    survey = "acs1",
    year = 2023
  ) %>% mutate(category = .y)
)

# Relevant variables for employment status
employment_vars <- c(
  "C23002B_007", "C23002B_012", "C23002B_008", "C23002B_013", # Black Male
  "C23002B_020", "C23002B_025", "C23002B_021", "C23002B_026",  # Black Female
  "C23002D_007", "C23002D_012", "C23002D_008", "C23002D_013", # Asian Male
  "C23002D_020", "C23002D_025", "C23002D_021", "C23002D_026",  # Asian Female
  "C23002H_007", "C23002H_012", "C23002H_008", "C23002H_013", # White Non-Latino Male
  "C23002H_020", "C23002H_025", "C23002H_021", "C23002H_026",  # White Non-Latino Female
  "C23002I_007", "C23002I_012", "C23002I_008", "C23002I_013", # Latino Male
  "C23002I_020", "C23002I_025", "C23002I_021", "C23002I_026"  # Latino Female
)

# Clean and summarize data
employment_clean <- employment_data %>%
  filter(variable %in% employment_vars) %>%
  mutate(
    # Classify gender
    gender = case_when(
      str_detect(variable, "^(C23002[BDHI]_0(07|08|12|13))$") ~ "Male",
      str_detect(variable, "^(C23002[BDHI]_0(20|21|25|26))$") ~ "Female",
      TRUE ~ "Other"
    ),
    # Classify employment status
    employment_status = case_when(
      str_detect(variable, "^(C23002[BDHI]_0(07|12)|C23002[BDHI]_0(07|12|20|25))$") ~ "Employed",
      str_detect(variable, "^(C23002[BDHI]_0(08|13)|C23002[BDHI]_0(08|13|21|26))$") ~ "Unemployed",
      TRUE ~ "Other"
    ),
    # Classify ethnicity
    ethnicity = case_when(
      category == "Latino" ~ "Latino",
      category %in% c("Black or African American", "Asian", "White (Non-Latino)") ~ "Non-Latino",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(ethnicity, gender, employment_status) %>%
  summarize(total = sum(estimate, na.rm = TRUE), .groups = "drop")

# View the cleaned and summarized data
print(employment_clean)

# Optionally save the cleaned data to a CSV file
# write_csv(employment_clean, "employment_status_summary.csv")
