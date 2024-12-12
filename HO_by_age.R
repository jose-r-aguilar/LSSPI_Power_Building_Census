# Load necessary libraries
library(tidycensus)
library(dplyr)
library(tidyr)
library(stringr)


# Set your Census API key
census_api_key("6f6fae65578dfee3340f638a14e9ff0c8d9c01de")
#Change with your API key

# Define the state and counties
state <- "CA"
counties <- c("San Francisco", "Alameda", "Contra Costa", "Marin", "San Mateo")

# Load the age distribution for the Hispanic or Latino population (B01001I)

age_data <- get_acs(
  geography = "county",
  state = state,
  county = counties,
  table = "B01001I",
  survey = "acs5",
  year = 2022
)

# Load the Hispanic or Latino origin data (B03001)
hispanic_data <- get_acs(
  geography = "county",
  state = state,
  county = counties,
  table = "B03001",
  survey = "acs5",
  year = 2022
)
# Prepare the data for merging

# Prepare the age and gender data with the updated conventions
age_gender_data <- age_data %>%
  filter(!str_detect(variable, "B01001I_001")) %>%
  mutate(
    gender = case_when(
      str_detect(variable, "^B01001I_0(0[3-9]|1[0-6])$") ~ "Male",
      str_detect(variable, "^B01001I_0(1[8-9]|2[0-9]|3[0-1])$") ~ "Female",
      TRUE ~ "Other"  # Fallback
    ),
    age_group = case_when(
      variable %in% c("B01001I_003", "B01001I_018") ~ "Under 5 years",
      variable %in% c("B01001I_004", "B01001I_019") ~ "5 to 9 years",
      variable %in% c("B01001I_005", "B01001I_020") ~ "10 to 14 years",
      variable %in% c("B01001I_006", "B01001I_021") ~ "15 to 17 years",
      variable %in% c("B01001I_007", "B01001I_022") ~ "18 and 19 years",
      variable %in% c("B01001I_008", "B01001I_023") ~ "20 to 24 years",
      variable %in% c("B01001I_009", "B01001I_024") ~ "25 to 29 years",
      variable %in% c("B01001I_010", "B01001I_025") ~ "30 to 34 years",
      variable %in% c("B01001I_011", "B01001I_026") ~ "35 to 44 years",
      variable %in% c("B01001I_012", "B01001I_027") ~ "45 to 54 years",
      variable %in% c("B01001I_013", "B01001I_028") ~ "55 to 64 years",
      variable %in% c("B01001I_014", "B01001I_029") ~ "65 to 74 years",
      variable %in% c("B01001I_015", "B01001I_030") ~ "75 to 84 years",
      variable %in% c("B01001I_016", "B01001I_031") ~ "85 years and over",
      TRUE ~ "Other"  # Fallback
    )
  ) %>%
  group_by(GEOID, NAME, gender, age_group) %>%
  summarize(total = sum(estimate, na.rm = TRUE))

## merge by 


# Hispanic origin data
hispanic_data <- hispanic_data %>%
  filter(variable %in% c("B03001_003", "B03001_002")) %>%
  mutate(hispanic_origin = ifelse(variable == "B03001_003", "Hispanic or Latino", "Not Hispanic or Latino")) %>%
  select(GEOID, hispanic_origin, estimate) %>%
  pivot_wider(names_from = hispanic_origin, values_from = estimate)

# Join data to create a table showing age distribution by Hispanic origin
age_distribution_table <- age_data %>%
  left_join(hispanic_data, by = "GEOID")

# View result
print(age_distribution_table)
