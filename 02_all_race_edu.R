# Load necessary libraries
library(tidycensus)
library(dplyr)
library(stringr)

# Set your Census API key
census_api_key("6f6fae65578dfee3340f638a14e9ff0c8d9c01de")  # Replace with your API key

# Define the state and counties
state <- "CA"
counties <- c("San Francisco", "Alameda", "Contra Costa", "Marin", "San Mateo")

# Define the race codes and categories
race_codes <- c(
  Hispanic = "C15002I",
  White = "C15002H",
  Black = "C15002B",
  Asian = "C15002D"
)
education_categories <- c("Less than High School Diploma", "High School", "Some College", "Bachelor's Degree")

# Load the Educational Attainment data for all race codes
education_data <- lapply(names(race_codes), function(race) {
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

# Ensure the 'county' field is explicitly extracted and available
education_data <- education_data %>%
  mutate(
    county = str_extract(NAME, paste(counties, collapse = "|"))  # Extract county name from the NAME field
  )

# Clean and calculate percentages for each gender and combined genders
education_clean <- education_data %>%
  filter(!str_detect(variable, "001")) %>%  # Exclude the total population row
  mutate(
    gender = case_when(
      str_detect(variable, "003$|004$|005$|006$") ~ "Male",
      str_detect(variable, "008$|009$|010$|011$") ~ "Female",
      TRUE ~ "All"
    ),
    educational_attainment = case_when(
      variable %in% c("C15002I_003", "C15002H_003", "C15002B_003", "C15002D_003",
                      "C15002I_008", "C15002H_008", "C15002B_008", "C15002D_008") ~ "Less than High School Diploma",
      variable %in% c("C15002I_004", "C15002H_004", "C15002B_004", "C15002D_004",
                      "C15002I_009", "C15002H_009", "C15002B_009", "C15002D_009") ~ "High School",
      variable %in% c("C15002I_005", "C15002H_005", "C15002B_005", "C15002D_005",
                      "C15002I_010", "C15002H_010", "C15002B_010", "C15002D_010") ~ "Some College",
      variable %in% c("C15002I_006", "C15002H_006", "C15002B_006", "C15002D_006",
                      "C15002I_011", "C15002H_011", "C15002B_011", "C15002D_011") ~ "Bachelor's Degree",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(gender), !is.na(educational_attainment)) %>%
  group_by(race, county, gender, educational_attainment) %>%
  summarize(total = sum(estimate, na.rm = TRUE), .groups = "drop")

# Add combined gender totals
education_clean_combined <- education_clean %>%
  group_by(race, county, educational_attainment) %>%
  summarize(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
  mutate(gender = "All") %>%
  bind_rows(education_clean)

# Calculate percentages at the county level by race, gender, and educational attainment
county_gender_percentages <- education_clean_combined %>%
  group_by(race, county, gender) %>%
  mutate(percentage = (total / sum(total)) * 100) %>%
  ungroup()

# Calculate combined five-county table percentages by race, gender, and educational attainment
five_county_gender_summary <- education_clean_combined %>%
  group_by(race, gender, educational_attainment) %>%
  summarize(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
  group_by(race, gender) %>%
  mutate(percentage = (total / sum(total)) * 100) %>%
  ungroup()

# View results
print("County-level percentages by gender (including combined):")
print(county_gender_percentages)

print("Five-county combined summary by gender (including combined):")
print(five_county_gender_summary)

# Optionally save results to CSV
write.csv(county_gender_percentages, "county_gender_combined_percentages.csv", row.names = FALSE)
write.csv(five_county_gender_summary, "five_county_gender_combined_summary.csv", row.names = FALSE)
