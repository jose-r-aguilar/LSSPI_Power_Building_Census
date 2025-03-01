# Load necessary libraries
library(tidycensus)
library(dplyr)
library(stringr)
library(readr)  # Required for write_csv

# Set your Census API key
census_api_key("6f6fae65578dfee3340f638a14e9ff0c8d9c01de")  # Replace with your API key

# Define the state and counties
state <- "CA"
counties <- c("San Francisco", "Alameda", "Contra Costa", "Marin", "San Mateo")

# Load the Hispanic or Latino Origin data (B03001)
origin_data <- get_acs(
  geography = "county",
  state = state,
  county = counties,
  table = "B03001",
  survey = "acs1",
  year = 2023
)

# Define origin categories based on the table shell, excluding B03001_008, B03001_016, and B03001_027
origin_clean <- origin_data %>%
  filter(variable %in% c(
    "B03001_003", "B03001_004", "B03001_005", "B03001_006", "B03001_007",
    "B03001_009", "B03001_010", "B03001_011", "B03001_012", "B03001_013",
    "B03001_014", "B03001_015", "B03001_017", "B03001_018", "B03001_019",
    "B03001_020", "B03001_021", "B03001_022", "B03001_023", "B03001_024",
    "B03001_025", "B03001_026", "B03001_028", "B03001_029", "B03001_030", "B03001_031"
  )) %>%
  mutate(
    origin_category = case_when(
      variable == "B03001_003" ~ "Hispanic or Latino",
      variable == "B03001_004" ~ "Mexican",
      variable == "B03001_005" ~ "Puerto Rican",
      variable == "B03001_006" ~ "Cuban",
      variable == "B03001_007" ~ "Dominican (Dominican Republic)",
      variable == "B03001_009" ~ "Costa Rican",
      variable == "B03001_010" ~ "Guatemalan",
      variable == "B03001_011" ~ "Honduran",
      variable == "B03001_012" ~ "Nicaraguan",
      variable == "B03001_013" ~ "Panamanian",
      variable == "B03001_014" ~ "Salvadoran",
      variable == "B03001_015" ~ "Other Central American",
      variable == "B03001_017" ~ "Argentinean",
      variable == "B03001_018" ~ "Bolivian",
      variable == "B03001_019" ~ "Chilean",
      variable == "B03001_020" ~ "Colombian",
      variable == "B03001_021" ~ "Ecuadorian",
      variable == "B03001_022" ~ "Paraguayan",
      variable == "B03001_023" ~ "Peruvian",
      variable == "B03001_024" ~ "Uruguayan",
      variable == "B03001_025" ~ "Venezuelan",
      variable == "B03001_026" ~ "Other South American",
      variable == "B03001_028" ~ "Spaniard",
      variable == "B03001_029" ~ "Spanish",
      variable == "B03001_030" ~ "Spanish American",
      variable == "B03001_031" ~ "All other Hispanic or Latino",
      TRUE ~ NA_character_
    )
  )

# Get the total Hispanic or Latino population for each county
total_latino <- origin_data %>%
  filter(variable == "B03001_003") %>%  # Hispanic or Latino total population
  select(GEOID, NAME, estimate) %>%
  rename(total_latino_population = estimate)

# Merge total Latino population data with the categorized origin data
origin_clean <- origin_clean %>%
  left_join(total_latino, by = c("GEOID", "NAME")) %>%
  mutate(percentage = (estimate / total_latino_population) * 100)

# Summarized version combining all counties
origin_combined <- origin_clean %>%
  group_by(origin_category) %>%
  summarize(
    total = sum(estimate, na.rm = TRUE),
    total_latino_population = sum(total_latino_population, na.rm = TRUE)
  ) %>%
  mutate(percentage = (total / total_latino_population) * 100) %>%
  select(origin_category, total, percentage)

# View the cleaned and summarized data
print("Origin Data by County:")
print(origin_clean)

print("Combined Origin Data:")
print(origin_combined)

# Save to CSV files
write_csv(origin_clean, "B03001_origin_by_county.csv")
write_csv(origin_combined, "B03001_origin_combined.csv")
