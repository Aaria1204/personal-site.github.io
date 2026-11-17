# 01_clean.R
# Basic cleaning for Zillow data

library(tidyverse)
library(here)

# ---- Folders ----
raw_dir   <- dirname(list.files(here(), pattern = "^County_zori",
                                recursive = TRUE, full.names = TRUE)[1])
clean_dir <- here("clean data")
dir.create(clean_dir, showWarnings = FALSE)

# ---- Read a file, reshape months into rows, keep 2021 onward ----
read_zillow <- function(pattern) {
  file <- list.files(raw_dir, pattern = pattern, full.names = TRUE)[1]
  read_csv(file, show_col_types = FALSE) %>%
    pivot_longer(matches("^\\d{4}-"), names_to = "date", values_to = "value") %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= as.Date("2021-01-01"), !is.na(value))
}

# ---- Regions to keep ----
nyc_counties <- c("New York County", "Kings County", "Queens County",
                  "Bronx County", "Richmond County")

metros <- c("New York, NY", "Boston, MA", "Chicago, IL",
            "Philadelphia, PA", "Los Angeles, CA", "United States")

# ---- County rents ----
county_zori <- read_zillow("^County_zori") %>%
  filter(
    (State == "NY" & RegionName %in% c(nyc_counties, "Nassau County", "Westchester County")) |
      (State == "MA" & RegionName == "Suffolk County") |
      (State == "IL" & RegionName == "Cook County") |
      (State == "PA" & RegionName == "Philadelphia County") |
      (State == "CA" & RegionName == "Los Angeles County")
  )

# ---- Zip rents (NYC only) ----
zip_zori <- read_zillow("^Zip_zori") %>%
  filter(State == "NY", CountyName %in% nyc_counties)

# ---- Metro files ----
new_listings  <- read_zillow("^Metro_new_listings")      %>% filter(RegionName %in% metros)
renter_income <- read_zillow("^Metro_new_renter_income") %>% filter(RegionName %in% metros)
zordi         <- read_zillow("^Metro_zordi")             %>% filter(RegionName %in% metros)

# ---- Quick check: rows and regions in each file ----
list(county_zori = county_zori, zip_zori = zip_zori, new_listings = new_listings,
     renter_income = renter_income, zordi = zordi) %>%
  map(~ c(rows = nrow(.x), regions = n_distinct(.x$RegionName))) %>%
  print()

# ---- Save ----
write_csv(county_zori,   file.path(clean_dir, "county_zori_clean.csv"))
write_csv(zip_zori,      file.path(clean_dir, "zip_zori_clean.csv"))
write_csv(new_listings,  file.path(clean_dir, "new_listings_clean.csv"))
write_csv(renter_income, file.path(clean_dir, "renter_income_clean.csv"))
write_csv(zordi,         file.path(clean_dir, "zordi_clean.csv"))
