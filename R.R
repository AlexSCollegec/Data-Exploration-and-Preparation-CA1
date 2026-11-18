=====================================================================
  # CA1 - Data Exploration and Preparation
  # Domain : Energy
  # Topic  : How countries make their electricity (2000 - 2022)
  # Data   : Our World in Data - Energy dataset
  #          https://github.com/owid/energy-data
  # =====================================================================

# ---- 0. Packages ----------------------------------------------------
library(dplyr)     # data cleaning
library(tidyr)     # reshape data (wide <-> long)
library(ggplot2)   # charts
library(corrplot)  # correlation chart

dir.create("plots", showWarnings = FALSE)   # folder for saved charts
set.seed(123)                                # same results every run

# ---- 1. Load the raw data -------------------------------------------
raw <- read.csv("owid-energy-data.csv", stringsAsFactors = FALSE)
cat("Raw data:", nrow(raw), "rows,", ncol(raw), "columns\n")

# ---- 2. Select rows and columns -------------------------------------
# Keep only real countries (regions like "World" or "Europe" have no
# ISO code) and years 2000-2022. GDP stops after 2022, and before
# 2000 many small countries are missing, which would make fake jumps
# in the averages.
keep_cols <- c("country", "iso_code", "year", "population", "gdp",
               "energy_per_capita", "per_capita_electricity",
               "electricity_generation", "carbon_intensity_elec",
               "coal_share_elec", "gas_share_elec", "oil_share_elec",
               "nuclear_share_elec", "hydro_share_elec",
               "solar_share_elec", "wind_share_elec")

df <- raw %>%
  filter(iso_code != "", !is.na(iso_code),
         !grepl("^OWID", iso_code),          # remove OWID groups
         year >= 2000, year <= 2022) %>%
  select(all_of(keep_cols))
cat("After filtering:", nrow(df), "rows,", ncol(df), "columns\n")

# ---- 3. Missing values ----------------------------------------------
# Count missing values in each column
na_before <- colSums(is.na(df))
print(na_before)

# Rule 1: drop rows where the electricity mix is unknown
# (we can't guess a country's whole electricity mix)
df <- df %>% filter(!is.na(coal_share_elec), !is.na(per_capita_electricity))

# Rule 2: fill small gaps inside each country with linear
# interpolation (value between the year before and the year after),
# then carry the nearest value for gaps at the start/end.
fill_gaps <- function(x) {
  if (sum(!is.na(x)) < 2) return(x)
  approx(seq_along(x), x, xout = seq_along(x), rule = 2)$y
}
num_fill <- c("population", "gdp", "energy_per_capita",
              "carbon_intensity_elec", "gas_share_elec", "oil_share_elec",
              "nuclear_share_elec", "hydro_share_elec",
              "solar_share_elec", "wind_share_elec")
df <- df %>% arrange(country, year) %>% group_by(country) %>%
  mutate(across(all_of(num_fill), fill_gaps)) %>% ungroup()

# Rule 3: anything still missing (country has no data at all for that
# column) -> fill with the median of that year (all countries)
df <- df %>% group_by(year) %>%
  mutate(across(all_of(num_fill),
                ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))) %>%
  ungroup()

cat("Missing values left:", sum(is.na(df)), "\n")