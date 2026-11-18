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