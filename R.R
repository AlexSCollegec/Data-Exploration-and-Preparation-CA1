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

# ---- 4. New features ------------------------------------------------
df <- df %>% mutate(
  gdp_per_capita       = gdp / population,
  renewable_share_elec = solar_share_elec + wind_share_elec,  # new renewables
  fossil_share_elec    = coal_share_elec + gas_share_elec + oil_share_elec,
  decade               = paste0(floor(year / 10) * 10, "s")
)

# Main electricity source = the source with the biggest share
src_cols <- c("coal_share_elec", "gas_share_elec", "oil_share_elec",
              "nuclear_share_elec", "hydro_share_elec", "renewable_share_elec")
src_names <- c("Coal", "Gas", "Oil", "Nuclear", "Hydro", "Wind+Solar")
df$main_source <- src_names[max.col(as.matrix(df[src_cols]), ties.method = "first")]

# ---- 5. Outliers ----------------------------------------------------
# IQR rule: a value is an outlier if it is below Q1 - 1.5*IQR or
# above Q3 + 1.5*IQR.
count_outliers <- function(x) {
  q <- quantile(x, c(.25, .75), na.rm = TRUE); i <- q[2] - q[1]
  sum(x < q[1] - 1.5 * i | x > q[2] + 1.5 * i, na.rm = TRUE)
}
out_cols <- c("gdp_per_capita", "energy_per_capita", "per_capita_electricity",
              "electricity_generation", "carbon_intensity_elec")
print(sapply(df[out_cols], count_outliers))

# Boxplot before treatment
png("plots/01_outliers_boxplot.png", 1400, 600, res = 130)
print(
  df %>% select(all_of(out_cols)) %>% pivot_longer(everything()) %>%
    ggplot(aes(name, value)) + geom_boxplot(fill = "#8ecae6") +
    facet_wrap(~name, scales = "free", nrow = 1) +
    labs(title = "Outliers before treatment", x = NULL, y = NULL) +
    theme_minimal() + theme(axis.text.x = element_blank(),
                            plot.background = element_rect(fill = "white", colour = NA))
)
dev.off()

# These outliers are REAL (e.g. Iceland and Qatar really use a lot of
# energy, China really makes a lot of electricity), so we do NOT delete
# them. We reduce their effect with a log transform instead.
df <- df %>% mutate(
  log_gdp_pc    = log10(gdp_per_capita),
  log_energy_pc = log10(energy_per_capita + 1),
  log_elec_pc   = log10(per_capita_electricity + 1),
  log_elec_gen  = log10(electricity_generation + 0.01)
)
# Carbon intensity has a few extreme values -> cap at 1st/99th percentile
cap <- quantile(df$carbon_intensity_elec, c(.01, .99))
df$carbon_intensity_elec <- pmin(pmax(df$carbon_intensity_elec, cap[1]), cap[2])

# Remove any duplicate country-year rows
df <- df %>% distinct(country, year, .keep_all = TRUE)
cat("Clean data:", nrow(df), "rows,", ncol(df), "columns\n")
write.csv(df, "energy_clean.csv", row.names = FALSE)

# =====================================================================
# ---- 6. EDA ---------------------------------------------------------
# =====================================================================
theme_set(theme_minimal(base_size = 13) +
            theme(plot.background = element_rect(fill = "white", colour = NA)))

# Q1: How has the electricity mix changed since 2000?
# (average share across countries, each year)
mix <- df %>% group_by(year) %>%
  summarise(Coal = mean(coal_share_elec), Gas = mean(gas_share_elec),
            Oil = mean(oil_share_elec), Nuclear = mean(nuclear_share_elec),
            Hydro = mean(hydro_share_elec),
            `Wind+Solar` = mean(renewable_share_elec)) %>%
  pivot_longer(-year, names_to = "source", values_to = "share")
p1 <- ggplot(mix, aes(year, share, colour = source)) + geom_line(linewidth = 1.2) +
  labs(title = "Q1. Average electricity mix per country, 2000-2022",
       y = "Share of electricity (%)", x = "Year", colour = "Source")
ggsave("plots/02_mix_over_time.png", p1, width = 9, height = 5, dpi = 150)

# Q2: Do richer countries use more electricity? (covariation)
p2 <- ggplot(df %>% filter(year == 2022),
             aes(gdp_per_capita, per_capita_electricity, colour = main_source)) +
  geom_point(size = 2.5, alpha = .8) + scale_x_log10() + scale_y_log10() +
  geom_smooth(aes(group = 1), method = "lm", colour = "black", se = FALSE) +
  labs(title = "Q2. Richer countries use more electricity (2022)",
       x = "GDP per person (log)", y = "Electricity per person, kWh (log)",
       colour = "Main source")
ggsave("plots/03_gdp_vs_electricity.png", p2, width = 9, height = 5.5, dpi = 150)
cat("Q2 correlation (log GDP pc vs log elec pc):",
    round(cor(df$log_gdp_pc, df$log_elec_pc), 2), "\n")

# Q3: Does more coal mean dirtier electricity?
p3 <- ggplot(df, aes(coal_share_elec, carbon_intensity_elec)) +
  geom_point(alpha = .15, colour = "grey30") +
  geom_smooth(method = "lm", colour = "red") +
  labs(title = "Q3. More coal = more CO2 per kWh",
       x = "Coal share of electricity (%)", y = "Carbon intensity (gCO2/kWh)")
ggsave("plots/04_coal_vs_carbon.png", p3, width = 8, height = 5, dpi = 150)
cat("Q3 correlation (coal share vs carbon intensity):",
    round(cor(df$coal_share_elec, df$carbon_intensity_elec), 2), "\n")

# Q4: How does the main source change across decades? (variation)
p4 <- ggplot(df, aes(decade, fill = main_source)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Q4. Main electricity source of country-years, by decade",
       x = "Decade", y = "Share of rows", fill = "Main source")
ggsave("plots/05_main_source_by_decade.png", p4, width = 8, height = 5, dpi = 150)

# Q5: Unusual patterns - very dirty electricity AND high use
unusual <- df %>% filter(year == 2022) %>%
  arrange(desc(carbon_intensity_elec * log_elec_pc)) %>%
  select(country, carbon_intensity_elec, per_capita_electricity, main_source) %>%
  head(8)
print(unusual)

# Distribution (variation) of carbon intensity
p5 <- ggplot(df, aes(carbon_intensity_elec)) +
  geom_histogram(bins = 40, fill = "#219ebc", colour = "white") +
  labs(title = "Distribution of carbon intensity",
       x = "gCO2 per kWh", y = "Count")
ggsave("plots/06_carbon_hist.png", p5, width = 8, height = 4.5, dpi = 150)

# Correlation matrix (covariation of all numeric features)
num_vars <- c("log_gdp_pc", "log_energy_pc", "log_elec_pc", "log_elec_gen",
              "carbon_intensity_elec", "coal_share_elec", "gas_share_elec",
              "oil_share_elec", "nuclear_share_elec", "hydro_share_elec",
              "renewable_share_elec")
png("plots/07_correlation.png", 1100, 1000, res = 140)
corrplot(cor(df[num_vars]), method = "color", type = "upper",
         addCoef.col = "black", number.cex = .6, tl.col = "black", tl.cex = .75)
dev.off()

# =====================================================================
# ---- 7. Encoding ----------------------------------------------------
# =====================================================================
# (a) Label encoding: "energy_level" has a natural order
#     Low < Medium < High  ->  1, 2, 3
df$energy_level <- cut(df$energy_per_capita,
                       breaks = quantile(df$energy_per_capita, c(0, 1/3, 2/3, 1)),
                       labels = c("Low", "Medium", "High"), include.lowest = TRUE)
df$energy_level_code <- as.integer(df$energy_level)
print(head(df[, c("country", "year", "energy_level", "energy_level_code")]))

# (b) One-hot encoding: "main_source" has NO order, so each source
#     gets its own 0/1 column
onehot <- model.matrix(~ main_source - 1, data = df)
colnames(onehot) <- gsub("main_source", "src_", colnames(onehot))
df <- cbind(df, onehot)
print(head(onehot))

# =====================================================================
# ---- 8. PCA ---------------------------------------------------------
# =====================================================================
# Use the numeric features + label code. scale = TRUE so every
# feature has mean 0 and sd 1 (otherwise big numbers dominate).
pca_vars <- c(num_vars, "energy_level_code")
pca <- prcomp(df[pca_vars], center = TRUE, scale. = TRUE)
print(summary(pca))

var_exp <- pca$sdev^2 / sum(pca$sdev^2)
cum_exp <- cumsum(var_exp)
n_keep  <- which(cum_exp >= 0.80)[1]          # PCs needed for 80%
cat("PCs needed for 80% of variance:", n_keep, "of", length(pca_vars), "\n")

# Scree plot
scree <- data.frame(PC = seq_along(var_exp), var = var_exp, cum = cum_exp)
p6 <- ggplot(scree, aes(PC, var)) + geom_col(fill = "#023047") +
  geom_line(aes(y = cum), colour = "orange", linewidth = 1) +
  geom_point(aes(y = cum), colour = "orange") +
  geom_hline(yintercept = .8, linetype = "dashed") +
  scale_x_continuous(breaks = scree$PC) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Scree plot: variance explained by each PC",
       y = "Variance explained", x = "Principal component")
ggsave("plots/08_scree.png", p6, width = 8, height = 4.5, dpi = 150)

# Loadings - which features build PC1, PC2, PC3
load <- as.data.frame(pca$rotation[, 1:3])
print(round(load, 2))

# Biplot-style chart: rows (coloured by main source) + feature arrows
scores <- as.data.frame(pca$x[, 1:2]); scores$main_source <- df$main_source
arrows <- load[, 1:2] * 6
arrows$var <- gsub("_share_elec|log_|_elec", "", rownames(arrows))  # short names
p7 <- ggplot(scores, aes(PC1, PC2)) +
  geom_point(aes(colour = main_source), alpha = .25, size = 1) +
  geom_segment(data = arrows, aes(0, 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(.2, "cm"))) +
  geom_label(data = arrows, aes(PC1 * 1.15, PC2 * 1.15, label = var),
             size = 3, alpha = .8) +
  labs(title = "PCA biplot (PC1 vs PC2)",
       x = sprintf("PC1 (%.0f%%)", var_exp[1] * 100),
       y = sprintf("PC2 (%.0f%%)", var_exp[2] * 100), colour = "Main source")
ggsave("plots/09_pca_biplot.png", p7, width = 9, height = 6.5, dpi = 150)

# Reduced dataset: keep only the first n_keep PCs
reduced <- cbind(df[, c("country", "year")], pca$x[, 1:n_keep])
write.csv(reduced, "energy_pca_reduced.csv", row.names = FALSE)
cat("Reduced data:", nrow(reduced), "rows,", ncol(reduced) - 2, "PCs\n")