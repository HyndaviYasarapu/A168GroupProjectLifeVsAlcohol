# ============================================================================
# ALCOHOL CONSUMPTION VS LIFE EXPECTANCY ANALYSIS
# 7COM1079 - Team Research and Development Project
# ============================================================================

# Load required libraries
library(ggplot2)
library(dplyr)

# ============================================================================
# 1. DATA LOADING AND PREPARATION
# ============================================================================

cat("=== LOADING DATASETS ===\n\n")

# Read the TWO dataset files
drinks <- read.csv("C:\\Users\\hynda\\Downloads\\drinks.csv")
life_exp <- read.csv("C:\\Users\\hynda\\Downloads\\lifeexpectancy.csv")

# Display drinks dataset structure
cat("--- DRINKS DATASET ---\n")
cat("Columns:", paste(colnames(drinks), collapse=", "), "\n")
cat("Dimensions:", nrow(drinks), "rows x", ncol(drinks), "columns\n\n")
str(drinks)
summary(drinks)

# Display life expectancy dataset structure
cat("\n--- LIFE EXPECTANCY DATASET ---\n")
cat("Columns:", paste(colnames(life_exp), collapse=", "), "\n")
cat("Dimensions:", nrow(life_exp), "rows x", ncol(life_exp), "columns\n\n")
str(life_exp)
summary(life_exp)

# Check for missing values in both datasets
cat("\n--- MISSING VALUES CHECK ---\n")
cat("Missing values in drinks:", sum(is.na(drinks)), "\n")
cat("Missing values in life expectancy:", sum(is.na(life_exp)), "\n\n")

# ============================================================================
# 2. DATA MERGING
# ============================================================================

cat("=== MERGING DATASETS ===\n\n")

# Merge the two datasets by country
# IMPORTANT: Adjust column names based on your actual data
# Common variations in drinks.csv: "country" or "Country"
# Common variations in life expectancy: "Country", "country", "country_name"

# Check column names to determine merge keys
cat("Drinks column names:", colnames(drinks), "\n")
cat("Life expectancy column names:", colnames(life_exp), "\n\n")

# Merge - adjust the by.x and by.y parameters to match your actual column names
# Example assuming drinks has "country" and life_exp has "CountryDisplay"
data_merged <- merge(drinks, life_exp, by.x = "country", by.y = "CountryDisplay", all = FALSE)

cat("Number of countries after merge:", nrow(data_merged), "\n\n")

# ============================================================================
# 3. DATA CLEANING AND SELECTION
# ============================================================================
colnames(data_merged)
cat("=== DATA CLEANING ===\n\n")

# Remove rows with missing values
data_clean <- na.omit(data_merged)
cat("Number of countries after removing NA:", nrow(data_clean), "\n\n")

# Select and rename relevant columns for analysis
# drinks.csv columns: country, beer_servings, spirit_servings, wine_servings, total_litres_of_pure_alcohol
# lifeexpectency.csv has the life expectancy value in the column called "Numeric"

# Select only the columns we need for analysis
data_clean <- data_merged %>%
  select(country, total_litres_of_pure_alcohol, Numeric) %>%
  rename(Country = country,
         Alcohol_Consumption = total_litres_of_pure_alcohol,
         Life_Expectancy = Numeric)

# Display final cleaned dataset
cat("--- FINAL CLEANED DATASET ---\n")
str(data_clean)
head(data_clean, 50)

# ============================================================================
# 4. DESCRIPTIVE STATISTICS
# ============================================================================

cat("\n\n=== DESCRIPTIVE STATISTICS ===\n\n")

# Alcohol consumption statistics
mean_alcohol <- mean(data_clean$Alcohol_Consumption)
sd_alcohol <- sd(data_clean$Alcohol_Consumption)
median_alcohol <- median(data_clean$Alcohol_Consumption)
min_alcohol <- min(data_clean$Alcohol_Consumption)
max_alcohol <- max(data_clean$Alcohol_Consumption)

cat("--- ALCOHOL CONSUMPTION (litres per capita) ---\n")
cat("Mean:   ", round(mean_alcohol, 2), "\n")
cat("SD:     ", round(sd_alcohol, 2), "\n")
cat("Median: ", round(median_alcohol, 2), "\n")
cat("Min:    ", round(min_alcohol, 2), "\n")
cat("Max:    ", round(max_alcohol, 2), "\n\n")

# Life expectancy statistics
mean_life_exp <- mean(data_clean$Life_Expectancy)
sd_life_exp <- sd(data_clean$Life_Expectancy)
median_life_exp <- median(data_clean$Life_Expectancy)
min_life_exp <- min(data_clean$Life_Expectancy)
max_life_exp <- max(data_clean$Life_Expectancy)

cat("--- LIFE EXPECTANCY (years) ---\n")
cat("Mean:   ", round(mean_life_exp, 2), "\n")
cat("SD:     ", round(sd_life_exp, 2), "\n")
cat("Median: ", round(median_life_exp, 2), "\n")
cat("Min:    ", round(min_life_exp, 2), "\n")
cat("Max:    ", round(max_life_exp, 2), "\n\n")

# ============================================================================
# 5. VISUALIZATIONS (VIEW IN RSTUDIO)
# ============================================================================

cat("=== CREATING VISUALIZATIONS ===\n\n")

# --------------------------------------------------
# Visualization 1: Histogram of Alcohol Consumption
# --------------------------------------------------

hist(data_clean$Alcohol_Consumption,
     main = "Distribution of Alcohol Consumption per Capita",
     xlab = "Alcohol Consumption (litres of pure alcohol per capita)",
     ylab = "Frequency (Number of Countries)",
     col = "steelblue",
     border = "white",
     breaks = 15,
     las = 1)

cat("Displayed: Histogram of Alcohol Consumption\n")

# --------------------------------------------------
# Visualization 2: Histogram of Life Expectancy
# --------------------------------------------------

hist(data_clean$Life_Expectancy,
     main = "Distribution of Life Expectancy at Birth",
     xlab = "Life Expectancy (years)",
     ylab = "Frequency (Number of Countries)",
     col = "coral",
     border = "white",
     breaks = 15,
     las = 1)

cat("Displayed: Histogram of Life Expectancy\n")

# --------------------------------------------------
# Visualization 3: Main Scatter Plot with Regression Line
# --------------------------------------------------

plot(data_clean$Alcohol_Consumption, 
     data_clean$Life_Expectancy,
     main = "Relationship between Alcohol Consumption and Life Expectancy",
     xlab = "Alcohol Consumption per Capita (litres of pure alcohol)",
     ylab = "Life Expectancy at Birth (years)",
     pch = 19,
     col = rgb(0, 0, 0.5, 0.6),  # Dark blue with transparency
     cex = 1.2,
     las = 1)

# Add regression line
abline(lm(Life_Expectancy ~ Alcohol_Consumption, data = data_clean), 
       col = "red", 
       lwd = 2)

# Add grid for better readability
grid(col = "gray", lty = "dotted")

# Add subtitle with sample size
mtext(paste("n =", nrow(data_clean), "countries"), 
      side = 3, 
      line = 0.5, 
      cex = 0.9)

cat("Displayed: Scatter Plot with Regression Line\n")

# --------------------------------------------------
# Visualization 4: Q-Q Plot for Alcohol Consumption
# --------------------------------------------------

qqnorm(data_clean$Alcohol_Consumption,
       main = "Q-Q Plot: Alcohol Consumption",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       pch = 19,
       col = "steelblue")

qqline(data_clean$Alcohol_Consumption, 
       col = "red", 
       lwd = 2)

cat("Displayed: Q-Q Plot for Alcohol Consumption\n")

# --------------------------------------------------
# Visualization 5: Q-Q Plot for Life Expectancy
# --------------------------------------------------

qqnorm(data_clean$Life_Expectancy,
       main = "Q-Q Plot: Life Expectancy",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       pch = 19,
       col = "coral")

qqline(data_clean$Life_Expectancy, 
       col = "red", 
       lwd = 2)

cat("Displayed: Q-Q Plot for Life Expectancy\n\n")
