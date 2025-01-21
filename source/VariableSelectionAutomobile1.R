# Data Preperation
column_names <- c("symboling", "normalized_losses", "make", "fuel_type", "aspiration", 
                  "num_of_doors", "body_style", "drive_wheels", "engine_location", 
                  "wheel_base", "length", "width", "height", "curb_weight", 
                  "engine_type", "num_of_cylinders", "engine_size", "fuel_system", 
                  "bore", "stroke", "compression_ratio", "horsepower", "peak_rpm", 
                  "city_mpg", "highway_mpg", "price")

cars <- read.csv("imports-85.data", header = FALSE, sep = ",", col.names = column_names, na.strings = "?")
#Remove observations with missing price values
cars <- cars[!is.na(cars$price), ]

#fit whole model, autometic forward and backward variable selection
library(leaps)
# Ensure 'make' is a factor
cars$make <- as.factor(cars$make)
cars$engine_location <- as.factor(cars$engine_location)
forward_model <- regsubsets(price ~ symboling + normalized_losses + make + fuel_type + aspiration +
                              num_of_doors + body_style + drive_wheels + engine_location +wheel_base +
                              length + width + height + curb_weight + engine_type + num_of_cylinders +
                              engine_size + fuel_system + bore + stroke + compression_ratio + horsepower +
                              peak_rpm + city_mpg + highway_mpg, 
                            data = cars, method = "forward")

summary(forward_model)
backward_model <- regsubsets(price ~ symboling + normalized_losses + make + fuel_type + aspiration +
                               num_of_doors + body_style + drive_wheels + engine_location +wheel_base +
                               length + width + height + curb_weight + engine_type + num_of_cylinders +
                               engine_size + fuel_system + bore + stroke + compression_ratio + horsepower +
                               peak_rpm + city_mpg + highway_mpg, 
                             data = cars, method = "forward")

summary(backward_model)

# Create a mapping for text to numeric values
cylinder_mapping <- c("two" = 2, "three" = 3, "four" = 4, "five" = 5, "six" = 6, "eight" = 8, "twelve" = 12)

# Replace the text values with numeric values using the mapping
cars$num_of_cylinders <- as.numeric(cylinder_mapping[cars$num_of_cylinders])
# Create a mapping for text to numeric values
door_mapping <- c("two" = 2, "four" = 4)

# Replace the text values with numeric values using the mapping
cars$num_of_doors <- as.numeric(door_mapping[cars$num_of_doors])
cor(cars$num_of_cylinders, cars$engine_size, use = "complete.obs")
cor(cars$num_of_doors, cars$width, use = "complete.obs")

#Calculate Correlation Coefficient
# Select only numeric columns from the data set
numeric_cols <- cars[, c("symboling", "normalized_losses","wheel_base", "length", "width", "height", 
                         "curb_weight", "engine_size", "bore", "stroke", "num_of_cylinders",
                         "compression_ratio", "horsepower", "peak_rpm", 
                         "city_mpg", "highway_mpg")]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_cols, use = "complete.obs")

# Print the correlation matrix
print(cor_matrix)
# Set a threshold for high correlation
threshold <- 0.8

# Find variable pairs with high correlation
high_corr_pairs <- which(abs(cor_matrix) > threshold, arr.ind = TRUE)

# Filter out diagonal elements (self-correlation)
high_corr_pairs <- high_corr_pairs[high_corr_pairs[, 1] != high_corr_pairs[, 2], ]

# Print the variable pairs with high correlation
cat("Variable pairs with high correlation (>|", threshold, "|):\n")
for (i in seq_len(nrow(high_corr_pairs))) {
  row_var <- rownames(cor_matrix)[high_corr_pairs[i, 1]]
  col_var <- colnames(cor_matrix)[high_corr_pairs[i, 2]]
  corr_value <- cor_matrix[high_corr_pairs[i, 1], high_corr_pairs[i, 2]]
  cat(row_var, "-", col_var, ":", corr_value, "\n")
}

#Calculate VIFs
library(car)
cars$engine_location <- as.factor(cars$engine_location)
# Fit a linear regression model (all numerical)
model <- lm(price ~ symboling + normalized_losses +
              wheel_base +
              length + width + height + curb_weight + 
              engine_size + bore + stroke + compression_ratio + horsepower +
              peak_rpm + city_mpg + highway_mpg, 
            data = cars)

# Calculate VIF for each predictor
vif_values <- vif(model)
print(vif_values)

# Identify predictors with high VIF
high_vif <- vif_values[vif_values > 5]  # Threshold typically 5 or 10
cat("Predictors with high VIF:\n")
print(high_vif)

#Forward Variable Selection by Hand (Case 1)
# Forward variable selection
cars$make <- as.factor(cars$make)
cars$engine_location <- as.factor(cars$engine_location)

fit.FW_14 <- lm(price~1, data = cars)
add1(fit.FW_14, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_13 <- lm(price~engine_size, data = cars)
add1(fit.FW_13, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_12 <- lm(price~engine_size + make, data = cars)
add1(fit.FW_12, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_11 <- lm(price~engine_size + make + curb_weight, data = cars)
add1(fit.FW_11, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_10 <- lm(price~engine_size + make + curb_weight + engine_location, data = cars)
add1(fit.FW_10, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_9 <- lm(price~engine_size + make + curb_weight + engine_location + width, data = cars)
add1(fit.FW_9, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_8 <- lm(price~engine_size + make + curb_weight + engine_location + width + peak_rpm, data = cars)
add1(fit.FW_8, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_7 <- lm(price~engine_size + make + curb_weight + engine_location + width + peak_rpm + aspiration, data = cars)
add1(fit.FW_7, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_6 <- lm(price~engine_size + make + curb_weight + engine_location + width + peak_rpm + aspiration + num_of_cylinders, data = cars)
add1(fit.FW_6, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_5 <- lm(price~engine_size + make + curb_weight + engine_location + width + peak_rpm + aspiration + num_of_cylinders +engine_type , data = cars)
add1(fit.FW_5, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_4 <- lm(price~engine_size + make + curb_weight + engine_location + width + peak_rpm + aspiration + num_of_cylinders +engine_type + stroke, data = cars)
add1(fit.FW_4, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_5 <- lm(price~engine_size + make + curb_weight + engine_location + width + peak_rpm + aspiration + num_of_cylinders + engine_type + stroke + highway_mpg, data = cars)
add1(fit.FW_5, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_6 <- lm(price~engine_size + make + curb_weight + engine_location + width + peak_rpm + aspiration + num_of_cylinders + engine_type + stroke + highway_mpg + body_style, data = cars)
add1(fit.FW_6, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
# final variable selection model with engine_location, without normalized_losses is fit.FW_6.