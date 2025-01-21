# stepwise variable selection by hand for case 1
fit.FW_14 <- lm(price~1, data = cars)
add1(fit.FW_14, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_13 <- lm(price~curb_weight, data = cars)
add1(fit.FW_13, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_13 <- lm(price~curb_weight + make + height, data = cars)
drop1(fit.FW_13, test = "F")
fit.FW_12 <- lm(price~curb_weight + make, data = cars)
add1(fit.FW_12, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_12 <- lm(price~curb_weight + make + height, data = cars)
drop1(fit.FW_12, test = "F")
fit.FW_11 <- lm(price~curb_weight + make + height, data = cars)
add1(fit.FW_11, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")

fit.FW_11 <- lm(price~curb_weight + make + height, data = cars)
drop1(fit.FW_11, test = "F")
fit.FW_10 <- lm(price~curb_weight + make + height + aspiration, data = cars)
add1(fit.FW_10, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")

drop1(fit.FW_10, test = "F")
fit.FW_9 <- lm(price~curb_weight + make + height + aspiration + body_style, data = cars)
add1(fit.FW_9, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
drop1(fit.FW_9, test = "F")

fit.FW_8 <- lm(price~curb_weight + make + height + aspiration + body_style + wheel_base, data = cars)
add1(fit.FW_8, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_7 <- lm(price~curb_weight + make + height + aspiration + body_style + wheel_base + num_of_cylinders, data = cars)
add1(fit.FW_7, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")

fit.FW_6 <- lm(price~curb_weight + make + height + aspiration + body_style + wheel_base + num_of_cylinders + drive_wheels, data = cars)
add1(fit.FW_6, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
# Stepwise variable selection get repetition for adding and dropping same variable. Stop here.

# Second round of stepwise variable selection for both cases since we only eliminated variables that are not picked by FS and dropped by BS
fit1 <- lm(price ~ make + aspiration +
             num_of_doors + body_style + drive_wheels + engine_location + wheel_base
           + width + height + engine_type + num_of_cylinders +
             engine_size + bore + stroke + compression_ratio +
             peak_rpm, 
           data = cars)
drop1(fit1, test = "F")

fit2 <- lm(price ~make + aspiration +
             num_of_doors + body_style + drive_wheels + engine_location + wheel_base
           + width + height + engine_type + num_of_cylinders +
             engine_size + bore + stroke +
             peak_rpm, 
           data = cars)
add1 (fit2, price ~ make + aspiration +
        num_of_doors + body_style + drive_wheels + engine_location + wheel_base
      + width + height + engine_type + num_of_cylinders +
        engine_size + bore + stroke + compression_ratio +
        peak_rpm, test = "F")
drop1(fit2, test = "F")

fit3 <- lm(price ~ make + aspiration +
             body_style + drive_wheels + engine_location + wheel_base
           + width + height + engine_type + num_of_cylinders +
             engine_size + bore + stroke +
             peak_rpm, 
           data = cars)
add1 (fit3, price ~ make + aspiration +
        num_of_doors + body_style + drive_wheels + engine_location + wheel_base
      + width + height + engine_type + num_of_cylinders +
        engine_size + bore + stroke + compression_ratio +
        peak_rpm, test = "F")
drop1(fit3, test = "F")

fit4 <- lm(price ~ make + aspiration +
             body_style + drive_wheels + engine_location + wheel_base
           + width + engine_type + num_of_cylinders +
             engine_size + bore + stroke +
             peak_rpm, 
           data = cars)
add1 (fit4, price ~ make + aspiration +
        num_of_doors + body_style + drive_wheels + engine_location + wheel_base
      + width + height + engine_type + num_of_cylinders +
        engine_size + bore + stroke + compression_ratio +
        peak_rpm, test = "F")
drop1(fit4, test = "F")

fit5 <- lm(price ~ make + aspiration +
             body_style + drive_wheels + engine_location
           + width + engine_type + num_of_cylinders +
             engine_size + bore + stroke +
             peak_rpm, 
           data = cars)
add1 (fit5, price ~ make + aspiration +
        num_of_doors + body_style + drive_wheels + engine_location + wheel_base
      + width + height + engine_type + num_of_cylinders +
        engine_size + bore + stroke + compression_ratio +
        peak_rpm, test = "F")
drop1(fit5, test = "F")

fit6 <- lm(price ~ make + aspiration +
             body_style + drive_wheels + engine_location
           + width + engine_type + num_of_cylinders +
             engine_size + stroke +
             peak_rpm, 
           data = cars)
add1 (fit6, price ~ make + aspiration +
        num_of_doors + body_style + drive_wheels + engine_location + wheel_base
      + width + height + engine_type + num_of_cylinders +
        engine_size + bore + stroke + compression_ratio +
        peak_rpm, test = "F")
drop1(fit6, test = "F")

fit7 <- lm(price ~ make + aspiration +
             body_style + engine_location
           + width + engine_type + num_of_cylinders +
             engine_size + stroke +
             peak_rpm, 
           data = cars)
add1 (fit7, price ~ make + aspiration +
        num_of_doors + body_style + drive_wheels + engine_location + wheel_base
      + width + height + engine_type + num_of_cylinders +
        engine_size + bore + stroke + compression_ratio +
        peak_rpm, test = "F")
drop1(fit7, test = "F")







