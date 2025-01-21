# Forward variable selection by hand case 2
cars$make <- as.factor(cars$make)
cars$engine_location <- as.factor(cars$engine_location)

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
fit.FW_12 <- lm(price~curb_weight + make, data = cars)
add1(fit.FW_12, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_11 <- lm(price~curb_weight + make + height, data = cars)
add1(fit.FW_11, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_10 <- lm(price~curb_weight + make + height + aspiration, data = cars)
add1(fit.FW_10, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")

fit.FW_9 <- lm(price~curb_weight + make + height + aspiration + body_style, data = cars)
add1(fit.FW_9, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
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

fit.FW_5 <- lm(price~curb_weight + make + height + aspiration + body_style + wheel_base + num_of_cylinders + drive_wheels + compression_ratio , data = cars)
add1(fit.FW_5, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_4 <- lm(price~curb_weight + make + height + aspiration + body_style + wheel_base + num_of_cylinders + drive_wheels + compression_ratio + length, data = cars)
add1(fit.FW_4, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")

fit.FW_3 <- lm(price~curb_weight + make + height + aspiration + body_style + wheel_base + num_of_cylinders + drive_wheels + compression_ratio + length + num_of_doors, data = cars)
add1(fit.FW_3, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.FW_2 <- lm(price~curb_weight + make + height + aspiration + body_style + wheel_base + num_of_cylinders + drive_wheels + compression_ratio + length + num_of_doors + fuel_system, data = cars)
add1(fit.FW_2, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")

fit.FW_1 <- lm(price~curb_weight + make + height + aspiration + body_style + wheel_base + num_of_cylinders + drive_wheels + compression_ratio + length + num_of_doors + fuel_system + engine_size, data = cars)
add1(fit.FW_1, price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + wheel_base + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
# end of forward variable selection by hand for case 2

#backward variable selectin by hand for case 2
fit.00 <- lm(price ~ symboling +normalized_losses + make + fuel_type + aspiration + num_of_doors + 
               body_style + drive_wheels + wheel_base + length + width + 
               height + curb_weight + engine_type + num_of_cylinders + 
               engine_size + fuel_system + bore + stroke + 
               compression_ratio + horsepower + peak_rpm + city_mpg + 
               highway_mpg, data = cars)
drop1(fit.00, test = "F")

fit.01 <- lm(price ~ normalized_losses + make + fuel_type + aspiration + num_of_doors + 
               body_style + drive_wheels + wheel_base + length + width + 
               height + curb_weight + engine_type + num_of_cylinders + 
               engine_size + fuel_system + bore + stroke + 
               compression_ratio + horsepower + peak_rpm + city_mpg + 
               highway_mpg, data = cars)
drop1(fit.01, test = "F")
fit.02 <- lm(price ~ normalized_losses + make + fuel_type + aspiration + num_of_doors + 
               body_style + drive_wheels + wheel_base + length + width + 
               height + curb_weight + engine_type + num_of_cylinders + 
               fuel_system + bore + stroke + 
               compression_ratio + horsepower + peak_rpm + city_mpg + 
               highway_mpg, data = cars)
drop1(fit.02, test = "F")
fit.03 <- lm(price ~ normalized_losses + make + fuel_type + aspiration + num_of_doors + 
               body_style + drive_wheels + wheel_base + length + width + 
               height + curb_weight + engine_type + num_of_cylinders + 
               fuel_system + bore + 
               compression_ratio + horsepower + peak_rpm + city_mpg + 
               highway_mpg, data = cars)
drop1(fit.03, test = "F")

fit.04 <- lm(price ~ normalized_losses + make + fuel_type + aspiration + num_of_doors + 
               body_style + drive_wheels + wheel_base + length + width + 
               height + curb_weight + engine_type + num_of_cylinders + 
               fuel_system + bore + 
               compression_ratio + horsepower  + city_mpg + 
               highway_mpg, data = cars)
drop1(fit.04, test = "F")

fit.05 <- lm(price ~ make + fuel_type + aspiration + num_of_doors + 
               body_style + drive_wheels + wheel_base + length + width + 
               height + curb_weight + engine_type + num_of_cylinders + 
               fuel_system + bore + 
               compression_ratio + horsepower  + city_mpg + 
               highway_mpg, data = cars)
drop1(fit.05, test = "F")

fit.06 <- lm(price ~ make + fuel_type + aspiration  + 
               body_style + drive_wheels + wheel_base + length + width + 
               height + curb_weight + engine_type + num_of_cylinders + 
               fuel_system + bore + 
               compression_ratio + horsepower  + city_mpg + 
               highway_mpg, data = cars)
drop1(fit.06, test = "F")

fit.07 <- lm(price ~ make + fuel_type + aspiration  + 
               body_style + drive_wheels + wheel_base + length + width + 
               height + curb_weight + engine_type + num_of_cylinders + 
               fuel_system + bore + 
               compression_ratio + horsepower + 
               highway_mpg, data = cars)
drop1(fit.07, test = "F")

fit.08 <- lm(price ~ make + fuel_type+ 
               body_style + drive_wheels + wheel_base + length + width + 
               height + curb_weight + engine_type + num_of_cylinders + 
               fuel_system + bore + 
               compression_ratio + horsepower + 
               highway_mpg, data = cars)
drop1(fit.08, test = "F")
fit.09 <- lm(price ~ make + fuel_type+ 
               body_style + wheel_base + length + width + 
               height + curb_weight + engine_type + num_of_cylinders + 
               fuel_system + bore + 
               compression_ratio + horsepower + 
               highway_mpg, data = cars)
drop1(fit.09, test = "F")

fit.10 <- lm(price ~ make + fuel_type+ 
               body_style + wheel_base + length + width + 
               height + curb_weight + engine_type + num_of_cylinders + 
               fuel_system + bore + 
               compression_ratio + horsepower, data = cars)
drop1(fit.10, test = "F")

fit.11 <- lm(price ~ make + fuel_type+ 
               body_style + wheel_base + length + width + 
               height + curb_weight + engine_type + num_of_cylinders + 
               fuel_system + bore + 
               horsepower,data = cars)
drop1(fit.11, test = "F")

fit.12 <- lm(price ~ make + fuel_type+ 
               body_style + wheel_base + length + width + 
               height + curb_weight + engine_type + num_of_cylinders + 
               bore + 
               horsepower,data = cars)
drop1(fit.12, test = "F")

fit.13 <- lm(price ~ make + fuel_type+ 
               body_style + wheel_base + length + width + 
               height + curb_weight + engine_type + num_of_cylinders + 
               horsepower, data = cars)
drop1(fit.13, test = "F")

fit.14 <- lm(price ~ make + fuel_type+ 
               wheel_base + length + width + 
               height + curb_weight + engine_type + num_of_cylinders + 
               horsepower, data = cars)
drop1(fit.14, test = "F")


fit.15 <- lm(price ~ make + 
               wheel_base + length + width + 
               height + curb_weight + engine_type + num_of_cylinders + 
               horsepower, data = cars)
drop1(fit.15, test = "F")

# End of backward selection by hand of case 2