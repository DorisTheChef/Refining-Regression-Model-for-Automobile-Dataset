#backward variable selection by hand (case 1)
fit.BW_0 <- lm(price ~ symboling + make + fuel_type + aspiration +
                 num_of_doors + body_style + drive_wheels + engine_location +wheel_base +
                 length + width + height + curb_weight + engine_type + num_of_cylinders +
                 engine_size + fuel_system + bore + stroke + compression_ratio + horsepower +
                 peak_rpm + city_mpg + highway_mpg, 
               data = cars)
drop1(fit.BW_0, test = "F")
fit.BW_1 <- lm(price ~ symboling + make + fuel_type + aspiration +
                 body_style + drive_wheels + engine_location +wheel_base +
                 length + width + height + curb_weight + engine_type + num_of_cylinders +
                 engine_size + fuel_system + bore + stroke + compression_ratio + horsepower +
                 peak_rpm + city_mpg + highway_mpg,
               data = cars)
drop1(fit.BW_1, test = "F")
fit.BW_2 <- lm(price ~ symboling + make + fuel_type + aspiration +
                 body_style + drive_wheels + engine_location +wheel_base +
                 length + width + height + curb_weight + engine_type + num_of_cylinders +
                 engine_size + fuel_system + bore + stroke + compression_ratio + horsepower +
                 peak_rpm + highway_mpg, 
               data = cars)
drop1(fit.BW_2, test = "F")
fit.BW_3 <- lm(price ~ symboling + make + fuel_type + aspiration +
                 body_style + drive_wheels + engine_location +wheel_base +
                 length + width + height + curb_weight + engine_type + num_of_cylinders +
                 engine_size + fuel_system + bore + stroke + compression_ratio  +
                 peak_rpm + highway_mpg, 
               data = cars)
drop1(fit.BW_3, test = "F")
fit.BW_4 <- lm(price ~ symboling + make + fuel_type + aspiration +
                 body_style + engine_location +wheel_base +
                 length + width + height + curb_weight + engine_type + num_of_cylinders +
                 engine_size + fuel_system + bore + stroke + compression_ratio  +
                 peak_rpm + highway_mpg,  
               data = cars)
drop1(fit.BW_4, test = "F")
fit.BW_5 <- lm(price ~ symboling + make + fuel_type + aspiration +
                 body_style + engine_location +wheel_base +
                 length + width + height + curb_weight + engine_type + num_of_cylinders +
                 engine_size + fuel_system + bore + compression_ratio  +
                 peak_rpm + highway_mpg,
               data = cars)
drop1(fit.BW_5, test = "F")
fit.BW_6 <- lm(price ~  make + fuel_type + aspiration +
                 body_style + engine_location +wheel_base +
                 length + width + height + curb_weight + engine_type + num_of_cylinders +
                 engine_size + fuel_system + bore + compression_ratio  +
                 peak_rpm + highway_mpg, 
               data = cars)
drop1(fit.BW_6, test = "F")
fit.BW_7 <- lm(price ~  make + fuel_type + aspiration +
                 body_style + engine_location +wheel_base +
                 length + width + height + curb_weight + num_of_cylinders +
                 engine_size + fuel_system + bore + compression_ratio  +
                 peak_rpm + highway_mpg, 
               data = cars)
drop1(fit.BW_7, test = "F")
fit.BW_8 <- lm(price ~  make + fuel_type + aspiration +
                 body_style + engine_location +wheel_base +
                 length + width + height + curb_weight + num_of_cylinders +
                 engine_size + fuel_system + bore + compression_ratio  +
                 peak_rpm,
               data = cars)
drop1(fit.BW_8, test = "F")
fit.BW_9 <- lm(price ~  make + fuel_type + aspiration +
                 body_style + engine_location +wheel_base +
                 length + width + height + curb_weight + num_of_cylinders +
                 engine_size + fuel_system + bore  +
                 peak_rpm, 
               data = cars)
drop1(fit.BW_9, test = "F")
fit.BW_10 <- lm(price ~ make + fuel_type + aspiration +
                  body_style + engine_location +wheel_base +
                  length + width + height + curb_weight + num_of_cylinders +
                  engine_size + bore  +
                  peak_rpm,
                data = cars)
drop1(fit.BW_10, test = "F")
fit.BW_11 <- lm(price ~  make  + aspiration +
                  body_style + engine_location +wheel_base +
                  length + width + height + curb_weight + num_of_cylinders +
                  engine_size + bore  +
                  peak_rpm, 
                data = cars)
drop1(fit.BW_11, test = "F")
# End of backward variable selection by hand for case 1

#Stepwise variable selection (case 1)
# ADD1
cars$make <- as.factor(cars$make)
cars$engine_location <- as.factor(cars$engine_location)

fit.SW_0 <- lm(price~1, data = cars)
add1(fit.SW_0, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.SW_1 <- lm(price~engine_size, data = cars)
drop1(fit.SW_1, test = "F")

add1(fit.SW_1, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")

fit.SW_2 <- lm(price~engine_size + make, data = cars)
drop1(fit.SW_2, test = "F")

add1(fit.SW_2, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.SW_3 <- lm(price~engine_size + make + curb_weight, data = cars)
drop1(fit.SW_3, test = "F")

add1(fit.SW_3, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.SW_4 <- lm(price~engine_size + make + curb_weight + engine_location, data = cars)
drop1(fit.SW_4, test = "F")

add1(fit.SW_4, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.SW_5 <- lm(price~engine_size + make + curb_weight + engine_location + width, data = cars)
drop1(fit.SW_5, test = "F")

add1(fit.SW_5, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.SW_6 <- lm(price~engine_size + make + curb_weight + engine_location + width + peak_rpm, data = cars)
drop1(fit.SW_6, test = "F")

add1(fit.SW_6, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.SW_7 <- lm(price~engine_size + make + curb_weight + engine_location + width + peak_rpm + aspiration, data = cars)
drop1(fit.SW_7, test = "F")

add1(fit.SW_7, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.SW_9 <- lm(price~engine_size + make + curb_weight + engine_location + width + peak_rpm + aspiration + num_of_cylinders, data = cars)
drop1(fit.SW_9, test = "F")

add1(fit.SW_9, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.SW_10 <- lm(price~engine_size + make + curb_weight + engine_location + width + peak_rpm + aspiration + num_of_cylinders + engine_type , data = cars)
drop1(fit.SW_10, test = "F")

add1(fit.SW_10, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.SW_11 <- lm(price~engine_size + make + curb_weight + engine_location + width + peak_rpm + aspiration + num_of_cylinders + engine_type + stroke, data = cars)
drop1(fit.SW_11, test = "F")

add1(fit.SW_11, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.SW_12 <- lm(price~engine_size + make + curb_weight + engine_location + width + peak_rpm + aspiration + num_of_cylinders + engine_type + stroke + highway_mpg, data = cars)
drop1(fit.SW_12, test = "F")

add1(fit.SW_12, price ~ symboling + make + fuel_type + aspiration + num_of_doors + 
       body_style + drive_wheels + engine_location + wheel_base   + length + width + 
       height + curb_weight + engine_type + num_of_cylinders + 
       engine_size + fuel_system + bore + stroke + 
       compression_ratio + horsepower + peak_rpm + city_mpg + 
       highway_mpg, test = "F")
fit.SW_13 <- lm(price ~ engine_size + make + curb_weight + engine_location + width + peak_rpm + aspiration + num_of_cylinders + engine_type + stroke + highway_mpg + body_style, data = cars)
drop1(fit.SW_13, test = "F")
# End of stepwise variable selection by hand for case 1.