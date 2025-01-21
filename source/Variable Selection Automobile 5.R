#calculate coorealtion of potencial highly coorelated variables
# Calculate correlation
cor(cars$num_of_cylinders, cars$engine_size, use = "complete.obs")
cor(cars$num_of_cylinders, cars$horsepower, use = "complete.obs")
cor(cars$num_of_doors, cars$wheel_base, use = "complete.obs")

#Compare models for alpha is 0.05 and 0.1 in case 1
fit11 <- lm(price ~ engine_size + make + width + engine_location +
              peak_rpm + aspiration +engine_type + stroke +
              body_style 
            + height +  bore 
            , 
            data = cars)
fit11_compare <- lm(price ~ engine_size + make + width + engine_location +
                      peak_rpm + aspiration +engine_type + stroke +
                      body_style 
                    + height +  bore +compression_ratio + fuel_system + fuel_type
                    , 
                    data = cars)

#compare_their plots and statistics:
anova(fit11, fit11_compare)
plot(fit11)
plot(fit11_compare)
summary(fit11)
summary(fit11_compare)
anova(fit11)
anova(fit11_compare)

#The p-value (0.1143) is greater than the typical significance level (e.g., 0.05).
#This means that the additional predictors in Model 2 do not significantly improve the model's fit 
#compared to Model 1. use model with significance level 0.05.fit11

#Stepwise for case 1
drop1(fit11, test = "F")
fit12 <- lm(price ~ engine_size + make + width + engine_location +
              peak_rpm + aspiration +engine_type + stroke +
              body_style 
            +  bore 
            , 
            data = cars)
add1 (fit12, price ~ engine_size + make + width + engine_location +
        peak_rpm + aspiration +engine_type + stroke +
        body_style 
      + height +  bore 
      ,  
      test = "F")
drop1(fit12, test = "F")
fit13 <- lm(price ~ engine_size + make + width + engine_location +
              peak_rpm + aspiration +engine_type +
              body_style 
            +  bore 
            , 
            data = cars)
add1 (fit13, price ~ engine_size + make + width + engine_location +
        peak_rpm + aspiration +engine_type + stroke +
        body_style 
      + height +  bore 
      ,  test = "F")
drop1(fit13, test = "F")

#at a 0.05 level drop engine_type
fit14 <- lm(price ~ engine_size + make + width + engine_location +
              peak_rpm + aspiration +
              body_style +  bore,
            data = cars)
add1 (fit14, price ~ engine_size + make + width + engine_location +
        peak_rpm + aspiration +engine_type + stroke +
        body_style 
      + height +  bore 
      ,  test = "F")
drop1(fit14, test = "F")

#compare fit13 and fit14 (0.1 and 0.05)
anova(fit13, fit14)
#final model is fit14 for case 1.


#Compare models for alpha is 0.05 and 0.1 in case 2
common_data <- cars[complete.cases(cars[, c("make", "aspiration", "num_of_doors", 
                                            "body_style", "drive_wheels", 
                                            "engine_location", "wheel_base", 
                                            "height", "engine_type", 
                                            "num_of_cylinders", "fuel_system", 
                                            "compression_ratio", "horsepower")]), ]

fit1 <- lm(price ~ make + aspiration + body_style + drive_wheels + 
             engine_location + wheel_base + height + engine_type + 
             num_of_cylinders + compression_ratio + horsepower, 
           data = common_data)

fit1_compare <- lm(price ~ make + aspiration + num_of_doors + body_style + 
                     drive_wheels + engine_location + wheel_base + 
                     height + engine_type + num_of_cylinders + fuel_system + 
                     compression_ratio + horsepower, 
                   data = common_data)
anova(fit1, fit1_compare)

#second round variable selection for case 1 model
fit1 <- lm(price ~ make + aspiration + body_style + drive_wheels + 
             engine_location + wheel_base + height + engine_type + 
             num_of_cylinders + compression_ratio + horsepower, data = cars)
drop1(fit1, test = "F")
fit2 <- lm(price~ make + body_style + drive_wheels + 
             engine_location + wheel_base + height + engine_type + 
             num_of_cylinders + compression_ratio + horsepower,
           data = cars)
add1 (fit2, price ~ make + aspiration+ body_style +drive_wheels + engine_location + wheel_base + height + engine_type + num_of_cylinders + compression_ratio + horsepower,test = "F")
drop1(fit2, test = "F")

fit3 <- lm(price~ make + body_style + 
             engine_location + wheel_base + height + engine_type + 
             num_of_cylinders + compression_ratio + horsepower,,data = cars)
add1 (fit3, price ~ make + aspiration+ body_style  + engine_location + wheel_base  + engine_type + num_of_cylinders + compression_ratio + horsepower + drive_wheels + height,
      test = "F")
drop1(fit3, test = "F")

fit4 <- lm(price ~ make + body_style + 
             engine_location + wheel_base  + engine_type + 
             num_of_cylinders + compression_ratio + horsepower,data = cars)
add1 (fit4, price ~ make + aspiration+ body_style +drive_wheels + engine_location + wheel_base + height + engine_type + num_of_cylinders + compression_ratio + horsepower,test = "F")
drop1(fit4, test = "F")
add1 (fit4, price ~ make + aspiration+ num_of_doors+ body_style +drive_wheels + engine_location + wheel_base + height + engine_type + num_of_cylinders +fuel_system + compression_ratio + horsepower, test = "F")

# fit4 (0.05) and fit2 (0.1) are the final model for variable group without engine_location
#compare them:
anova(fit4, fit2)
#fit4 is good enough, it is the final model for case 2. drive_wheels and height are not significant.

#final models:
#Case 1:
#final model 1:(strat without normalized_losses, with engine_location, alpha = 0.05) 
fit.final1 <- lm(price ~ engine_size + make + width + engine_location + peak_rpm + aspiration + 
                   body_style + bore,
                 data = cars)

#Case 2:
# final model 2:(start without engine_location, with normalized_losses, alpha = 0.05) 
fit.final2 <-lm( price ~ make + body_style + engine_location + 
                   wheel_base + engine_type + num_of_cylinders + compression_ratio + 
                   horsepower, data = cars)
```
  


