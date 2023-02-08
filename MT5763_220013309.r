# https://github.com/erna1997/MT5763_1_22001330.git

# Set up the working directory and loaded libraries
setwd("~/Documents/MT5763 Software for Data Analysis/Assignments/Assignment_1")
library(tidyverse)
library(dplyr)
library(lubridate)
library(scales)
library(ggplot2)
library(olsrr)
library(sjPlot)

# 1.1 Data Wrangling.
# Read in the BikeSeoul.csv data frame as bike_seoul
bike_seoul <- read.csv("BikeSeoul.csv")

# Data wrangling of bike_seoul data frame: removed unwanted columns. 
# Filtered out observations with no bike count data, changed some column names, 
# modified date format, changed factor levels of holiday column, 
# changed order of season column, turned percent column to percentage, created
# a new column full_date. select() doesn't seem to work with pipes, maybe 
# because of the MAC?.
bike_seoul <- select(bike_seoul, -Visibility..10m., -Dew.point.temperature.C., 
         -Solar.Radiation..MJ.m2., -Rainfall.mm., -Snowfall..cm.)

bike_seoul <- bike_seoul %>%
  filter(!Rented.Bike.Count == "" | !Rented.Bike.Count == NA | 
           !Rented.Bike.Count == 0) %>%
  rename("count" = "Rented.Bike.Count",
         "temperature" = "Temperature.C.",
         "humidity" = "Humidity...",
         "wind_speed" = "Wind.speed..m.s.",
         "season" = "Seasons",
         "holiday" = "Holiday",
         "date" = "Date",
         "hour" = "Hour")

bike_seoul <- select(bike_seoul, -Functioning.Day)

# Set up inputs for full_date column, created the new full_date column.
# Inputs:
#  years, months and days - extracted from the date column. 
# Refactored the holiday column and the season column,.
bike_seoul$date <- as.Date(bike_seoul$date, format = "%d/%m/%Y")
years <- format(bike_seoul$date, format = "%Y")
months <- format(bike_seoul$date, format = "%m")
days <- format(bike_seoul$date, format = "%d")
bike_seoul$full_date <- make_datetime(year = years, 
                                      month = months, 
                                      day = days, 
                                      hour = bike_seoul$hour, 
                                      min = 00,
                                      sec = 00)

bike_seoul$holiday <- recode_factor(bike_seoul$holiday, 
                                    congruent = "Yes",
                                    incongruent = "No")

bike_seoul$season <- recode_factor(bike_seoul$season, 
                                    "Spring", 
                                    "Summer", 
                                    "Autumn", 
                                    "Winter")

# -----------------------------------------------------------------------------

# 1.2 Data wrangling 
# Read in the BikeWashingtondDC.csv data frame as bike_washington_DC
bike_washington_DC <- read.csv("BikeWashingtonDC.csv")

# Data wrangling of bike_washington_DC data frame: removed unwanted columns, 
# changed some column names, converted humidity column to percentage,
# converted temperature column to degrees Celcius, converted windspeed 
# column to m/s, changed factor levels of season column, changed factor levels 
# of Holiday column.
# select() function doesn't work with tidyverse pipes, therefore the 
# updated data frame has separate lines of code.
bike_washington_DC <- select(bike_washington_DC, -instant, -yr, -mnth, 
                             -weekday,- workingday, -weathersit, -atemp, 
                             -casual, -registered)

bike_washington_DC <- bike_washington_DC %>% 
                                        rename("count" = "cnt",
                                        "date" = "dteday",
                                        "hour" = "hr",
                                        "season" = "season",
                                        "holiday" = "holiday",
                                        "temperature" = "temp",
                                        "humidity" = "hum",
                                        "wind_speed" = "windspeed")
  
bike_washington_DC$humidity <- bike_washington_DC$humidity * 100

bike_washington_DC$temperature <- round(bike_washington_DC$temperature * 47 - 8, 
                                        digits = 1)

bike_washington_DC$wind_speed <- round(bike_washington_DC$wind_speed * 67 * 
                                         5 / 18, digits = 1)

bike_washington_DC$season <- recode_factor(bike_washington_DC$season, 
                                           "Winter", 
                                           "Spring", 
                                           "Summer", 
                                           "Autumn")

bike_washington_DC$holiday <- as.logical(bike_washington_DC$holiday)
bike_washington_DC$holiday <- factor(bike_washington_DC$holiday, 
                                     labels = c("No Holiday", "Holiday"))

# Refactored the date column.
# Set up to create full_date column, created the new full_date column.
# Inputs:
#  years, months and days - extracted from the date column.
bike_washington_DC$date <- as.Date(bike_washington_DC$date, format = "%Y-%m-%d")
years <- format(bike_washington_DC$date, format = "%Y")
months <- format(bike_washington_DC$date, format = "%m")
days <- format(bike_washington_DC$date, format = "%d")
bike_washington_DC$full_date <- make_datetime(year = years, 
                                      month = months, 
                                      day = days, 
                                      hour = bike_washington_DC$hour, 
                                      min = 00,
                                      sec = 00)

# -----------------------------------------------------------------------------
# 2. Data Visualisation.
# To compare the similarities and differences between bike_seoul and 
# bike_washington_DC, a few plots are created. 

# 2.1. Temperature over the years.
# Used ggplot2 to create separate plots for the two data sets on bikes rented 
# depending on temperature. 
# ylim() is set to be the same for both of the plots.
ggplot(bike_seoul) +
  geom_point(aes(x = full_date, y = temperature), alpha = 0.02) +
  stat_smooth(aes(x = full_date, y = temperature)) +
  xlab("Date") + ylab("Temperature, C") + 
  ggtitle("Temperature in Seoul Over the Years") + 
  ylim(-20, 40) +
  theme_minimal()

ggplot(bike_washington_DC) +
  geom_point(aes(x = full_date, y = temperature), alpha = 0.02) +
  stat_smooth(aes(x = full_date, y = temperature)) +
  xlab("Date") + ylab("Temperature, C") + 
  ggtitle("Temperature in Washington DC Over the Years") + 
  ylim(-20, 40) +
  theme_minimal()

# 2.2. Season and bikes rented.
# Used ggplot2 to create separate plots for the two data sets on bikes rented 
# depending on seasons. 
ggplot(bike_seoul) +
  geom_col(aes(x = season, y = count, fill = season)) +
  xlab("Season") + ylab("Bikes Rented") + 
  labs(fill = "Season") +
  ggtitle("Bikes Rented in Seoul by Season") +
  ylim(0, 2500000) +
  theme_minimal()

ggplot(bike_washington_DC) +
  geom_col(aes(x = season, y = count, fill = season)) +
  xlab("Season") + ylab("Bikes Rented") + 
  labs(fill = "Season") +
  ggtitle("Bikes Rented in Washington DC by Season") +
  ylim(0, 1200000) +
  theme_minimal()

# 2.3. Holidays and bikes rented.
# Used ggplot2 to create separate plots for the two data sets on bikes rented 
# depending on whether it's a holiday or a week day. 
ggplot(bike_seoul) +
  geom_col(aes(x = holiday, y = count, fill = holiday)) +
  xlab("Day") + ylab("Bikes Rented") + 
  labs(fill = "Type of Day") + 
  ggtitle("Bikes Rented in Seoul Depending on the Day") + 
  theme_minimal()

ggplot(bike_washington_DC) +
  geom_col(aes(x = holiday, y = count, fill = holiday)) +
  xlab("Day") + ylab("Bikes Rented") + 
  labs(fill = "Type of Day") +
  ggtitle("Bikes Rented in Washington DC Depending on the Day") + 
  theme_minimal()

# 2.4. Time of day and bikes rented.
# Used ggplot2 to create separate plots for the two data sets on bikes rented 
# depending on hour of the day. 
# Set the ylim() for both cities to be the same.
ggplot(bike_seoul) +
  stat_smooth(aes(x = hour, y = count)) +
  xlab("Hour") + ylab("Bikes Rented") + 
  ggtitle("Bikes Rented in Seoul Depending on Time of Day") + 
  ylim(-100, 1500) +
  theme_minimal()

ggplot(bike_washington_DC) +
  stat_smooth(aes(x = hour, y = count)) +
  xlab("Hour") + ylab("Bikes Rented") + 
  ggtitle("Bikes Rented in Washington DC Depending on Time of Day") + 
  ylim(-100, 1500) +
  theme_minimal()

# 2.4. Meteorological variables and bikes rented.
# Used ggplot2 to create separate plots for the two data sets on bikes rented 
# depending on temperature, wind speed and humidity.
# Plots for bike_seoul
ggplot(bike_seoul) +
  stat_smooth(aes(x = temperature, y = count)) +
  xlab("Temperature") + ylab("Bikes Rented") + 
  ggtitle("Bikes Rented in Seoul Depending on Temperature") + 
  theme_minimal()

ggplot(bike_seoul) +
  stat_smooth(aes(x = wind_speed, y = count)) +
  xlab("Wind Speed") + ylab("Bikes Rented") + 
  ggtitle("Bikes Rented in Seoul Depending on Wind Speed") + 
  theme_minimal()

ggplot(bike_seoul) +
  stat_smooth(aes(x = humidity, y = count)) +
  xlab("Humidity") + ylab("Bikes Rented") + 
  ggtitle("Bikes Rented in Seoul Depending on Humidity") + 
  theme_minimal()

# Plots for bike_washington_DC
ggplot(bike_washington_DC) +
  stat_smooth(aes(x = temperature, y = count)) +
  xlab("Temperature") + ylab("Bikes Rented") +
  ggtitle("Bikes Rented in Washington DC Depending on Temperature") + 
  theme_minimal()

ggplot(bike_washington_DC) +
  stat_smooth(aes(x = wind_speed, y = count)) +
  xlab("Wind Speed") + ylab("Bikes Rented") +
  ggtitle("Bikes Rented in Washington DC Depending on Wind Speed") + 
  theme_minimal()

ggplot(bike_washington_DC) +
  stat_smooth(aes(x = humidity, y = count)) +
  xlab("Humidity") + ylab("Bikes Rented") +
  ggtitle("Bikes Rented in Washington DC Depending on Humidity") + 
  theme_minimal()

# 3. Statistical Modelling.
# 3.1. Fitted the linear model for bikes rented in Seoul with seasons, 
# air temperature, humidity and wind speed as predictors.
# Printed out the summary for the linear model.
# Found the 97% confidence intervals for the estimated regression coefficients.
seoul_lm <- lm(count ~ season + temperature + humidity + wind_speed, 
                 data = bike_seoul)
summary(seoul_lm)

# Because the residuals are not very symmetrical, I wanted to plot them out
# and check if they look normal
ols_plot_resid_qq(seoul_lm)

# Modelling diagnostics: noise
seoul_noise <- plot_model(seoul_lm, type = "diag")
plot_grid(seoul_noise)

# Model diagnostics: signal
seoul_signal <- plot_model(seoul_lm, type = "eff")
plot_grid(seoul_signal)

# Histogram of residuals
seoul_lm_resid <- resid(seoul_lm)
hist(seoul_lm_resid)

# Fitted the linear model for bikes rented in Washington DC with seasons, 
# air temperature, humidity and wind speed as predictors.
# Printed out the summary for the linear model.
# Found the 97% confidence intervals for the estimated regression coefficients.
washington_DC_lm <- lm(count ~ season + temperature + humidity + wind_speed, 
               data = bike_washington_DC)
summary(washington_DC_lm)

# Checking if the residual distribution is normal
ols_plot_resid_qq(washington_DC_lm)

# Model diagnostics: noise
pw <- plot_model(washington_DC_lm, type = "diag")
plot_grid(pw)

# Model diagnostics: signal
wash_signal <- plot_model(washington_DC_lm, type = "eff")
plot_grid(wash_signal)

# Histogram of residuals
wash_lm_resid <- resid(washington_DC_lm)
hist(wash_lm_resid)

# 3.2. Found the bike_seoul confidence intervals for regression coefficients
confint(seoul_lm, level = 0.97) 

# Found the bike_washington_DC confidence intervals for regression coefficients.
confint(washington_DC_lm, level = 0.97)


# 3.3. Expected number of rented bikes n winter when the air temperature is 
# freezing (0◦C), in the presence of light wind (0.5m/s) and a humidity of 20%.
# Expected number for bike_seoul.
seoul_pred <- data.frame(season = "Winter", 
                     temperature = 0, 
                     wind_speed = 0.5,
                     humidity = 20)
predict(seoul_lm, newdata = seoul_pred)

# Provided the 90% prediction intervals for bike count in Seoul
predict(seoul_lm, newdata = seoul_pred, interval = "predict", level = 0.9)

# Expected number of rented bikes for bike_washington_DC
washington_DC_pred <- data.frame(season = "Winter", 
                         temperature = 0, 
                         wind_speed = 0.5,
                         humidity = 20)
predict(washington_DC_lm, newdata = washington_DC_pred)

# Provided the 90% prediction intervals for bike count in Washington DC
predict(washington_DC_lm, newdata = washington_DC_pred, 
        interval = "predict", level = 0.9)
