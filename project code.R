library(tidyverse)

power_city_data <- read.csv("data/Tetuan City power consumption.csv")
head(power_city_data)



#exclude city1 and city2 power consumption for the first model 
power_city_data <- power_city_data[-c(8,9)]
#convert character into datetime object

power_city_data$DateTime <- as.POSIXct(power_city_data$DateTime, format = "%m/%d/%Y %H:%M")


power_city_data$time <- format(power_city_data$DateTime, "%H:%M:%S")





match <- regexpr(".....",power_city_data$time)
power_city_data$time<-regmatches(power_city_data$time,match)

head(power_city_data$time,10)

match <- regexpr("..",power_city_data$time)
power_city_data$hour<-regmatches(power_city_data$time,match)

head(power_city_data$hour,10)


power_city_data$minutes <- sub("..:", "",power_city_data$time)
typeof(result)
head(power_city_data)

