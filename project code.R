library(tidyverse)

power_city_data <- read.csv("data/Tetuan City power consumption.csv")
head(power_city_data)
#explore data
glimpse(power_city_data)

length(colnames(power_city_data))
#exclude city1 and city2 power consumption for the first model 
power_city_data <- power_city_data[-c(8,9)]
colnames(power_city_data)
#data cleaning tasks
sum(is.na(power_city_data))
power_city_data%>%map(~sum(is.na(.)))
glimpse(power_city_data)
unique(as.time(power_city_data$DateTime))
typeof(power_city_data$DateTime)
head(as.POSIXct(strptime(power_city_data$DateTime,"%d/%m/%Y %H:%M")))
power_city_data$DateTime <- as.POSIXct(power_city_data$DateTime, format = "%m/%d/%Y %H:%M")
head(date_object,15)
typeof(date_object)
str(power_city_data$DateTime)
head(power_city_data$DateTime,15)
power_city_data$time <- format(power_city_data$DateTime, "%H:%M:%S")
head(time)
typeof(power_city_data$time)
power_city_data$time %>% mutate_if(is.character,as.numeric)
glimpse(power_city_data)

separate(power_city_data$time,sep =" ",into = c("hour","minutes","second"))
a = str_split("1:2:3",":")
typeof(a)
names(a) <-"hour-minut-second"
b<-str_split(a[["hour-minut-second"]]," ")
length(b)
names(b) <- c("hour","minut","second")
head(power_city_data$time)
time("00:30:00")
head(power_city_data$DateTime)
split("2:2",":")
power_city_data$time <- str_split(power_city_data$time,":")
head(power_city_data$time)
permanent<-str_split(power_city_data$time," ")

#separate time column
volatil<-as.list(power_city_data$time)
typeof(volatil)
head(volatil)
volatil<-str_split(volatil,":")
volatil<-str_split(volatil," ")
              