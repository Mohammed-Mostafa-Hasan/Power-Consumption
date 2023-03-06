library(tidyverse)
library(xts)
library(broom)
library(ppcor)
# goals of the project
# 1-what is the highest parameter effect to power consumption 
# 2-is there significant different between three region in power consumption
# 3- measure relation between variable

#read data
power_city_data <- read.csv("data/Tetuan City power consumption.csv")
head(power_city_data)
nrow(power_city_data)


#exclude city1 and city2 power consumption for the first model 
#power_city_data <- power_city_data[-c(8,9)]

#convert character into datetime object
power_city_data$DateTime <- as.POSIXct(power_city_data$DateTime, format = "%m/%d/%Y %H:%M")
nrow(power_city_data)

#show brief about data and it's type
glimpse(power_city_data)

#separate date and time form DateTime column
power_city_data$Date <- as.Date(power_city_data$DateTime)
power_city_data$time <- format(power_city_data$DateTime, "%H:%M:%S")

#Remove original DateTime column
n_col <- which(names(power_city_data) == "DateTime")
power_city_data <- power_city_data[,-n_col]
head(power_city_data)
#exclude seconds form time columns 
match <- regexpr(".....",power_city_data$time)
power_city_data$time<-regmatches(power_city_data$time,match)
head(power_city_data$time,10)

#split Hour and Minutes into new columns & exclude time column
match <- regexpr("..",power_city_data$time)
power_city_data$hour<-regmatches(power_city_data$time,match)
power_city_data$minutes <- sub("..:", "",power_city_data$time)
head(power_city_data,10)

n_col2 <- which(names(power_city_data)=="time")
power_city_data <- power_city_data[,-n_col2]
glimpse(power_city_data)

#separate datetime column into 
power_city_data <-power_city_data%>%
  separate(Date,sep = "-", into = c("year","month","day"))

#convert char columns into numeric
power_city_data <- power_city_data %>%
  mutate_all(type.convert)%>%
  mutate_if(is.character,as.numeric)
head(power_city_data)

#save cleaned file into data folder
write.csv(power_city_data,"data/cleaned_Tetuan_power.csv")
#find correlation between some variable 
attach(power_city_data)
pcor(cbind(hour,Zone.1.Power.Consumption,Zone.2..Power.Consumption,Zone.3..Power.Consumption))

#represent correlation visually 
pairs(cbind(hour,Zone.1.Power.Consumption,Zone.2..Power.Consumption,Zone.3..Power.Consumption))

#show boxplot to find distribution and detect outlier in the data 
ggplot(power_city_data,aes(x=month,y=Zone.1.Power.Consumption))+
  geom_boxplot(fill = "bisque",color = "black", alpha = 0.3)+
  geom_jitter(color = 'orange', alpha=0.1)+
  scale_x_discrete(limits = factor(1:12))+
  scale_y_discrete(limits = seq(10000,50000,5000))

range(1:5,2)
seq(1,5,2)  
anova()
cor.test()
#remember null hypothsise state for 
#if p-value > .005 then we reject null hypothise which state for there no 
#different between tow variables  

#check missing data
power_city_data %>% map(~sum(is.na(.)))


#check data type for each columns
sapply(power_city_data,typeof)

converted_data<- power_city_data%>%
  select(year,month,day,hour,minutes)%>%
  mutate_all(type.convert)%>%
  mutate_if(is.character,as.numeric)
colnames(converted_data)
sapply(converted_data,typeof)
# Drop character columns type 
col_year    <-which(names(power_city_data)=='year')
col_month   <-which(names(power_city_data)=='month')
col_day     <-which(names(power_city_data)=='day')
col_time    <-which(names(power_city_data)=='time')
col_hour    <-which(names(power_city_data)=='hour')
col_minutes <-which(names(power_city_data)=='minutes')

colnames(power_city_data)

power_city_data <- power_city_data[(-c(col_year   
                                      ,col_month  
                                      ,col_day    
                                      ,col_time   
                                      ,col_hour   
                                      ,col_minutes))]

power_city_data <- cbind(converted_data,power_city_data)
colnames(power_city_data)

glimpse(power_city_data)

qplot(power_city_data$year,bins = 30,xlab = 'years',ylab = 'years count'
      ,main = 'count number of years power city consumption' ,
      fill = I('red'),colour = I('blue')
      )

length(power_city_data %>%filter(year == '2017'))

qplot(power_city_data$Zone.1.Power.Consumption,geom = "histogram",bins = 15,
      colour = I('black'),xlab = 'power consumtion',ylab = 'count',main = 'Histogram',
      fill = I("red"))
ggplot(power_city_data,aes(x= hour))+
  geom_line(aes(y = Zone.1.Power.Consumption),color = "red")


power_city_data$h<-as.numeric(time(power_city_data$hour))
xlab(xlim())
head(h,100)
#remove h column
col <- which(names(power_city_data)=='h')
 power_city_data<-power_city_data[-col]
colnames(power_city_data)

test_data_op <- read.csv("data/Tetuan City power consumption.csv")
#?xts()
test_data_op$DateTime<-as.POSIXct(test_data_op$DateTime, format= "%m/%d/%Y %H:%M")

sum(is.na(test_data_op$DateTime))
test_data_op$Date<-xts(test_data_op[,-1],order.by = test_data_op$DateTime)
ncol(test_data_op)

typeof(test_data_op$DateTime)
head(test_data_op$DateTime)

test_data_op$DateTime <- as.Date(test_data_op$DateTime)
tail(test_data_op$Date)
#read the data again for some modification 

#----------------------------#for time series data-------------------
#read data 
pow_consumming <- read.csv("data/Tetuan City power consumption.csv")
dim(pow_consumming)
#discover data type and brief summary
glimpse(pow_consumming)
 
#convert Datetime column from char into data time object 
pow_consumming$DateTime<-as.POSIXct(pow_consumming$DateTime,format = "%m/%d/%Y %H:%M")
head(pow_consumming$DateTime)

#extract date form datetime column
pow_consumming$Date <- as.Date(pow_consumming$DateTime)
head(pow_consumming$Date)
dim(pow_consumming)
#convert data into time series data
pow_city_tseies <-ts(pow_consumming,start = c(2016,12),end = c(2017,12),frequency = 100) 
pow_city_tseies<-as.data.frame(pow_city_tseies)
dim(pow_city_tseies)
#remove datetime column 
pow_city_tseies <-subset(pow_city_tseies,select = -c(DateTime))

typeof(pow_city_tseies)
colnames(pow_city_tseies)
head(pow_city_tseies)


#show different time consumption for each zone
ggplot(pow_city_tseies,aes(x=Date))+
  geom_line(aes(y= Zone.1.Power.Consumption),color = "red")+
  geom_line(aes(y=Zone.2..Power.Consumption),color ="blue")+
  geom_line(aes(y=Zone.3..Power.Consumption),color ="green")+
  ylab("power consumming over 3 Zones")

#create boxplot for date and zone one 
ggplot(pow_city_tseies,aes(x = Date, y = Zone.3..Power.Consumption))+
  geom_boxplot()
dim(pow_city_tseies)

#select some columns for data visualization 

power_zones <- pow_city_tseies %>%
  select(Date,Zone.1.Power.Consumption, Zone.2..Power.Consumption, Zone.3..Power.Consumption)
dim(power_zones)
#convert date into tidy data

gatherd_data <- gather(power_zones,key = 'zones_names',value = 'power_values',-Date)
head(gatherd_data)
colnames(gatherd_data)

#visualize
ggplot(gatherd_data,aes(x = Date, y = power_values,))+
  geom_boxplot(aes(color = zones_names) )+
  labs(ylim =c(15000, 50000))
  
#now we will make some statistics
summary(gatherd_data)

dim(gatherd_data)


time_series_data$DateTime <- as.POSIXct(time_series_data$DateTime,format = '%d/%m/%Y %H:%M')
time_series_data$Date <- as.numeric(time(time_series_data))
typeof(time_series_data)
time_series_data <- as.data.frame(time_series_data)
typeof(time_series_data)
EuStockDF <- as.data.frame(EuStockMarkets)
typeof(EuStockDF)
typeof(EuStockMarkets)
time_series_data$Date<-as.numeric(time(time_series_data))
glimpse(EuStockMarkets)
head(time_series_data$DateTime)
time_series_data$DateTime <- as.numeric(format(time_series_data$DateTime, "%Y"))
?ts(test_data_op,start = 1,end = length(test_data_op),frequency = 1)


head(pow_city_tseies)
