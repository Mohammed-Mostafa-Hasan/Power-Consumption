# data analysis file
# goal of the file:

#1- find the relation between feature to see it's influence on target variable
#2- make some descriptive statistics and inferential statistics 
#3- discover data distribution using data visualization techniques

library(tidyverse)

power_data <- read.csv("data/cleaned_Tetuan_power.csv")
pairs(power_data)
