#create linear seach algorithm
library(DBI)
install.packages("RJDBC")
install.packages("RODBC")
findCard <- function(cards, query){
  for (card in cards) {
    if (card !=query){ 
       next
    }
    else {
      print("founded card number")
      break
    }
      
  }
}
get_card <- function(cards, query) {
  for (card in cards) {
    if (card == query) {
      print("card founded in cards which is")
      
      return(card)
    }
  }
}

#dealing with structure data type in R
print(c("This is iteration number:",as.character(5)))
regexpr('@.*',c('mohammed@gmail.com',' ahmed mostafa','nedaa@gmail.com'))
empylist = vector(mode = "list", length = 4 )
empylist[[1]]<-1 
empylist
empylist[["newcol"]]<-43
length(empylist)
empylist
names(empylist)
A <- matrix(1:10,nrow = 5)
dim(A)
B <- matrix(21:30, nrow=5)
B
A
A%*%B
c <- matrix(11:20,ncol = 5)
dim(c)
dim(A)
A %*% c
array(1:6,dim = c(2,3))
#using webscrapping
library(rvest)
ribal <- read_html('http://www.jaredlander.com/data/ribalta.html')
class(ribal)
ribal
ribalta %>% html_nodes('ul') %>% html_nodes('span')
ribal %>%
  html_nodes('table.food-items')%>%
   magrittr::extract2(5)%>%
    html_table()
ribal%>%html_node('#longitude')%>%html_attr('value')

#define linear search algorithm 
get_cards <- function(cards,query){

  for (card in cards){
    if (card==query){
    return(card)
      
    }
   
  }
  return("card not found")
}
elements <- matrix(1:12,ncol = 12)
get_cards(elements,2)

elements[y]

8<=4
elements[2]==2




library(Hmisc)


fruit <- c("banna","orange","strawbery")
fruitLength <- nchar(fruit)
names(fruitLength)<-fruit
fruitLength
fruitLength2 <- rep(NA,length(fruit))
names(fruitLength2) <- fruit
for (a in fruit){
  
  fruitLength2[a]<-nchar(a)}

  run.this <- function(x, hj=mean)
     {
       do.call(hj, args=list(x))
       }
  run.this(1:10)


?lines()
help("co2")



wineUrl <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data'
wine <- read.table(wineUrl, header=FALSE, sep=',',
                      stringsAsFactors=FALSE,
                      col.names=c('Cultivar', 'Alcohol', 'Malic.acid',
                                   'Ash', 'Alcalinity.of.ash',
                                   'Magnesium', 'Total.phenols',
                                   'Flavanoids', 'Nonflavanoid.phenols',
                                   'Proanthocyanin', 'Color.intensity',
                                   'Hue', 'OD280.OD315.of.diluted.wines',
                                   'Proline'
                                   ))
head(wine)
length(colnames(wine))

---------------------------------------------------------------------------------
  class(co2)
plot(CO2)
colnames(CO2)
co2.linear.model <- lm(co2 ~ time(co2) )
plot(co2, main='Atmospheric CO2 Concentration with Fitted Line')
abline(co2.linear.model)
( co2.residuals = resid( co2.linear.model ) )

par(mfrow=c(1,3))
( c02.residuals = resid( co2.linear.model ) )
hist(co2.residuals, main= "Histogram of CO2 Residuals")
qqnorm(c02.residuals, main= "Normal Probability Plot")
qqline(c02.residuals)
plot(c02.residuals ~ time(co2), main="Residuals on Time")
head(time( co2))
?time()
plot(c02.residuals ~ time(co2), xlim=c(1960, 1963), main="Zoomed in Residuals on Time")

plot(extra~group, data=sleep, main = "Extra Sleep in Gossett Data by Group")




ts_to_df <- as.data.frame(EuStockMarkets)
head(ts_to_df)
ts_to_df$Date <- as.numeric(time(ts_to_df))
#-------------------------------------#[practice on tidyr libraray -------------
pairs(pow_city_tseies,pch= 21,bg=c('blue'))
?pairs()
library(tidyr)

# Create a sample data frame
df <- data.frame(
  id = c(1, 2, 3),
  var1 = c(10, 20, 30),
  var2 = c(15, 25, 35)
)

# Reshape the data frame from wide to long format
tidy_df <- tidy(df, 
                key = "variable",
                value = "value", 
                names_to = "variable_name",
                values_to = "value")

# Print the tidied data frame
head(tidy_df)


# Create a sample data frame
df <- data.frame(
  vendor = c("A", "B", "C"),
  apples_week1 = c(10, 20, 15),
  oranges_week1 = c(5, 8, 10),
  apples_week2 = c(15, 18, 20),
  oranges_week2 = c(10, 12, 15)
)

# Print the original data frame
print(df)


# Reshape the data frame from wide to long format using gather()
tidy_df <- gather(df, key = "week_fruit", value = "sales", -vendor)

# Print the tidied data frame
print(tidy_df)
#-----------------------------------------------------------
a_samples <- rnorm(200,mean = 1, sd = 2) 
b_samples <- rnorm(200,mean = 0, sd = 1)
df <- data.frame(label = factor(rep(c('A','B'), each = 200))
          ,value = c(a_samples,b_samples))
ggplot(df,aes(x = value,y= label))+
  geom_boxplot()
ggplotly()
?theme()
p1 <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  labs(title = "Fuel economy declines as weight increases")
p1

# Plot ---------------------------------------------------------------------
p1 + theme(axis.text.x = element_text(angle = 30,color = 'blue'))
p1 + theme(plot.background = element_rect(fill = "green"))

# Panels --------------------------------------------------------------------

p1 + theme(panel.background = element_rect(fill = "white", colour = "grey50"))
p1 + theme(panel.border = element_rect(linetype = "dashed", fill = NA))
p1 + theme(panel.grid.major = element_line(colour = "black"))
p1 + theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank()
)

# Put gridlines on top of data
p1 + theme(
  panel.background = element_rect(fill = NA),
  panel.grid.major = element_line(colour = "grey50"),
  panel.ontop = TRUE
)
p2 <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point(aes(colour = factor(cyl), shape = factor(vs))) +
  labs(
    x = "Weight (1000 lbs)",
    y = "Fuel economy (mpg)",
    colour = "Cylinders",
    shape = "Transmission"
  )
# Position
p2 + theme(legend.position = "none")
p2 + theme(legend.justification = "top")
p2 + theme(legend.position = "bottom")

p2 + theme(
  legend.position = c(.95, .95),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)

p2 + theme(
  legend.box.background = element_rect(),
  legend.box.margin = margin(6, 6, 6, 6)
)

p2 + theme(legend.key = element_rect(fill = "white", colour = "black"))
p2 + theme(legend.text = element_text(size = 8, colour = "red"))
p2 + theme(legend.title = element_text(face = "bold"))

p3 <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  facet_wrap(~ cyl)
p3

p3 + theme(strip.background = element_rect(colour = "black", fill = "white"))
p3 + theme(strip.text.x = element_text(colour = "white", face = "bold"))
p3 + theme(panel.spacing = unit(1, "lines"))
library(leaflet)
#------------------------------------------------------------------

?leaflet()

m <- leaflet() %>% addTiles()
m  # a map with the default OSM tile layer


# set bounds
m %>% fitBounds(0, 40, 10, 50)

# move the center to Snedecor Hall
m <- m %>% setView(-93.65, 42.0285, zoom = 17)
m

# popup
m %>% addPopups(-93.65, 42.0285, "Here is the <b>Department of Statistics</b>, ISU")
rand_lng <- function(n = 10) rnorm(n, -93.65, .01)
rand_lat <- function(n = 10) rnorm(n, 42.0285, .01)

# use automatic bounds derived from lng/lat data
m <- m %>% clearBounds()

# popup
m %>% addPopups(rand_lng(), rand_lat(), "Random popups")

# marker
m %>% addMarkers(rand_lng(), rand_lat())
m %>% addMarkers(
  rand_lng(), rand_lat(), popup = paste("A random letter", sample(LETTERS, 10))
)

Rlogo <- file.path(R.home("doc"), "html", "logo.jpg")
m %>% addMarkers(
  174.7690922, -36.8523071, icon = list(
    iconUrl = Rlogo, iconSize = c(100, 76)
  ), popup = "R was born here!"
)

m %>% addMarkers(rnorm(30, 175), rnorm(30, -37), icon = list(
  iconUrl = Rlogo, iconSize = c(25, 19)
))

# circle (units in metres)
m %>% addCircles(rand_lng(50), rand_lat(50), radius = runif(50, 50, 150))

# circle marker (units in pixels)
m %>% addCircleMarkers(rand_lng(50), rand_lat(50), color = "#ff0000")
m %>% addCircleMarkers(rand_lng(100), rand_lat(100), radius = runif(100, 5, 15))

# rectangle
m %>% addRectangles(
  rand_lng(), rand_lat(), rand_lng(), rand_lat(),
  color = "red", fill = FALSE, dashArray = "5,5", weight = 3
)

# polyline
m %>% addPolylines(rand_lng(50), rand_lat(50))

# polygon
m %>% addPolygons(rand_lng(), rand_lat(), layerId = "foo")

# geoJSON
seattle_geojson <- list(
  type = "Feature",
  geometry = list(
    type = "MultiPolygon",
    coordinates = list(list(list(
      c(-122.36075812146,  47.6759920119894),
      c(-122.360781646764, 47.6668890126755),
      c(-122.360782108665,  47.6614990696722),
      c(-122.366199035722, 47.6614990696722),
      c(-122.366199035722,  47.6592874248973),
      c(-122.364582509469, 47.6576254522105),
      c(-122.363887331445,  47.6569107302038),
      c(-122.360865528129, 47.6538418253251),
      c(-122.360866157644,  47.6535254473167),
      c(-122.360866581103, 47.6533126275176),
      c(-122.362526540691,  47.6541872926348),
      c(-122.364442114483, 47.6551892850798),
      c(-122.366077719797,  47.6560733960606),
      c(-122.368818463838, 47.6579742346694),
      c(-122.370115159943,  47.6588730808334),
      c(-122.372295967029, 47.6604350102328),
      c(-122.37381369088,  47.660582362063),
      c(-122.375522972109, 47.6606413027949),
      c(-122.376079703095,  47.6608793094619),
      c(-122.376206315662, 47.6609242364243),
      c(-122.377610811371,  47.6606160735197),
      c(-122.379857378879, 47.6610306942278),
      c(-122.382454873022,  47.6627496239169),
      c(-122.385357955057, 47.6638573778241),
      c(-122.386007328104,  47.6640865692306),
      c(-122.387186331506, 47.6654326177161),
      c(-122.387802656231,  47.6661492860294),
      c(-122.388108244121, 47.6664548739202),
      c(-122.389177800763,  47.6663784774359),
      c(-122.390582858689, 47.6665072251861),
      c(-122.390793942299,  47.6659699214511),
      c(-122.391507906234, 47.6659200946229),
      c(-122.392883050767,  47.6664166747017),
      c(-122.392847210144, 47.6678696739431),
      c(-122.392904778401,  47.6709016021624),
      c(-122.39296705153, 47.6732047491624),
      c(-122.393000803496,  47.6759322346303),
      c(-122.37666945305, 47.6759896300663),
      c(-122.376486363943,  47.6759891899754),
      c(-122.366078869215, 47.6759641734893),
      c(-122.36075812146,  47.6759920119894)
    )))
  ),
  properties = list(
    name = "Ballard",
    population = 48000,
    # You can inline styles if you want
    style = list(
      fillColor = "yellow",
      weight = 2,
      color = "#000000"
    )
  ),
  id = "ballard"
)
m %>% setView(-122.36075812146, 47.6759920119894, zoom = 13) %>% addGeoJSON(seattle_geojson)


# use the Dark Matter layer from CartoDB
leaflet() %>% addTiles("https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
                       attribution = paste(
                         "&copy; <a href=\"https://openstreetmap.org\">OpenStreetMap</a> contributors",
                         "&copy; <a href=\"https://cartodb.com/attributions\">CartoDB</a>"
                       )
) %>% setView(-122.36, 47.67, zoom = 10)

# provide a data frame to leaflet()
categories <- LETTERS[1:10]
df <- data.frame(
  lat = rand_lat(100), lng = rand_lng(100), size = runif(100, 5, 20),
  category = factor(sample(categories, 100, replace = TRUE), levels = categories),
  value = rnorm(100)
)
m <- leaflet(df) %>% addTiles()
m %>% addCircleMarkers(~lng, ~lat, radius = ~size)
m %>% addCircleMarkers(~lng, ~lat, radius = runif(100, 4, 10), color = c("red"))

# Discrete colors using the "RdYlBu" colorbrewer palette, mapped to categories
RdYlBu <- colorFactor("RdYlBu", domain = categories)
m %>% addCircleMarkers(~lng, ~lat, radius = ~size,
                       color = ~RdYlBu(category), fillOpacity = 0.5)

# Continuous colors using the "Greens" colorbrewer palette, mapped to value
greens <- colorNumeric("Greens", domain = NULL)
m %>% addCircleMarkers(~lng, ~lat, radius = ~size,
                       color = ~greens(value), fillOpacity = 0.5)


beer2 <- window(ausbeer,start=1992,end=c(2007,4))
# Plot some forecasts
autoplot(beer2) +
  autolayer(meanf(beer2, h=11),
            series="Mean", PI=FALSE) +
  autolayer(naive(beer2, h=11),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(beer2, h=11),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))




adult <- read_csv("adult.csv")
colnames(adult)
p <- ggplot(adult)  

p + 
  geom_bar(aes( x= AGE,color = factor(SEX)), position = "stack") + # add bar geom and use input$categorical_variables as fill 
  theme(legend.position = "bottom")+
  facet_wrap(~PREDICTION)

rm(list =ls(all = TRUE));par(mfrow = c(3,1))
phi.1 = .6 ; phi.2 = .2
data.ts = arima.sim(n=500,list(ar = c (phi.1,phi.2)))
plot(data.ts,main = paste("Autoregressive provess with phi.1=",phi.1,"phi.2=",phi.2))
acf(data.ts,main = "Autocorellation Function")
acf(data.ts,type = "partial",main = "Partail Autocorrelation Function")

library(tidyverse)
glimpse(adult)
adults.ts = ts(adult[,1],start = 1500)
plot(adults.ts,ylab = "Age",main = "Predict Adult outcome per year")
adult.ma =stats::filter(adults.ts,rep(1/31,31),sides = 2)
lines(adult.ma,col = "red")

par(mfrow = c(3,1))
Y = adults.ts/adult.ma
plot(Y,ylab="Scalled age",main = "Transformed adults age data")
acf(na.exclude(Y),main = "Autocorrelation function of Transformed adult age data")
acf(na.exclude(Y),type = "partial", main = "partial Autocorrelation function of Transformed adult data")

library(stats)
ar(na.exclude(Y),order.max = 5)
install.packages('isdals')
library(isdals)
data("bodyfat")
glimpse(bodyfat)
install.packages("ppcor")
library(ppcor)
pcor(cbind(Fat,T))
attach(bodyfat)
pairs(cbind(Fat,Triceps,Thigh,Midarm))
 cor(cbind(Fat,Triceps,Thigh,Midarm))
fat.hat = predict(lm(Fat~Thigh)) 
triceps.hat = predict(lm(Triceps~Thigh))
cor( (Fat - fat.hat),(Triceps - triceps.hat))
library(ppcor)
pcor(cbind(Fat,Triceps,Thigh))
library(astsa)
LakeHuron


