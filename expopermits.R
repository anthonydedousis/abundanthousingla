# source: http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# source for explanation of ZIP code database: https://austinwehrwein.com/digital-humanities/creating-a-density-map-in-r-with-zipcodes/
## Install and set up all the packages

#install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
#install.packages(c("maps", "mapdata")) #standard map packages
#install.packages("zipcode") #install ZIP code to lat-long database
#install.packages("choroplethr") #choropleth maker
#install.packages("devtools")
#install.packages("tigris") #zcta boundaries
#install.packages("viridis") #better color palette
#install.packages("scales") #better labels on legend
#devtools::install_github("dkahle/ggmap") #ggmap, github source
#install.packages("googleway") # interactive google maps
#install.packages("ggpol")
#install.packages("geosphere")

#Load from library
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(stringr)
library(dplyr)
library(devtools)
install_github('arilamstein/choroplethrZip@v1.5.0') #dataframe with ZCTA boundaries
library(choroplethrZip)
library(viridis)
library(scales)
library(googleway)
library(ggpol)
library(geosphere) # computes distances between two sets of coordinates

# import data and set up views we want

setwd("~/Desktop/AHLA Blogging/171227 - Expo Line")
expopermits <- read.csv("expopermits.csv")
expostations <- read.csv("expostations.csv")
library(zipcode)
data(zipcode)

# check out the datasets
head(expopermits)
head(expostations)

# now use ggmap to overlay dots on a Google Map image

#set_key("AIzaSyDML4DP3Z-675erwTe9cxTg3TD5SYwj7rg", api = "geocode")
#set_key("AIzaSyDML4DP3Z-675erwTe9cxTg3TD5SYwj7rg", api = "map")
#set_key("AIzaSyDML4DP3Z-675erwTe9cxTg3TD5SYwj7rg", api = "default")
#google_keys() # check to see which API keys are active
register_google(key = "AIzaSyDML4DP3Z-675erwTe9cxTg3TD5SYwj7rg")

expo_map <- get_map(location = c(lon=-118.43, lat=34.03),  maptype = "roadmap", source = "google", zoom = 13) # map of expo line area
ggmap(expo_map) + geom_point(data = expostations, mapping = aes(x = long, y = lat), size = 3, color = "black") + geom_point(data = expostations, mapping = aes(x = long, y = lat), size = 2, color = "#00FFFF") # add locations of 4 expo line stations
expo1 <- ggmap(expo_map) + geom_point(data = expostations, mapping = aes(x = long, y = lat), size = 3, color = "black") + geom_point(data = expostations, mapping = aes(x = long, y = lat), size = 2, color = "#00FFFF") + geom_text(data = expostations, aes(x = long, y = lat, label = paste("  ", as.character(station), sep="")), size = 3, angle = 30, hjust = 0, color = "black") # add locations of 4 expo line stations, plus labels
expo1 # short name

# now add 800m radii around the Metro stations

make_circles <- function(expostations, radius, nPoints = 100){
  # centers: the data frame of centers with ID
  # radius: radius measured in kilometer
  #
  meanLat <- mean(expostations$lat)
  # length per longitude changes with latitude, so need correction
  radiusLon <- radius /111 / cos(meanLat/57.3) 
  radiusLat <- radius /111
  circleDF <- data.frame(ID = rep(expostations$station, each = nPoints))
  angle <- seq(0,2*pi,length.out = nPoints)
  
  circleDF$lon <- unlist(lapply(expostations$long, function(x) x + radiusLon * cos(angle)))
  circleDF$lat <- unlist(lapply(expostations$lat, function(x) x + radiusLat * sin(angle)))
  return(circleDF)
}

# here is the data frame for all circles
myCircles <- make_circles(expostations, 0.8) # here's where you specify how many km to make the circle's radii
head(myCircles)

# now map the new circles
expo2 <- ggmap(expo_map) + geom_polygon(data = myCircles, aes(x = lon, y = lat, group = ID), fill = NA, color = "black") # draw 0.5 mi circles around Metro stations
expo3 <- expo2 + geom_point(data = expostations, mapping = aes(x = long, y = lat), size = 3, color = "black") + geom_point(data = expostations, mapping = aes(x = long, y = lat), size = 2, color = "#00FFFF") + geom_text(data = expostations, aes(x = long, y = lat, label = paste("  ", as.character(station), sep="")), size = 3, angle = 30, hjust = 0, color = "black") # add locations and labels of 4 expo line stations, plus labels 
expo3
ggsave(filename="expo_radii.png", plot=expo3)

# add distances between permitted locations and nearest Metro station
mat <- distm(expopermits[,c('long','lat')], expostations[,c('long','lat')], fun=distVincentyEllipsoid)
expopermits$locality <- expostations$station[max.col(-mat)] # adds closest Metro station to Permits database
expopermits$near_dist <- apply(mat, 1, min)
tail(expopermits) # check locality and near_dist columns; near_dist is in meters

# add 1-year periods before and after Measure JJJ (9/22/17)
class(expopermits$date) # the date column is a factor right now
expopermits$date <- as.Date(expopermits$date, format = "%m/%d/%Y") # convert to a date
class(expopermits$date) # now it's a date
expopermits$jjj <- ifelse(expopermits$date <= as.Date('2016-09-21'), "1+ yr Before JJJ", 
                          ifelse((expopermits$date > as.Date('2016-09-21') & expopermits$date <= as.Date('2017-09-21')), "<1 yr Before JJJ", 
                                 ifelse((expopermits$date > as.Date('2017-09-21') & expopermits$date <= as.Date('2018-09-21')), "<1 yr After JJJ", "1+ yr After JJJ")))
tail(expopermits)

# create aggregated building permit dataframe by RDU count, size, year, period, zip, nearest metro station, distance to nearest metro station, JJJ status
expopermits_summary <- expopermits[c(2,3,7,8,12,22,23,24)]
head(expopermits_summary)
sum(expopermits_summary$rdus) # should be 3,585

expopermits_summary$within_walking <- ifelse(expopermits_summary$near_dist<=800, "<0.5 mi", ">0.5 mi")
head(expopermits_summary)

expopermits_agg <- expopermits_summary %>%
  group_by(size, year, period, locality, within_walking,jjj) %>%
  summarize(rdu_total = sum(rdus))
head(expopermits_agg)
nrow(expopermits_agg)
write.csv(expopermits_agg, "expopermits_agg.csv")

# create map of the Metro stations east of Phase II

expopermits2 <- read.csv("expopermitsphase1.csv")
expostations2 <- read.csv("expostationsphase1.csv")
head(expopermits2)
head(expostations2)

expo_map2 <- get_map(location = c(lon=-118.35, lat=34.03),  maptype = "roadmap", source = "google", zoom = 13) # map of expo line area
ggmap(expo_map2) + geom_point(data = expostations2, mapping = aes(x = long, y = lat), size = 3, color = "black") + geom_point(data = expostations2, mapping = aes(x = long, y = lat), size = 2, color = "#00FFFF") # add locations of 4 expo line stations
expo4 <- ggmap(expo_map2) + geom_point(data = expostations2, mapping = aes(x = long, y = lat), size = 3, color = "black") + geom_point(data = expostations2, mapping = aes(x = long, y = lat), size = 2, color = "#00FFFF") + geom_text(data = expostations2, aes(x = long, y = lat, label = paste("  ", as.character(station), sep="")), size = 3, angle = 30, hjust = 0, color = "black") # add locations of 4 expo line stations, plus labels
expo4 # short name

# now add 800m radii around the Metro stations

make_circles2 <- function(expostations2, radius, nPoints = 100){
  # centers: the data frame of centers with ID
  # radius: radius measured in kilometer
  #
  meanLat2 <- mean(expostations2$lat)
  # length per longitude changes with latitude, so need correction
  radiusLon <- radius /111 / cos(meanLat/57.3) 
  radiusLat <- radius /111
  circleDF2 <- data.frame(ID = rep(expostations2$station, each = nPoints))
  angle <- seq(0,2*pi,length.out = nPoints)
  
  circleDF2$lon <- unlist(lapply(expostations2$long, function(x) x + radiusLon * cos(angle)))
  circleDF2$lat <- unlist(lapply(expostations2$lat, function(x) x + radiusLat * sin(angle)))
  return(circleDF2)
}

# here is the data frame for all circles
myCircles2 <- make_circles(expostations2, 0.8) # here's where you specify how many km to make the circle's radii
head(myCircles2)

# now map the new circles
expo5 <- ggmap(expo_map2) + geom_polygon(data = myCircles2, aes(x = lon, y = lat, group = ID), fill = NA, color = "black") # draw 0.5 mi circles around Metro stations
expo6 <- expo5 + geom_point(data = expostations2, mapping = aes(x = long, y = lat), size = 3, color = "black") + geom_point(data = expostations2, mapping = aes(x = long, y = lat), size = 2, color = "#00FFFF") + geom_text(data = expostations2, aes(x = long, y = lat, label = paste("  ", as.character(station), sep="")), size = 3, angle = 30, hjust = 0, color = "black") # add locations and labels of 4 expo line stations, plus labels 
expo6
ggsave(filename="expo_radii_east.png", plot=expo6)

# add distances between permitted locations and nearest Metro station

mat2 <- distm(expopermits2[,c('long','lat')], expostations2[,c('long','lat')], fun=distVincentyEllipsoid)
expopermits2$locality <- expostations2$station[max.col(-mat2)] # adds closest Metro station to Permits database
expopermits2$near_dist <- apply(mat2, 1, min)
tail(expopermits2) # check locality and near_dist columns; near_dist is in meters

# add 1-year periods before and after Measure JJJ (9/22/17)
class(expopermits2$date) # the date column is a factor right now
expopermits2$date <- as.Date(expopermits2$date, format = "%m/%d/%Y") # convert to a date
class(expopermits2$date) # now it's a date
expopermits2$jjj <- ifelse(expopermits2$date <= as.Date('2016-09-21'), "1+ yr Before JJJ", 
                          ifelse((expopermits2$date > as.Date('2016-09-21') & expopermits2$date <= as.Date('2017-09-21')), "<1 yr Before JJJ", 
                                 ifelse((expopermits2$date > as.Date('2017-09-21') & expopermits2$date <= as.Date('2018-09-21')), "<1 yr After JJJ", "1+ yr After JJJ")))
tail(expopermits2)

# create aggregated building permit dataframe by RDU count, size, year, period, zip, nearest metro station, distance to nearest metro station, JJJ status
expopermits2_summary <- expopermits2[c(2,3,7,8,12,22,23,24)]
head(expopermits2_summary)
sum(expopermits2_summary$rdus) # should be 4,248

expopermits2_summary$within_walking <- ifelse(expopermits2_summary$near_dist<=800, "<0.5 mi", ">0.5 mi")
head(expopermits2_summary)

expopermits2_agg <- expopermits2_summary %>%
  group_by(size, year, period, locality, within_walking,jjj) %>%
  summarize(rdu_total = sum(rdus))
head(expopermits2_agg)
nrow(expopermits2_agg)
write.csv(expopermits2_agg, "expopermits_agg2.csv")





# now, shade the radius map to show count of homes in radii post-Expo Line opening

###???????
ggmap(expo3) +
  geom_sf(aes(fill = cut(rdu_total,c(0,100,500,1000,5000,10000), include.lowest = F, dig.lab=10)), data = permits3, lwd = 0.05, #omg yes lwd draws the lines, finally
          colour = 'black', inherit.aes = F, size = 0, alpha = 0.5) +
  coord_sf(ndiscr = F) +
  scale_fill_brewer(name = "RDUs Permitted", palette=8) +
  theme_bw()