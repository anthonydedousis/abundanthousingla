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
#install.packages("ggrepel")
#install.packages("shades")

## Load from library

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
library(ggrepel) # makes it easier to add labels
library(rgdal) # use to upload shapefiles
library(raster) # maybe a different way to upload shapefiles
library(cowplot) # allows you to plot multiple plots side by side
library(shades) # lets you make color palettes lighter/darker

## import data and set up views we want

setwd("~/Documents/Abundant Housing LA/1906xx - TOC") # you'll need to change this
metrorail <- read.csv("metrorail_with_purple.csv")
toc <- read.csv("toc.csv")
head(metrorail)
head(toc)

## import LA neighborhood boundaries

file.exists("LA_Times_Neighborhoods.shp")
neighborhood <- readOGR(dsn = "~/Documents/Abundant Housing LA/1906xx - TOC/LA_Times_Neighborhoods", layer = "LA_Times_Neighborhoods") # you'll need to change this
#you will have to save the 6 LA_Times_Neighborhoods files to a folder called "LA_Times_Neighborhoods" in your working directory
head(neighborhood)
class(neighborhood)
nh_df <- broom::tidy(neighborhood, region = "name") # turn into a dataframe
lapply(nh_df, class)
head(nh_df)
class(nh_df)
nh_names <- aggregate(cbind(long, lat) ~ id, data=nh_df, FUN=mean) # create dataframe with names of neighborhoods
head(nh_names)

## now use ggmap to make a map of Los Angeles

#set_key("AIzaSyDML4DP3Z-675erwTe9cxTg3TD5SYwj7rg", api = "geocode")
#set_key("AIzaSyDML4DP3Z-675erwTe9cxTg3TD5SYwj7rg", api = "map")
#set_key("AIzaSyDML4DP3Z-675erwTe9cxTg3TD5SYwj7rg", api = "default")
#google_keys() # check to see which API keys are active
register_google(key = "xxx") # get a Google Map API key please :) or find an open-source map

la_map <- get_map(location = c(lon=-118.35, lat=34.1),  maptype = "roadmap", source = "google", zoom = 11) # map of los angeles
ggmap(la_map)

la_map2 <- get_map(location = c(lon=-118.31, lat=34.0),  maptype = "roadmap", source = "google", zoom = 10) # map of los angeles
ggmap(la_map2)

la_map_zoom <- ggmap(la_map) +
  coord_fixed(xlim = c(-118.52, -118.12), ylim = c(33.8, 34.2), ratio = 1.17)
la_map_zoom # this zoom will avoid neighborhood boundary problems

## now add neighborhood boundaries

la_map_nhood <- ggmap(la_map2) + 
  geom_polygon(data = nh_df, aes(x = long, y = lat, group = group), fill = NA, linetype = "21", size = 0.2, color = "black") 
la_map_nhood

la_map_zoom_nhood <- la_map_nhood +
  coord_fixed(xlim = c(-118.55, -118.15), ylim = c(33.9, 34.3), ratio = 1.17) 
la_map_zoom_nhood # better, but labels and highway signs are too large

## now use ggmap to overlay Metrorail locations on a Google Map image, including 3 new Purple Line stations

metro_map <- ggmap(la_map) + 
  geom_path(data = subset(metrorail, expo == "Yes"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, blue == "Yes" & expo == "No" & green == "No"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, red == "Yes"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, green == "Yes" & blue == "No"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, gold == "Yes" & red == "No"), aes(x = long, y = lat)) +
  geom_line(data = subset(metrorail, purple == "Yes"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, crenshaw == "Yes"), aes(x = long, y = lat)) +
  geom_point(data = subset(metrorail, line == "expo"), mapping = aes(x = long, y = lat), size = 2, color = "#00FFFF") + # expo line stations
  geom_point(data = subset(metrorail, line == "red"), mapping = aes(x = long, y = lat), size = 2, color = "#FF0000") + # red line stations
  geom_point(data = subset(metrorail, line == "gold"), mapping = aes(x = long, y = lat), size = 2, color = "#DAA520") +  # gold line stations
  geom_point(data = subset(metrorail, line == "green"), mapping = aes(x = long, y = lat), size = 2, color = "#32CD32") +  # green line stations
  geom_point(data = subset(metrorail, line == "blue"), mapping = aes(x = long, y = lat), size = 2, color = "#1E90FF") +  # blue line stations
  geom_point(data = subset(metrorail, line == "purple"), mapping = aes(x = long, y = lat), size = 2, color = "#4B0082") + # purple line stations
  geom_point(data = subset(metrorail, line == "crenshaw"), mapping = aes(x = long, y = lat), size = 2, color = "#8B4513") + # crenshaw line stations
  geom_point(data = subset(metrorail, line == "multi"), mapping = aes(x = long, y = lat), size = 2, color = "#696969") + # >2 line transfer stations
  geom_point(data = subset(metrorail, line == "purplered"), mapping = aes(x = long, y = lat), size = 3, color = "#4B0082") + # purple/red line stations
  geom_point(data = subset(metrorail, line == "purplered"), mapping = aes(x = long, y = lat), size = 2, color = "#FF0000") +  # purple/red line stations
  geom_point(data = subset(metrorail, line == "bluegreen"), mapping = aes(x = long, y = lat), size = 3, color = "#32CD32") +  # green/blue line stations
  geom_point(data = subset(metrorail, line == "bluegreen"), mapping = aes(x = long, y = lat), size = 2, color = "#1E90FF") + # green/blue line stations
  geom_point(data = subset(metrorail, line == "greencrenshaw"), mapping = aes(x = long, y = lat), size = 3, color = "#8B4513") + # green/crenshaw line stations
  geom_point(data = subset(metrorail, line == "greencrenshaw"), mapping = aes(x = long, y = lat), size = 2, color = "#32CD32") +  # green/crenshaw line stations
  geom_point(data = subset(metrorail, line == "expocrenshaw"), mapping = aes(x = long, y = lat), size = 3, color = "#8B4513") + # expo/crenshaw line stations
  geom_point(data = subset(metrorail, line == "expocrenshaw"), mapping = aes(x = long, y = lat), size = 2, color = "#00FFFF") + # expo/crenshaw line stations
  geom_point(data = subset(metrorail, line == "expoblue"), mapping = aes(x = long, y = lat), size = 3, color = "#00FFFF") + # expo/blue line stations
  geom_point(data = subset(metrorail, line == "expoblue"), mapping = aes(x = long, y = lat), size = 2, color = "#1E90FF")   # expo/blue line stations
metro_map

metro_map2 <- ggmap(la_map2) + 
  geom_path(data = subset(metrorail, expo == "Yes"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, blue == "Yes" & expo == "No" & green == "No"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, red == "Yes"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, green == "Yes" & blue == "No"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, gold == "Yes" & red == "No"), aes(x = long, y = lat)) +
  geom_line(data = subset(metrorail, purple == "Yes"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, crenshaw == "Yes"), aes(x = long, y = lat)) +
  geom_point(data = subset(metrorail, line == "expo"), mapping = aes(x = long, y = lat), size = 2, color = "#00FFFF") + # expo line stations
  geom_point(data = subset(metrorail, line == "red"), mapping = aes(x = long, y = lat), size = 2, color = "#FF0000") + # red line stations
  geom_point(data = subset(metrorail, line == "gold"), mapping = aes(x = long, y = lat), size = 2, color = "#DAA520") +  # gold line stations
  geom_point(data = subset(metrorail, line == "green"), mapping = aes(x = long, y = lat), size = 2, color = "#32CD32") +  # green line stations
  geom_point(data = subset(metrorail, line == "blue"), mapping = aes(x = long, y = lat), size = 2, color = "#1E90FF") +  # blue line stations
  geom_point(data = subset(metrorail, line == "purple"), mapping = aes(x = long, y = lat), size = 2, color = "#4B0082") + # purple line stations
  geom_point(data = subset(metrorail, line == "crenshaw"), mapping = aes(x = long, y = lat), size = 2, color = "#8B4513") + # crenshaw line stations
  geom_point(data = subset(metrorail, line == "multi"), mapping = aes(x = long, y = lat), size = 2, color = "#696969") + # >2 line transfer stations
  geom_point(data = subset(metrorail, line == "purplered"), mapping = aes(x = long, y = lat), size = 3, color = "#4B0082") + # purple/red line stations
  geom_point(data = subset(metrorail, line == "purplered"), mapping = aes(x = long, y = lat), size = 2, color = "#FF0000") +  # purple/red line stations
  geom_point(data = subset(metrorail, line == "bluegreen"), mapping = aes(x = long, y = lat), size = 3, color = "#32CD32") +  # green/blue line stations
  geom_point(data = subset(metrorail, line == "bluegreen"), mapping = aes(x = long, y = lat), size = 2, color = "#1E90FF") + # green/blue line stations
  geom_point(data = subset(metrorail, line == "greencrenshaw"), mapping = aes(x = long, y = lat), size = 3, color = "#8B4513") + # green/crenshaw line stations
  geom_point(data = subset(metrorail, line == "greencrenshaw"), mapping = aes(x = long, y = lat), size = 2, color = "#32CD32") +  # green/crenshaw line stations
  geom_point(data = subset(metrorail, line == "expocrenshaw"), mapping = aes(x = long, y = lat), size = 3, color = "#8B4513") + # expo/crenshaw line stations
  geom_point(data = subset(metrorail, line == "expocrenshaw"), mapping = aes(x = long, y = lat), size = 2, color = "#00FFFF") + # expo/crenshaw line stations
  geom_point(data = subset(metrorail, line == "expoblue"), mapping = aes(x = long, y = lat), size = 3, color = "#00FFFF") + # expo/blue line stations
  geom_point(data = subset(metrorail, line == "expoblue"), mapping = aes(x = long, y = lat), size = 2, color = "#1E90FF")   # expo/blue line stations
metro_map2

# make combined map of Metro and neighborhood boundaries
metro_map3 <- la_map_nhood + 
  geom_path(data = subset(metrorail, expo == "Yes"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, blue == "Yes" & expo == "No" & green == "No"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, red == "Yes"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, green == "Yes" & blue == "No"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, gold == "Yes" & red == "No"), aes(x = long, y = lat)) +
  geom_line(data = subset(metrorail, purple == "Yes"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, crenshaw == "Yes"), aes(x = long, y = lat)) +
  geom_point(data = subset(metrorail, line == "expo"), mapping = aes(x = long, y = lat), size = 2, color = "#00FFFF") + # expo line stations
  geom_point(data = subset(metrorail, line == "red"), mapping = aes(x = long, y = lat), size = 2, color = "#FF0000") + # red line stations
  geom_point(data = subset(metrorail, line == "gold"), mapping = aes(x = long, y = lat), size = 2, color = "#DAA520") +  # gold line stations
  geom_point(data = subset(metrorail, line == "green"), mapping = aes(x = long, y = lat), size = 2, color = "#32CD32") +  # green line stations
  geom_point(data = subset(metrorail, line == "blue"), mapping = aes(x = long, y = lat), size = 2, color = "#1E90FF") +  # blue line stations
  geom_point(data = subset(metrorail, line == "purple"), mapping = aes(x = long, y = lat), size = 2, color = "#4B0082") + # purple line stations
  geom_point(data = subset(metrorail, line == "crenshaw"), mapping = aes(x = long, y = lat), size = 2, color = "#8B4513") + # crenshaw line stations
  geom_point(data = subset(metrorail, line == "multi"), mapping = aes(x = long, y = lat), size = 2, color = "#696969") + # >2 line transfer stations
  geom_point(data = subset(metrorail, line == "purplered"), mapping = aes(x = long, y = lat), size = 3, color = "#4B0082") + # purple/red line stations
  geom_point(data = subset(metrorail, line == "purplered"), mapping = aes(x = long, y = lat), size = 2, color = "#FF0000") +  # purple/red line stations
  geom_point(data = subset(metrorail, line == "bluegreen"), mapping = aes(x = long, y = lat), size = 3, color = "#32CD32") +  # green/blue line stations
  geom_point(data = subset(metrorail, line == "bluegreen"), mapping = aes(x = long, y = lat), size = 2, color = "#1E90FF") + # green/blue line stations
  geom_point(data = subset(metrorail, line == "greencrenshaw"), mapping = aes(x = long, y = lat), size = 3, color = "#8B4513") + # green/crenshaw line stations
  geom_point(data = subset(metrorail, line == "greencrenshaw"), mapping = aes(x = long, y = lat), size = 2, color = "#32CD32") +  # green/crenshaw line stations
  geom_point(data = subset(metrorail, line == "expocrenshaw"), mapping = aes(x = long, y = lat), size = 3, color = "#8B4513") + # expo/crenshaw line stations
  geom_point(data = subset(metrorail, line == "expocrenshaw"), mapping = aes(x = long, y = lat), size = 2, color = "#00FFFF") + # expo/crenshaw line stations
  geom_point(data = subset(metrorail, line == "expoblue"), mapping = aes(x = long, y = lat), size = 3, color = "#00FFFF") + # expo/blue line stations
  geom_point(data = subset(metrorail, line == "expoblue"), mapping = aes(x = long, y = lat), size = 2, color = "#1E90FF")   # expo/blue line stations
metro_map3

## add 0.5 mile radii around the stations

make_circles <- function(metrorail, radius, nPoints = 100){
  # centers: the data frame of centers with ID
  # radius: radius measured in kilometer
  #
  meanLat <- mean(metrorail$lat)
  # length per longitude changes with latitude, so need correction
  radiusLon <- radius /111 / cos(meanLat/57.3) 
  radiusLat <- radius /111
  circleDF <- data.frame(ID = rep(metrorail$station, each = nPoints))
  angle <- seq(0,2*pi,length.out = nPoints)
  
  circleDF$lon <- unlist(lapply(metrorail$long, function(x) x + radiusLon * cos(angle)))
  circleDF$lat <- unlist(lapply(metrorail$lat, function(x) x + radiusLat * sin(angle)))
  return(circleDF)
}

# here is the data frame for all circles
PLRadii <- make_circles(metrorail, 0.8) # 0.8 is how many km to make the circle's radii
tail(PLRadii)

# now map the city with Metro stations and radii
metro_map_radii <- metro_map + 
  geom_polygon(data = PLRadii, aes(x = lon, y = lat, group = ID), fill = NA, color = "black") # draw 0.5 mi circles around Metro stations
metro_map_radii

# plot the locations of the TOC projects on the map, where colors are number of units

toc <- subset(toc, duplicate == "No") # delete projects that appears as both discretionary and by-right
tail(toc)
sum(toc$units) # should total 12,277 units

toc_map1 <- ggmap(la_map2) +
  geom_point(aes(x = long, y = lat, color = cut(units,c(0,20,50,100,200,500), 
          c("1-20 units", "21-50 units", "51-100 units", "100-199 units", "200+ units"),
          include.lowest = F, dig.lab=10)), data = toc,
          inherit.aes = T, size = 2, alpha = 0.5) +
  scale_color_brewer(name = "Units", palette="Purples", na.translate = F) +
  theme_bw()
toc_map1 # good, but let's try to find a way to use the 5 darkest purples only.

toc_map_closeup <- ggmap(la_map) +
  geom_point(aes(x = long, y = lat, color = cut(units,c(0,20,50,100,200,500), 
                                                c("1-20 units", "21-50 units", "51-100 units", "100-199 units", "200+ units"),
                                                include.lowest = F, dig.lab=10)), data = toc,
             inherit.aes = T, size = 2, alpha = 0.5) +
  scale_color_brewer(name = "Units", palette="Purples", na.translate = F) +
  theme_bw()
toc_map_closeup # good, but let's try to find a way to use the 5 darkest purples only.

# plot TOC projects with neighborhood boundaries

toc_map2 <- la_map_nhood +
  geom_point(aes(x = long, y = lat, color = cut(units,c(0,20,50,100,200,500), 
                                                c("1-20 units", "21-50 units", "51-100 units", "100-199 units", "200+ units"),
                                                include.lowest = F, dig.lab=10)), data = toc,
             inherit.aes = T, size = 2, alpha = 0.5) +
  scale_color_brewer(name = "Units", palette="Purples", na.translate = F) +
  theme_bw()
toc_map2 # boundaries add a little too much confusion.

# add distances between permitted locations and nearest Metro station

mat <- distm(toc[,c('long','lat')], metrorail[,c('long','lat')], fun=distVincentyEllipsoid)
toc$nearest_metro <- metrorail$station[max.col(-mat)] # adds closest Metro station to TOC project
toc$dist_to_metro <- apply(mat, 1, min)
toc$within_halfmile <- ifelse(toc$dist_to_metro<=800, "<0.5 mi", ">0.5 mi")
tail(toc) # check locality and near_dist columns; near_dist is in meters

toc_by_station <- toc %>%
  group_by(nearest_metro, within_halfmile) %>%
  summarize(units = sum(units))

head(toc_by_station)
nrow(toc_by_station)
sum(toc_by_station$units) # should be 12,277
head(metrorail)

# aggregate projects by neighborhood
coords <- toc[c('long','lat')]
head(coords)
coords <- data.frame(coords)
sp <- SpatialPoints(coords)
proj4string(sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(sp)
by_neighborhood <- over(sp, neighborhood) # creates a dataframe of TOC coordinates and neighborhood shapefile

by_nh_summary <- by_neighborhood %>%
  group_by(name) %>%
  summarise(total=n()) # count the number of TOC projects by neighborhood
head(by_nh_summary)
class(by_nh_summary)
sum(by_nh_summary$total) # should be 275

## combine TOC coordinates + neighborhood dataframe with TOC data

coords_with_nh <- cbind(coords, by_neighborhood$name)
colnames(coords_with_nh)[3] <- "neighborhood" # rename column
head(coords_with_nh)

toc_combined <- cbind(toc, coords_with_nh)
toc_combined <- toc_combined[-c(26,30,31)] # drop duplicate lat-long
head(toc_combined)
sum(toc_combined$units) # again, should be 12,277

# create a dataframe that totals TOC units, affordable units, Metro-adjacent units by neighborhood

toc_by_nh <- toc_combined %>%
  group_by(neighborhood) %>%
  summarise(total_units=sum(units), market_rate_units=sum(market_rate), affordable_units=sum(total_affordable), affordable_pct=(affordable_units/total_units)) %>%
  arrange(desc(total_units))
head(toc_by_nh) # Koreatown has 2,296 units; Westlake has 1,268 units, Palms has 708, Sawtelle has 609
# this matches the LA Times' count: https://www.latimes.com/business/la-fi-affordable-housing-transit-zoning-20190526-story.html
nrow(toc_by_nh)

# add columns that match neighborhoods to broader regions
id_names <- read.csv("name_match.csv")
head(id_names)
id_names <- id_names[c(1,3,4)]
id_names$id <- as.character(id_names$id)
colnames(id_names)[1] <- "neighborhood"
toc_by_nh <- toc_by_nh %>%
  inner_join(id_names, by = "neighborhood")
toc_by_nh <- toc_by_nh[c(1,6,2,3,4,5)]
head(toc_by_nh)

# create dataframe that totals number of TOC units near Metro by neighborhood
toc_near_metro <- subset(toc_combined, within_halfmile == "<0.5 mi")
sum(toc_near_metro$units) # 5,127 units are near rail (42% of total)
toc_near_metro <- toc_near_metro %>%
  inner_join(id_names, by = "neighborhood") # add major_area and minor_area to dataset
head(toc_near_metro)
write.csv(toc_near_metro, "toc_near_metro.csv")

toc_by_metro_and_nh <- toc_near_metro %>%
  group_by(neighborhood) %>%
  summarise(units_near_metro=sum(units)) %>%
  arrange(desc(units_near_metro))
head(toc_by_metro_and_nh) # Koreatown should have 1,978 units near Metro

toc_by_nh <- toc_by_nh %>% left_join(toc_by_metro_and_nh, by = "neighborhood") # join the data frames; left join includes boundaries neighborhoods with zero TOC units
toc_by_nh[is.na(toc_by_nh)] <- 0 # set NAs as 0
head(toc_by_nh)
toc_by_nh$metro_pct <- (toc_by_nh$units_near_metro/toc_by_nh$total_units)
write.csv(toc_by_nh, "toc_by_nh.csv")

class(toc_by_nh$major_area)
toc_by_nh$major_area <- as.character(toc_by_nh$major_area)
toc_by_region <- toc_by_nh %>%
  group_by(major_area) %>%
  summarise(units=sum(total_units)) %>%
  arrange(desc(units))
head(toc_by_region)

## combine neighborhood shapefile with TOC summary dataset
head(nh_df)
colnames(nh_df)[7] <- "neighborhood"
combined <- nh_df %>% left_join(toc_by_nh, by = "neighborhood") # join the data frames; left join includes boundaries neighborhoods with zero TOC units
head(combined)
#combined[is.na(combined)] <- 0 # set NAs as 0
write.csv(combined, "combined.csv")

## heat map of neighborhoods by number of TOC units

toc_map3 <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
        fill = cut(total_units, c(0,1,100,200,500,1000,2000,5000), 
                   c("0 units", "1-99 units", "100-199 units", "200-499 units", "500-999 units", "1,000-1,999 units", "2,000+ units"),
                   include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "TOC Units by Neighborhood", palette="YlGnBu", na.value="#D0D3D4") 
toc_map3

## close-up of heat map by number of TOC units

toc_map3a <- la_map_zoom_nhood + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(total_units, c(0,1,100,200,500,1000,2000,5000), 
                                               c("0 units", "1-99 units", "100-199 units", "200-499 units", "500-999 units", "1,000-1,999 units", "2,000+ units"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  #geom_text(data = nh_names, aes(x = long, y = lat, label = id), size = 2) +
scale_fill_brewer(name = "TOC Units by Neighborhood", palette="YlGnBu", na.value="#D0D3D4") 
toc_map3a


## heat map of neighborhoods by number of affordable units

toc_map4 <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(affordable_units, c(0,1,50,100,200,500), 
                                               c("Not Known", "1-49 units", "50-99 units","100-199 units", "200+ units"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "Affordable TOC Units", palette="YlGnBu", na.value="#D0D3D4") 
toc_map4

toc_map4a <- la_map_zoom_nhood + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(affordable_units, c(0,1,50, 100,200,500), 
                                               c("Not Known", "1-49 units", "50-99 units","100-199 units", "200+ units"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "Affordable TOC Units", palette="YlGnBu", na.value="#D0D3D4") 
toc_map4a

## heat map of neighborhoods by % units that are affordable

toc_map5 <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(affordable_pct, c(0,.001,0.1,0.2,0.5,1), 
                                               c("Not Known", "0-10%", "10-20%", "20%-50%","50+%"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "% Affordable TOC Units", palette="YlGnBu", na.value="#D0D3D4") 
toc_map5

toc_map5a <- la_map_zoom_nhood + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(affordable_pct, c(0,.001,0.1,0.2,0.5,1), 
                                               c("Not Known", "0-10%", "10-20%", "20%-50%","50+%"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "% Affordable TOC Units", palette="YlGnBu", na.value="#D0D3D4") 
toc_map5a

## heat map of neighborhoods by number of Metro-adjacent units

toc_map6 <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(units_near_metro, c(0,1,100,200,500,1000,2000,5000), 
                                               c("0 units", "1-99 units", "100-199 units", "200-499 units", "500-999 units", "1,000-1,999 units", "2,000+ units"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "Metro-Adjacent TOC Units", palette="YlGnBu", na.value="#D0D3D4") 
toc_map6

metro_map3a <- toc_map6 + 
  geom_path(data = subset(metrorail, expo == "Yes"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, blue == "Yes" & expo == "No" & green == "No"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, red == "Yes"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, green == "Yes" & blue == "No"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, gold == "Yes" & red == "No"), aes(x = long, y = lat)) +
  geom_line(data = subset(metrorail, purple == "Yes"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, crenshaw == "Yes"), aes(x = long, y = lat)) +
  geom_point(data = subset(metrorail, line == "expo"), mapping = aes(x = long, y = lat), size = 2, color = "#00FFFF") + # expo line stations
  geom_point(data = subset(metrorail, line == "red"), mapping = aes(x = long, y = lat), size = 2, color = "#FF0000") + # red line stations
  geom_point(data = subset(metrorail, line == "gold"), mapping = aes(x = long, y = lat), size = 2, color = "#DAA520") +  # gold line stations
  geom_point(data = subset(metrorail, line == "green"), mapping = aes(x = long, y = lat), size = 2, color = "#32CD32") +  # green line stations
  geom_point(data = subset(metrorail, line == "blue"), mapping = aes(x = long, y = lat), size = 2, color = "#1E90FF") +  # blue line stations
  geom_point(data = subset(metrorail, line == "purple"), mapping = aes(x = long, y = lat), size = 2, color = "#4B0082") + # purple line stations
  geom_point(data = subset(metrorail, line == "crenshaw"), mapping = aes(x = long, y = lat), size = 2, color = "#8B4513") + # crenshaw line stations
  geom_point(data = subset(metrorail, line == "multi"), mapping = aes(x = long, y = lat), size = 2, color = "#696969") + # >2 line transfer stations
  geom_point(data = subset(metrorail, line == "purplered"), mapping = aes(x = long, y = lat), size = 3, color = "#4B0082") + # purple/red line stations
  geom_point(data = subset(metrorail, line == "purplered"), mapping = aes(x = long, y = lat), size = 2, color = "#FF0000") +  # purple/red line stations
  geom_point(data = subset(metrorail, line == "bluegreen"), mapping = aes(x = long, y = lat), size = 3, color = "#32CD32") +  # green/blue line stations
  geom_point(data = subset(metrorail, line == "bluegreen"), mapping = aes(x = long, y = lat), size = 2, color = "#1E90FF") + # green/blue line stations
  geom_point(data = subset(metrorail, line == "greencrenshaw"), mapping = aes(x = long, y = lat), size = 3, color = "#8B4513") + # green/crenshaw line stations
  geom_point(data = subset(metrorail, line == "greencrenshaw"), mapping = aes(x = long, y = lat), size = 2, color = "#32CD32") +  # green/crenshaw line stations
  geom_point(data = subset(metrorail, line == "expocrenshaw"), mapping = aes(x = long, y = lat), size = 3, color = "#8B4513") + # expo/crenshaw line stations
  geom_point(data = subset(metrorail, line == "expocrenshaw"), mapping = aes(x = long, y = lat), size = 2, color = "#00FFFF") + # expo/crenshaw line stations
  geom_point(data = subset(metrorail, line == "expoblue"), mapping = aes(x = long, y = lat), size = 3, color = "#00FFFF") + # expo/blue line stations
  geom_point(data = subset(metrorail, line == "expoblue"), mapping = aes(x = long, y = lat), size = 2, color = "#1E90FF")   # expo/blue line stations
metro_map3a

## heat map of neighborhoods by % units that are Metro-adjacent

toc_map7 <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(metro_pct, c(0,.001,0.2,0.4,0.6,0.8,1), 
                                               c("0%", "0-20%", "20-40%", "40-60%","60-80%","80+%"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "% TOC Units Near Metro", palette = "Blues", na.value="#D0D3D4") 
toc_map7

## heat map with Metro lines

metro_map4 <- toc_map7 + 
  geom_path(data = subset(metrorail, expo == "Yes"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, blue == "Yes" & expo == "No" & green == "No"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, red == "Yes"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, green == "Yes" & blue == "No"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, gold == "Yes" & red == "No"), aes(x = long, y = lat)) +
  geom_line(data = subset(metrorail, purple == "Yes"), aes(x = long, y = lat)) +
  geom_path(data = subset(metrorail, crenshaw == "Yes"), aes(x = long, y = lat)) +
  geom_point(data = subset(metrorail, line == "expo"), mapping = aes(x = long, y = lat), size = 2, color = "#00FFFF") + # expo line stations
  geom_point(data = subset(metrorail, line == "red"), mapping = aes(x = long, y = lat), size = 2, color = "#FF0000") + # red line stations
  geom_point(data = subset(metrorail, line == "gold"), mapping = aes(x = long, y = lat), size = 2, color = "#DAA520") +  # gold line stations
  geom_point(data = subset(metrorail, line == "green"), mapping = aes(x = long, y = lat), size = 2, color = "#32CD32") +  # green line stations
  geom_point(data = subset(metrorail, line == "blue"), mapping = aes(x = long, y = lat), size = 2, color = "#1E90FF") +  # blue line stations
  geom_point(data = subset(metrorail, line == "purple"), mapping = aes(x = long, y = lat), size = 2, color = "#4B0082") + # purple line stations
  geom_point(data = subset(metrorail, line == "crenshaw"), mapping = aes(x = long, y = lat), size = 2, color = "#8B4513") + # crenshaw line stations
  geom_point(data = subset(metrorail, line == "multi"), mapping = aes(x = long, y = lat), size = 2, color = "#696969") + # >2 line transfer stations
  geom_point(data = subset(metrorail, line == "purplered"), mapping = aes(x = long, y = lat), size = 3, color = "#4B0082") + # purple/red line stations
  geom_point(data = subset(metrorail, line == "purplered"), mapping = aes(x = long, y = lat), size = 2, color = "#FF0000") +  # purple/red line stations
  geom_point(data = subset(metrorail, line == "bluegreen"), mapping = aes(x = long, y = lat), size = 3, color = "#32CD32") +  # green/blue line stations
  geom_point(data = subset(metrorail, line == "bluegreen"), mapping = aes(x = long, y = lat), size = 2, color = "#1E90FF") + # green/blue line stations
  geom_point(data = subset(metrorail, line == "greencrenshaw"), mapping = aes(x = long, y = lat), size = 3, color = "#8B4513") + # green/crenshaw line stations
  geom_point(data = subset(metrorail, line == "greencrenshaw"), mapping = aes(x = long, y = lat), size = 2, color = "#32CD32") +  # green/crenshaw line stations
  geom_point(data = subset(metrorail, line == "expocrenshaw"), mapping = aes(x = long, y = lat), size = 3, color = "#8B4513") + # expo/crenshaw line stations
  geom_point(data = subset(metrorail, line == "expocrenshaw"), mapping = aes(x = long, y = lat), size = 2, color = "#00FFFF") + # expo/crenshaw line stations
  geom_point(data = subset(metrorail, line == "expoblue"), mapping = aes(x = long, y = lat), size = 3, color = "#00FFFF") + # expo/blue line stations
  geom_point(data = subset(metrorail, line == "expoblue"), mapping = aes(x = long, y = lat), size = 2, color = "#1E90FF")   # expo/blue line stations
metro_map4

## create interactive map

# maps to include:
toc_map1 # map with individual TOC projects, where color = # of units
toc_map3 # map with # of TOC units by neighborhood
toc_map4 # map with # of affordable TOC units by neighborhood
toc_map5 # map with % of affordable TOC units by neighborhood
metro_map3a # map with # of Metro-adjacent TOC units by neighborhood
metro_map4 # map with % of Metro-adjacent TOC units by neighborhood

# now how to turn these into RShiny interactive maps?
install.packages("rmarkdown")
library(rmarkdown)
install.packages("shiny")
library(shiny)

# also, will probably have to find an open-source map instead of Google Maps
## how to do any of this ????
