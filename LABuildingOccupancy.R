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

# import data and set up views we want

setwd("~/Desktop/AHLA Blogging")
coo <- read.csv("coo103118.csv")
library(zipcode)
data(zipcode)

# create a smaller aggregated dataframe by zip, building size, and year
summarydf <- coo[c(14,15,4,13)]
head(summarydf)
sum(summarydf$rdus) # should be 52,482

coo_agg <- summarydf %>%
  group_by(zip, X50.units, period) %>%
  summarize(rdu_total = sum(rdus))
head(coo_agg)
nrow(coo_agg)

coo2 <- merge(coo_agg, zipcode, by = 'zip') #merge RDU and ZIP code database w/lat-longs by ZIP
head(coo2)

aggregate(rdu_total~zip, data=coo2, FUN=sum) # check totals

# now use ggmap to overlay dots on a Google Map image

la_map <- get_map(location = c(lon=-118.3, lat=34.0),  maptype = "roadmap", source = "google", zoom = 10) # yes found the right place to center the map. still might zoom in more and not show the Valley
la_map2 <- get_map(location = c(lon=-118.35, lat=34.0),  maptype = "roadmap", source = "google", zoom = 11) # zoom that in a little closer
la_map3 <- get_map(location = c(lon=-118.35, lat=34.0),  maptype = "roadmap", source = "google", zoom = 12) # closeeeeer...
ggmap(la_map) + geom_point(data = coo2, mapping = aes(x = longitude, y = latitude, color = rdu_total)) + scale_fill_distiller("RDUs Permitted", palette = "Spectral") #dots are colored by rdu_count

# now draw the ZCTA boundaries for Los Angeles
#install.packages("tidyverse")
library(tidyverse)
library(tigris)

LA <- c('90001','90002','90003',
        '90004',
        '90005',
        '90006',
        '90007',
        '90008',
        '90010',
        '90011',
        '90012',
        '90013',
        '90014',
        '90015',
        '90016',
        '90017',
        '90018',
        '90019',
        '90020',
        '90021',
        '90023',
        '90024',
        '90025',
        '90026',
        '90027',
        '90028',
        '90029',
        '90031',
        '90032',
        '90033',
        '90034',
        '90035',
        '90036',
        '90037',
        '90038',
        '90039',
        '90041',
        '90042',
        '90043',
        '90044',
        '90045',
        '90046',
        '90047',
        '90048',
        '90049',
        '90057',
        '90059',
        '90061',
        '90062',
        '90063',
        '90064',
        '90065',
        '90066',
        '90067',
        '90068',
        '90069',
        '90077',
        '90094',
        '90210',
        '90212',
        '90230',
        '90232',
        '90247',
        '90248',
        '90272',
        '90291',
        '90292',
        '90293',
        '90402',
        '90501',
        '90710',
        '90731',
        '90732',
        '90744',
        '91040',
        '91042',
        '91105',
        '91205',
        '91214',
        '91303',
        '91304',
        '91306',
        '91307',
        '91311',
        '91316',
        '91324',
        '91325',
        '91326',
        '91331',
        '91335',
        '91340',
        '91342',
        '91343',
        '91344',
        '91345',
        '91352',
        '91356',
        '91364',
        '91367',
        '91401',
        '91402',
        '91403',
        '91405',
        '91406',
        '91411',
        '91423',
        '91436',
        '91504',
        '91601',
        '91602',
        '91604',
        '91605',
        '91606',
        '91607')


LAZips <- bind_rows(tibble(area = "LA", zip = LA)) #create df with all ZIP codes in LA City
zips_la <- zctas(cb = T, starts_with = c("90","91"), class = "sf") %>%
  select(zip = ZCTA5CE10, geometry) # now pull the ZCTAs for all ZIPs beginning with 90, 91
options(tigris_use_cache = TRUE) #caches data, saving time
head(zips_la)
plot(zips_la) # you can see that this includes a ton of ZIPs outside of LA City
zips_lacity <- zips_la %>%
  inner_join(LAZips, by = "zip")
zips_lacity <- zips_lacity %>% select(zip = zip, geometry) #remove the "area" column
head(zips_lacity)
plot(zips_lacity) # oh baby the LA City ZCTAs are plotted correctly
ggmap(la_map) +
  geom_sf(aes(fill = zip), data = zips_lacity, inherit.aes = F, size = 0, alpha = 0.8) +
  coord_sf(ndiscr = F) +
  theme(legend.position = "none") # yeeees they plot correctly on a google map
zips_lacity$zip <- as.integer(zips_lacity$zip) # turn the ZIPs in zips_lacity into integers
class(zips_lacity$zip)
coo3 <- zips_lacity %>% inner_join(coo2, by = 'zip') # combine ZCTA info with RDU count and lat-longs

# let's make some maps!
# map of all buildings opened in Los Angeles, 1/1/13-10/15/18, by ZIP, colored by # RDUs

map1a <- ggmap(la_map) +
  geom_sf(aes(fill = cut(rdu_total,c(0,100,500,1000,5000,10000), include.lowest = F, dig.lab=10)), data = coo3, lwd = 0.05, #omg yes lwd draws the lines, finally
          colour = 'black', inherit.aes = F, size = 0, alpha = 0.5) +
  coord_sf(ndiscr = F) +
  scale_fill_brewer(name = "RDUs Opened", palette=8) +
  theme_bw()

# zoom in closer
map1b <- ggmap(la_map2) +
  geom_sf(aes(fill = cut(rdu_total,c(0,100,500,1000,5000,10000), include.lowest = F, dig.lab=10)), data = coo3, lwd = 0.05,
          colour = 'black', inherit.aes = F, size = 0, alpha = 0.5) +
  coord_sf(ndiscr = F) +
  scale_fill_brewer(name = "RDUs Opened", palette=8) +
  #scale_fill_continuous(label=comma)
  #trans = 'reverse' # this reverses a continuous color scale
  #scale_fill_gradientn(name = "RDUs Permitted", colors=rainbow(3), labels = comma) +
  #scale_fill_gradient(name = "RDUs Permitted", low="black", high="blue") +
  #scale_fill_distiller("RDUs Permitted", palette = "Spectral") +
  #viridis::scale_fill_viridis(discrete = T) +
  theme_bw()

# map of all buildings opened in Los Angeles, 1/1/18-10/29/18, by ZIP, colored by # RDUs
coo2018 <- subset(coo3, period == "2018") # limit to 2018 only
head(coo2018)
sum(coo2018$rdu_total) # should total 10,383

map2 <- ggmap(la_map2) +
  geom_sf(aes(fill = cut(rdu_total,c(0,100,250,1000,5000), include.lowest = F, dig.lab=10)), data = coo2018, lwd = 0.05,
          colour = 'black', inherit.aes = F, size = 0, alpha = 0.5) +
  coord_sf(ndiscr = F) +
  scale_fill_brewer(name = "RDUs Opened", palette=8) +
  theme_bw()

# map of all buildings opened in Los Angeles with 50+ units, 1/1/13-10/29/18, by ZIP, colored by # RDUs
coo50 <- subset(coo3, X50.units == "Yes") # limit to 50+ only
head(coo50)
sum(coo50$rdu_total) # should total 29,111

map3 <- ggmap(la_map2) +
  geom_sf(aes(fill = cut(rdu_total,c(0,500,1000,5000,10000), include.lowest = F, dig.lab=10)), data = coo50, lwd = 0.05,
          colour = 'black', inherit.aes = F, size = 0, alpha = 0.5) +
  coord_sf(ndiscr = F) +
  scale_fill_brewer(name = "RDUs Opened", palette=8) +
  theme_bw()

# map of all buildings opened in Los Angeles with 50+ units, 1/1/18-10/15/18, by ZIP, colored by # RDUs
coo50_2018 <- subset(coo2018, X50.units == "Yes") # limit to 50+ only
head(coo50_2018)
sum(coo50_2018$rdu_total) # should total 5,311

map4 <- ggmap(la_map2) +
  geom_sf(aes(fill = cut(rdu_total,c(0,200,1000,5000), include.lowest = F, dig.lab=10)), data = coo50_2018, lwd = 0.05,
          colour = 'black', inherit.aes = F, size = 0, alpha = 0.5) +
  coord_sf(ndiscr = F) +
  scale_fill_brewer(name = "RDUs Opened", palette=8) +
  theme_bw()

# now export these sick ass maps
ggsave(filename="map1a.png", plot=map1a)
ggsave(filename="map1b.png", plot=map1b)
ggsave(filename="map2.png", plot=map2)
ggsave(filename="map3.png", plot=map3)
ggsave(filename="map4.png", plot=map4)
