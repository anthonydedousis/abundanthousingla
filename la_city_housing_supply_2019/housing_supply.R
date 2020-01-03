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
library(tidyr)

## import data and set up views we want
setwd("~/Documents/Abundant Housing LA/2001xx - Housing Supply")
supply <- read.csv("LA City Parcel Dataset 2019.csv")
head(supply)
nrow(supply)

## drop unnecessary rows and columns
supply <- supply[c(3,7,8,10,11,12,13,15,18,19,20,47,49,50)] # limit to relevant columns
names(supply) <- c("ain", "address", "property_type", "gen_use_type", 
                     "spec_use_type", "detail1", "detail2", "year_built", 
                     "bedrooms","bathrooms", "units","zip_code","lat",
                     "long")
head(supply)
# limit to general use types that can contain housing units
supply <- subset(supply, gen_use_type=="Residential" | gen_use_type=="Commercial")
head(supply)
nrow(supply) # 770,072 rows

## confirm that numerical columns are considered numeric
is.numeric(supply$units) # TRUE
is.numeric(supply$bedrooms) # TRUE
is.numeric(supply$bathrooms) # TRUE
is.numeric(supply$zip_code) # TRUE
is.numeric(supply$lat) # TRUE
is.numeric(supply$long) # TRUE
is.numeric(supply$year_built) # TRUE

# double check by selecting a row that has Units = 0
supply_1row <-  subset(supply, address == "2129 W SUNSET BLVD  LOS ANGELES CA  90026" )
head(supply_1row) # correctly shows that this property has 0 units

# create a Commercial-only subset, limited to "Store and Residential" and "Office and Residential" only
supply_comm <- subset(supply, (gen_use_type=="Commercial" & 
              detail1=="Store and Residential Combination") | 
                (gen_use_type=="Commercial" & 
              detail1=="Office and Residential"))
supply_comm <- subset(supply_comm, units > 0) # delete properties with 0 residential units
head(supply_comm)
nrow(supply_comm) #  2,477 - matches count in Excel
sum(supply_comm$units) # 25,395 - matches count in Excel

# create a Residential-only subset
supply_res <- subset(supply, (gen_use_type=="Residential" & 
                                 units > 0)) # delete properties with 0 units
supply_res <- subset(supply_res, (gen_use_type=="Residential" & 
                           !(detail2=="Vacant Parcel with Existing Non-structural Other Imps.")
                         | (property_type=="SFR" & 
                              !(detail1=="Wireless Communication Tower"))))
head(supply_res)
nrow(supply_res) # 697,354 - matches count in Excel
sum(supply_res$units) # 1,376,298 units - matches count in Excel
sum(supply_res$units) + sum(supply_comm$units) # 1,401,693 units

# combine the Commercial and Residential dataframes

supply <- rbind(supply_res, supply_comm)
nrow(supply) # 699,831 rows
sum(supply$units) # 1,401,693 units

## identify rows that are missing lat-longs
sum(is.na(supply$lat))
sum(is.na(supply$long)) # 79 rows are missing lat-longs
sum(is.na(supply$long))
sum(is.na(supply$year_built)) # no NAs, but there are many rows with "0" as year
supply[,8][supply[,8] == 0] <- NA # replaces the year_built = 0 rows with NA
sum(is.na(supply$year_built)) # 1,877 rows with year_built unknown

# create separate dataframe for buildings without lat-longs
supply_no_coords <- supply
supply_no_coords <- subset(supply_no_coords, is.na(lat))
supply_no_coords[is.na(supply_no_coords)] <- " "
head(supply_no_coords)
nrow(supply_no_coords) # again, 79 rows are missing lat-longs

# drop rows without lat-longs that are also missing ZIP codes and addresses
supply_no_coords[,12][supply_no_coords[,12] == " "] <- NA # replaces the zip_code = blank rows with NA
supply_no_coords <- supply_no_coords[!(is.na(supply_no_coords$zip_code)),] # drop rows that are missing a ZIP code
nrow(supply_no_coords) # now, we see that 20 rows are missing lat-longs, but have addresses
head(supply_no_coords)
sum(supply_no_coords$units) # these 20 properties have 653 units total
write.csv(supply_no_coords, "supply_no_coords.csv") # export

# drop rows with NAs for lat-longs (we'll add them back in a minute)
supply <- supply[!(is.na(supply$long)) | !(is.na(supply$lat)),]
nrow(supply)

# import separate CSV containing the lat-longs for the 20 properties in supply_no_coords
geocodes <- read.csv("Geocodes2019.csv")
geocodes <- geocodes[c(1:3)]
colnames(geocodes)[1] <- "address"
colnames(geocodes)[2] <- "lat"
colnames(geocodes)[3] <- "long"
head(geocodes)
nrow(geocodes)

## append missing lat-longs into supply_no_coords
supply_no_coords <- left_join(supply_no_coords, geocodes, by = "address") # join the geocode lat-longs by address
head(supply_no_coords)
supply_no_coords <- supply_no_coords[c(1:12,15:16)] # drop null lat-long columns
colnames(supply_no_coords)[13] <- "lat"
colnames(supply_no_coords)[14] <- "long"
head(supply_no_coords) # great, these have the coordinates now

# join supply and supply_no_coords to restore original dataset
supply <- rbind(supply, supply_no_coords)
nrow(supply) # 699,772 rows = 669,752 rows with coords + 20 rows that were missing coords, but had addresses
sum(supply$units) # 1,401,634 homes
write.csv(supply, "supply_final.csv") # export

# update the supply_res and supply_comm datasets
supply_res <- subset(supply, (gen_use_type=="Residential"))
head(supply_res)
nrow(supply_res) # 697,295 rows
sum(supply_res$units) # 1,376,239 units

supply_comm <- subset(supply, (gen_use_type=="Commercial"))
head(supply_comm)
nrow(supply_comm) #  2,477 rows
sum(supply_comm$units) # 25,395 units

nrow(supply_res) + nrow(supply_comm) # 697,295 + 2,477 = 699,772 rows
sum(supply_res$units) + sum(supply_comm$units) # 1,401,634 units

# create discrete category of buildings by number of units
supply$building_size <- cut(supply$units,
                              breaks = c(-Inf, 2, 5, 10, 20, 50, 100, 200, Inf),
                              labels = c("1 unit", "2-4 units", "5-9 units", "10-19 units",
                                         "20-49 units", "50-99 units", "100-199 units",
                                         "200+ units"),
                              right = FALSE)
head(supply)

bldg_size <- supply %>%
  group_by(building_size) %>%
  summarise(units=sum(units))
bldg_size

# create discrete category of buildings by decade
supply$decade <- cut(supply$year_built,
                     breaks = c(-Inf, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, Inf),
                     labels = c("Before 1920", "1920-29", "1930-39", "1940-49", "1950-59",
                                "1960-69", "1970-79", "1980-89", "1990-99", "2000-09",
                                "2010-19"),
                     right = FALSE)
head(supply)

by_decade <- supply %>%
  group_by(decade) %>%
  summarise(units=sum(units))
by_decade

# create continuous variable for building age
supply$bldg_age <- 2019-supply$year_built
head(supply)

# create discrete category of buildings by age

supply$age_range <- cut(supply$bldg_age,
                     breaks = c(-Inf, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, Inf),
                     labels = c("<5 years", "5-9 years", "10-19 years", "20-29 years", 
                                "30-39 years", "40-49 years", "50-59 years", "60-69 years",
                                "70-79 years", "80-89 years", "90-99 years", "100+ years"
                                ),
                     right = FALSE)
head(supply)

by_age <- supply %>%
  group_by(age_range) %>%
  summarise(units=sum(units))
by_age

# create discrete category for building type
supply$bldg_category <- cut(supply$units,
                            breaks = c(-Inf, 1, 2, Inf),
                            labels = c("Error", "Single-family", "Multi-family"
                            ),
                            right = FALSE)
head(supply)

by_category <- supply %>%
  group_by(bldg_category) %>%
  summarise(units=sum(units))
by_category # excellent, confirms that there are no 0-unit properties in the dataset

## import LA neighborhood boundaries
file.exists("LA_Times_Neighborhoods.shp")
neighborhood <- readOGR(dsn = "~/Documents/Abundant Housing LA/2001xx - Housing Supply/LA_Times_Neighborhoods", layer = "LA_Times_Neighborhoods")
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
register_google(key = "AIzaSyDML4DP3Z-675erwTe9cxTg3TD5SYwj7rg")

la_map <- get_map(location = c(lon=-118.35, lat=34.1),  maptype = "roadmap", source = "google", zoom = 11) # map of los angeles
ggmap(la_map)

la_map2 <- get_map(location = c(lon=-118.31, lat=34.0),  maptype = "roadmap", source = "google", zoom = 10) # map of los angeles
ggmap(la_map2)

## now add neighborhood boundaries

la_map_nhood <- ggmap(la_map2) + 
  geom_polygon(data = nh_df, aes(x = long, y = lat, group = group), fill = NA, linetype = "21", size = 0.2, color = "black") 
la_map_nhood

# aggregate buildings by neighborhood
coords <- supply[c('long','lat')]
head(coords)
nrow(coords)
coords <- data.frame(coords)
sp <- SpatialPoints(coords)
proj4string(sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(sp)
by_neighborhood <- over(sp, neighborhood) # creates a dataframe of building coordinates and neighborhood shapefile

by_nh_summary <- by_neighborhood %>%
  group_by(name) %>%
  summarise(total=n()) # count the number of buildings by neighborhood
head(by_nh_summary)
class(by_nh_summary)
sum(by_nh_summary$total) # should be 699,772 rows

## combine building coordinates + neighborhood dataframe to create dataset 
## with a neighborhood associated with every building

coords_with_nh <- cbind(coords, by_neighborhood$name)
colnames(coords_with_nh)[3] <- "neighborhood" # rename column
head(coords_with_nh)

supply_combined <- cbind(supply, coords_with_nh)
supply_combined <- supply_combined[-c(20:21)] # drop duplicate lat-long
head(supply_combined)
sum(supply_combined$units) # should be 1,401,634 homes
supply_combined_no_nh <- subset(supply_combined, is.na(neighborhood))
sum(supply_combined_no_nh$units) # 74 homes don't have a neighborhood, not sure why
write.csv(supply_combined, "supply_combined.csv") # export

# create a dataframe that totals homes by neighborhood

supply_by_nh <- supply_combined %>%
  group_by(neighborhood) %>%
  summarise(total_homes=sum(units)
             , homes_after_1998=sum(units[bldg_age<21], na.rm=T)
             , share_after_1998=homes_after_1998/total_homes
             , homes_after_1968=sum(units[bldg_age<51], na.rm=T)
             , share_after_1968=homes_after_1968/total_homes
             , total_bldgs=n()
             , avg_bldg_age=weighted.mean(bldg_age, units, na.rm=T)
             , avg_bldg_size=mean(units)
             , sf_bldgs=sum(units[units<2])
             , sf_share=sf_bldgs/total_homes
             , mf_share=1-sf_share
             , five_plus_bldgs=sum(units[units>4])
             , ten_plus_bldgs=sum(units[units>9])
             , fifty_plus_bldgs=sum(units[units>49])
             , five_plus_share=five_plus_bldgs/total_homes
             , ten_plus_share=ten_plus_bldgs/total_homes
             , fifty_plus_share=fifty_plus_bldgs/total_homes) %>%
  mutate(share_of_nhood_homes=total_homes/sum(total_homes)) %>%
  arrange(desc(total_homes))
nrow(supply_by_nh) # 115 rows
head(supply_by_nh) 
tail(supply_by_nh) 
sum(supply_by_nh$total_homes) # again, should be 1,401,634 homes
write.csv(supply_by_nh, "supply_by_nh.csv") # export

## combine neighborhood shapefile with supply_by_nh dataset
head(nh_df)
colnames(nh_df)[7] <- "neighborhood"
combined <- nh_df %>% left_join(supply_by_nh, by = "neighborhood") # join the data frames; left join includes boundaries neighborhoods with zero units
head(combined)

## heat map of neighborhoods by number of homes

map_total_homes <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(total_homes, c(0,5000,10000,15000,20000,25000,35000,100000), 
                                               c("<5,000 units", "5,000-9,999 units", 
                                                 "10,000-14,999 units", "15,000-19,999 units", "20,000-24,999 units",
                                                 "25,000-34,999 units", "35,000+ units"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "Homes by Neighborhood", palette="YlGnBu", na.value="#D0D3D4") 
map_total_homes

## heat map of neighborhoods by average building size

map_avg_bldg_size <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(avg_bldg_size, c(0,1.5,2,3,4,5,6,50), 
                                               c("<1.5 units", "1.5-2 units", "2-3 units", 
                                                 "3-4 units", "4-5 units", "5-6 units",
                                                 "6+ units"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "Average Building Size", palette="YlGnBu", na.value="#D0D3D4") 
map_avg_bldg_size

## heat map of neighborhoods by average building age

map_avg_bldg_age <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(avg_bldg_age, c(0,20,50,60,70,80,90,Inf), 
                                               c("<20 years", "20-50 years", "50-60 years", "60-70 years",
                                                 "70-80 years", "80-90 years", "90+ years"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "Average Housing Unit Age", palette="RdYlGn", direction=-1, na.value="#D0D3D4") 
map_avg_bldg_age

## heat map of neighborhoods by number of homes that are <21 years old

map_homes_after_1998 <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(homes_after_1998, c(0,500,1000,2000,3000,4000,14000,100000), 
                                               c("<500 units", "500-999 units", 
                                                 "1,000-1,999 units", "2,000-2,999 units", "3,000-3,999 units",
                                                 "4,000-13,999 units","14,000+ units"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "Homes Built After 1998 by Neighborhood", palette="YlGnBu", na.value="#D0D3D4") 
map_homes_after_1998

## heat map of neighborhoods by % of homes that are <21 years old

map_share_after_1998 <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(share_after_1998, c(0,0.1,0.2,0.3,0.4,Inf), 
                                               c("<10%","10-20%","20-30%","30-40%","40%+"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "% of Homes <21 years old", palette="RdYlGn", na.value="#D0D3D4") 
map_share_after_1998

## heat map of neighborhoods by number of homes that are <51 years old

map_homes_after_1968 <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(homes_after_1968, c(0,1000,2000,3000,5000,7000,10000,15000,100000), 
                                               c("<1,000 units", 
                                                 "1,000-1,999 units", "2,000-2,999 units", "3,000-4,999 units",
                                                 "5,000-6,999 units","7,000-9,999 units","10,000-14,999 units","15,000+ units"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "Homes Built After 1968 by Neighborhood", palette="YlGnBu", na.value="#D0D3D4") 
map_homes_after_1968

## heat map of neighborhoods by % of homes that are <51 years old

map_share_after_1968 <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(share_after_1968, c(0,0.1,0.2,0.3,0.4,0.5,0.7,Inf), 
                                               c("<10%","10-20%","20-30%","30-40%","40-50%", "50-70%","70%+"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "% of Homes <51 years old", palette="RdYlGn", na.value="#D0D3D4") 
map_share_after_1968

## heat map of neighborhoods by % of all homes

map_share <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(share_of_nhood_homes, c(0,0.005,0.01,0.015,0.02,0.025,0.03,0.035), 
                                               c("< 0.5% of total", "0.5-1.0% of total", "1.0-1.5% of total", 
                                                 "1.5-2.0% of total", "2.0-2.5% of total", "2.5-3.0% of total",
                                                 ">3.0% of total"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "Neighborhood's Share of All Homes", palette="YlGnBu", na.value="#D0D3D4") 
map_share

## heat map of neighborhoods by % of homes that are single family

map_sf_share <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(sf_share, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,Inf), 
                                               c("<10%", "10-20%", "20-30%","30-40%","40-50%","50-60%",
                                                 "60-70%","70-80%","80%+"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "% of Residences that are Single-Family", palette="RdYlGn", direction=-1, na.value="#D0D3D4") 
map_sf_share

## heat map of neighborhoods by % of homes that are multifamily

map_mf_share <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(mf_share, c(0,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,Inf), 
                                               c("<20%","20-30%","30-40%","40-50%","50-60%",
                                                 "60-70%","70-80%","80-90%","90%+"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "% of Residences that are Multifamily", palette="RdYlGn", na.value="#D0D3D4") 
map_mf_share

## heat map of neighborhoods by % of residences that are 5+ units

map_five_plus_share <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(five_plus_share, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,Inf), 
                                               c("<10%","10-20%","20-30%","30-40%","40-50%","50-60%",
                                                 "60-70%","70-80%","80%+"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "% of Residences that are 5+ Units", palette="RdYlGn", na.value="#D0D3D4") 
map_five_plus_share

## heat map of neighborhoods by % of residences that are 10+ units

map_ten_plus_share <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(ten_plus_share, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,Inf), 
                                               c("<10%","10-20%","20-30%","30-40%","40-50%","50-60%",
                                                 "60-70%","70-80%","80%+"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "% of Residences that are 10+ Units", palette="RdYlGn", na.value="#D0D3D4") 
map_ten_plus_share

## heat map of neighborhoods by % of residences that are 50+ units

map_fifty_plus_share <- ggmap(la_map2) + 
  geom_polygon(data = combined, aes(x = long, y = lat, group = group, 
                                    fill = cut(fifty_plus_share, c(0,0.1,0.2,0.3,0.4,0.5,Inf), 
                                               c("<10%","10-20%","20-30%","30-40%","40-50%","50%+"),
                                               include.lowest = T, dig.lab=10)), linetype = "21", size = 0.2, color = "black") +
  scale_fill_brewer(name = "% of Residences that are 50+ Units", palette="RdYlGn", na.value="#D0D3D4") 
map_fifty_plus_share