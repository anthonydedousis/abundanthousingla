'''
The purpose of this script is to merge ACS, Zillow, and Housing Supply data for Los Angeles.  See the parent directory\'s README for more information.

Started: 09.24.2019

R: 3.5.2 (2018-12-20)

Note that the data names here are from datasets downloaded from https://public.tableau.com/profile/anthony.dedousis#!/vizhome/LosAngelesNeighborhoodLevelDashboard/Story
'''
###############
####### GLOBALS
###############
library(reshape)  # for melt function

setwd('/Users/Zack/Documents/UCLA/Data/HousingSupply/')



###############
####### DATA
###############
acs <- read.csv('Data/ACS.csv', stringsAsFactors=FALSE)
supply <- read.csv('Data/HousingSupplybyNeighborhood.csv', stringsAsFactors=FALSE)
zillow <- read.csv('Data/Zillow.csv', stringsAsFactors=FALSE)

acs <- acs[,!(names(acs) %in% 'X')]  # Drop this redundant column name

###############
####### GET COMMON COLUMN NAMES
###############

## make neighborhood same spelling
names(acs)[names(acs) == 'name'] <- 'neighborhood'
names(zillow)[names(zillow) == 'id'] <- 'neighborhood'

## make year same
## keep X in zillow, supply because R needs column name to start with a letter
names(zillow) <- gsub('.03', '', names(zillow))


###############
####### RESHAPE SO YEARS ARE IN 1 COLUMN
###############
## melt columns.  acs already has Year column, need to update zillow and supply
z <- melt(zillow, measure.vars = c('X2011', 'X2012', 'X2013', 'X2014', 'X2015', 'X2016', 'X2017', 'X2018', 'X2019'))
names(z)[names(z) == 'variable'] <- 'Year'
names(z)[names(z) == 'value'] <- 'Rent_inflationAdjusted'
z <- z[,!(names(z) %in% 'X')]
z$Year <- gsub('X', '', z$Year)  # Can now get rid of the X prefix

s <- melt(supply, measure.vars = c('X2013', 'X2014', 'X2015', 'X2016', 'X2017', 'X2018'))
names(s)[names(s) == 'variable'] <- 'Year'
names(s)[names(s) == 'value'] <- 'NumberUnits'
s$Year <- gsub('X', '', s$Year)

###############
####### MERGE
###############
## First, add column prefixes to remember whic columns are from which data.
## Below is messy.  It finds all columns that are not the id columns, then adds zillow_ to the front
names(z)[grep(paste('neighborhood', 'Year', sep='|'), names(z), invert=TRUE)] <- paste0('zillow_', names(z)[grep(paste('neighborhood', 'Year', sep='|'), names(z), invert=TRUE)])
names(acs)[grep(paste('neighborhood', 'Year', sep='|'), names(acs), invert=TRUE)] <- paste0('acs_', names(acs)[grep(paste('neighborhood', 'Year', sep='|'), names(acs), invert=TRUE)])

new <- merge(x=s, y=z, by.x=c('neighborhood', 'Year'), by.y=c('neighborhood', 'Year'), all.x=TRUE, all.y=FALSE)
new <- merge(x=new, y=acs, by.x=c('neighborhood', 'Year'), by.y=c('neighborhood', 'Year'), all.x=TRUE, all.y=FALSE)

###############
####### SAVE
###############
write.csv(new, 'Data/SupplyZillowACS_merged.csv', row.names=FALSE)
