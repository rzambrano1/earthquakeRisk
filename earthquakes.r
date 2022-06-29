######################################################
## PROOF SET DATASET - U.S.MINT - DATA SCIENTIST POS##
## Author:                      Ricardo Zambrano    ##
## Version:                     0.1                 ##
## Date Created:                May/24/2022         ##
## Most Recent Review Date:     May/24/2022         ##
## Rationale:                   Current Version     ##
######################################################

###############
## LIBRARIES ##
###############

library(tidyverse) # Data science standard library
library(lubridate)
library(stringr) # Regular expressions
library(htmlwidgets)
library(lobstr)
library(rlang)
library(vctrs)
library(rvest) # To scrape data from websites

#############################
## ENVIRONMENT PREPARATION ##
#############################

# To create a variable with the working directory of the current project
getwd() 
path <- getwd()


##################
## Data Request ##
##################

# STEP-1 Loading countries and areas from https://www.usembassy.gov/

embURL <- "https://www.usembassy.gov/"
embHTML <- read_html(embURL) # Loads html data from https://www.usembassy.gov/

htmlClass <- ".pcs-post-title"
htmlLines <- html_elements(embHTML,htmlClass)

htmlTitle <- "title"
embLocsRaw <- html_text(htmlLines,htmlTitle) # Extracts the list of countries and areas from https://www.usembassy.gov/
embLocsLC <- str_to_lower(embLocsRaw) # Changes the list into lower case to match url patterns
embLocsDF <- as_tibble(embLocsLC) # Since countries/areas with two words and special characters have unpredictable patters and because of the tight deadline the list will be imputed manually
write_csv(embLocsDF,paste0(path,"/embLocsLC.csv")) # Writes a csv file to be processed manually
embLocsImp <- read_csv(paste0(path,"/embLocsProcessed.csv"))

# Creating a dataframe with the urls for each country / area location
embLocsURLs <- map2("https://www.usembassy.gov/",embLocsImp,paste0)
embLocsURLs <- map2(embLocsURLs,"/",paste0)
embLocsURLs <- unlist(embLocsURLs)

# The following code creates a function to test the correcctness of the urls in embLocsURLs
urlWorks <- function(embURL) {
  tryCatch(
    error = function(cnd) FALSE,
    {loadTest <- read_html(embURL)
    TRUE}
  )
}

# The following snippet test the processed urls
urlsResult <- map(embLocsURLs,urlWorks)

# Then we identify the urls that did not worked with the following code
falseIndx <- list()
for (indx in 1:193) {
  if (urlsResult[[indx]]==FALSE) {
    falseIndx <- c(falseIndx,indx)
  }
}
falseIndx

# The following items did not work
# embLocsURLs[[40]] ## costa rica ## FIXED
# embLocsURLs[[109]] # marshal-islands ## FIXED
# embLocsURLs[[146]] # rwanda ## THE LINK IS BROKEN HAS TO BEE INPUTED MANUALLY
# the url reads "republic-of-congo" in lieu of "rwanda"
# Warnings correspond the errors
# Warning messages:
# 1: In for (i in seq_along(specs)) { :
#    closing unused connection 5 (https://www.usembassy.gov/rwanda/)
# 2: In for (i in seq_along(specs)) { :
#    closing unused connection 4 (https://www.usembassy.gov/marshal-islands/)
# 3: In for (i in seq_along(specs)) { :
#    closing unused connection 3 (https://www.usembassy.gov/costa rica/)

# The following code reads the ammended urls

embLocsImp <- read_csv(paste0(path,"/embLocsProcessed_v2.csv"))

# Creating a dataframe with the urls for each country / area location
embLocsURLs <- map2("https://www.usembassy.gov/",embLocsImp,paste0)
embLocsURLs <- map2(embLocsURLs,"/",paste0)
embLocsURLs <- unlist(embLocsURLs)

embLocs <- tibble (
  Country = embLocsRaw,
  StateURL = embLocsURLs
)

# The following helper function is used to split the address elements
splitter <- function (strElem) {
  re1 <- "\n"
  addressElements <- unlist(str_split(strElem,re1))
  addressElements
}

# The following function extracts the addresses listed for a given country or area
getAddresses <- function (cntryURL) {
  cntryHTLM <- read_html(as.character(cntryURL)) 
  cntryClass <- ".cityname1"
  cntryLines <- html_elements(cntryHTLM,cntryClass)
  cntryAddressRaw <- html_text(cntryLines,"a")
  listAddr <- map(cntryAddressRaw,splitter)
  listAddr
}

embLocsExp <- embLocs
embLocsExp["Locations"] <- rep(0,nrow(embLocs))

#This for loop will help find the mission with the largest number of locations
for (indx in 1:nrow(embLocsExp)) {
  tempAddr <- getAddresses(embLocsExp[indx,2])
  tempAddrNum <- length(tempAddr)
  embLocsExp[indx,3] <- tempAddrNum
}

max(embLocsExp$Locations,na.rm=TRUE) # Ten locations 
table(embLocsExp$Locations)

# NOTE: There are 55 missions with a different HTML scheme. 

for (indx in 1:10) {
  eval(expr(embLocsExp[!!paste0("City",indx)] <- rep("NA",nrow(embLocsExp))))
}

# The following for loop will attempt to extract the first line of each location city

for (indx in 1:nrow(embLocsExp)) {
  tempAddr <- getAddresses(embLocsExp[indx,2])
  tempAddrNum <- length(tempAddr)
  if (tempAddrNum == 0) {
    next
  } else {
    for (jndx in 1:tempAddrNum) {
      embLocsExp[indx,(jndx+3)] <- tempAddr[[jndx]][1]
    }
  }
}

# Because of time limitations the rest of the city locations will be manually imputed
write_csv(embLocsExp,paste0(path,"/embLocsExp.csv"))
embLocsCoord <- read_csv(paste0(path,"/embLocsCoordV4.csv")) # Reads the imputed file. It was scrubbed in MS Excel

# The mission location coordinates were obtained from 
# source url: https://simplemaps.com/data/world-cities


# STEP-2 Loading earthquake data from "NCEI/WDS Global Significant Earthquake Database, 2150 BC to Present"
# source url: https://data.noaa.gov/metaview/page?xml=NOAA/NESDIS/NGDC/MGG/Hazards/iso/xml/G012153.xml&view=getDataView#

earthquakes <- read_csv(paste0(path,"/earthquakesV3.csv"))
earthquakes

##################################
## Data Exploration and Analysis##
##################################

# STEP-1 Ploting U.S. Missions in a Map

world <- map_data("world")
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data=embLocsCoord,
    aes(Longitude,Latitude),
    colour="blue"
  ) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("U.S. Diplomatic Missions Locations")

# STEP-2 Plotting major earthquakes since 1970

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data=earthquakes,
    aes(Longitude,Latitude),
    colour="red"
  ) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Major Earthquakes Since 1970")

# STEP-3 Overlapping Plots
  
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data=earthquakes,
    aes(Longitude,Latitude),
    colour="red"
  ) +
  geom_point(
    data=embLocsCoord,
    aes(Longitude,Latitude),
    colour="blue"
  ) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("U.S. Diplomatic Missions - Major Earthquakes Since 1970")

# STEP-4 develop functions to calculate the distance between two earth locations
# by unsing their coordinates: latitude and longitude
# Follows implementation of Great Circle Navigation available on:
# https://www.nhc.noaa.gov/gccalc.shtml #ref1#
# http://edwilliams.org/avform147.htm#Intro #ref2#

# Assumes latitude and longitude of two coordinates in radians
# Returns the distance in radians using Great Circle navigation computations 
distCal <- function(lat1,lon1,lat2,lon2) {
  distance <- 2*asin(sqrt((sin((lat1-lat2)/2))^2+cos(lat1)*cos(lat2)*(sin((lon1-lon2)/2))^2))
  distance
}

# Assumes a coordinate (latitude or longitude) in decimal units
# Returns the coordinate in radians
toRad <- function(decCoord) {
  radCoord <- decCoord*pi/180
}

# Assumes latitude and longitude of two coordinates in decimal units
# Returns the distance in nautical miles (nM)
distCalnM <- function (lat1,lon1,lat2,lon2) {
  lt1 <- toRad(lat1)
  lt2 <- toRad(lat2)
  ln1 <- toRad(lon1)
  ln2 <- toRad(lon2)
  distanceRad <- distCal(lt1,ln1,lt2,ln2)
  distance2 <- distanceRad*180*60/pi
  distance2
}

# Assumes a distance in nautical miles (nM)
# Returns the distance in Km
nMtoKm <- function(nmDist) {
  kmDist <- nmDist*1.852
  kmDist
}

# According to the Richter magnitude scale a major earthquake (7.0 to 7.9)
# can be felt across great distances with major damage mostly limited to 
# 250 km from epicenter (or 135 nautical miles)
# source: https://en.wikipedia.org/wiki/Richter_magnitude_scale#Richter_magnitudes

# The next computations will count the number of significant earthquakes within a
# radius of 135 nautical miles from each U.S. mission location. 
# The assumption is that the greater the frequency of significant earthquakes within
# the vicinity of the mission location the greater the seismic risk for the location.
# It is also assumed that major earthquake is equivalent to significant earthquake in order 
# to establish the benchmark of 135 nautical miles.

embLocsRisk <- embLocsCoord
embLocsRisk["nearEarthQuakes"] <- rep(0,nrow(embLocsRisk))

# The following test (currently comented-out) detected an NA value in row 248 of the
# earthquake dataframe. The following removes the row with the NA value
# TESTS START #
# sum(is.na(earthquakes[,13]))
# sum(is.na(earthquakes[,14]))
# which(is.na(earthquakes[,13]))
# which(is.na(earthquakes[,14]))
# earthquakes[248,]
# earthquakes[248,13]
# earthquakes[248,14]
# TESTS END #

earthquakes <- filter(earthquakes,!is.na(Latitude))

for (indx in 1:nrow(embLocsRisk)) {
  missionLat <- unlist(embLocsRisk[indx,7]) # Fixes a mission location's latitude under analysis
  missionLon <- unlist(embLocsRisk[indx,8]) # Fixes a mission location's longitude under analysis
  earthquakeDists <- rep(0,nrow(earthquakes)) # Creates a vector to store distances from the mission's location to each significant earthquake in the data frame
  earthquakesMajor <- rep(0,nrow(earthquakes)) # Creates a vector to store a dummy variable for a significant earthquake near location
  earthquakeDists <- map2(earthquakes[,13],earthquakes[,14],distCalnM,missionLat,missionLon) # Calculates distances and stores them in a vector. Uses functional programming style and vectorization to reduce the loop's runtime
  earthquakeDists2 <- unlist(earthquakeDists$Latitude)
  earthquakesMajor <- ifelse(earthquakeDists2 < 136,1,0) # Creates and stores the dummy variable for significant earthquake ner mission location
  earthquakesCount <- sum(earthquakesMajor,na.rm=TRUE) # sums cumulatively number of significant earthquakes near missions location since 1970
  embLocsRisk[indx,9] <- earthquakesCount # Stores earthquakesCount variable in the embLocsRisk dataframe
}

table(embLocsRisk[,9]) # To visualize frequency of significant earthquake near location by significant earthquake count

numEmbLocInEQCount <- as.data.frame(table(embLocsRisk[,9]))
colnames(numEmbLocInEQCount)[1] <- "sigEarthquakeCount"
numEmbLocInEQCount <- as_tibble(numEmbLocInEQCount)
numEmbLocInEQCount

# The following plot is a histogram showing the number of U.S. missions locations falling
# into a region with a given count of nearby significant earthquake since 1970 

ggplot(data=embLocsRisk, aes(x=nearEarthQuakes)) +
  geom_histogram() +
  ggtitle("Count of U.S. Diplomatic Missions vs. Frequancy of Significant Earthquakes Since 1970")

embEQRisk <-arrange(embLocsRisk,nearEarthQuakes) 

embEQRisk %>%
  filter(nearEarthQuakes>20) %>%
  arrange(desc(nearEarthQuakes)) %>%
  ggplot(aes(x=fct_reorder(Mission,nearEarthQuakes),y=nearEarthQuakes))+
  geom_col(fill="red") +
  theme(axis.text.x = element_text(angle = 90)) +
  #coord_flip() +
  ggtitle("U.S. Mission Location vs. Number of Significant Earthquakes Near Location",
          subtitle = "Significant Earthquakes Count > 20") +
  labs(y="Number of Near Significant Earthquakes",x="U.S. Diplomatic Mission")

embEQRisk %>%
  filter(nearEarthQuakes>10 & nearEarthQuakes < 21) %>%
  arrange(desc(nearEarthQuakes)) %>%
  ggplot(aes(x=fct_reorder(Mission,nearEarthQuakes),y=nearEarthQuakes))+
  geom_col(fill="orange") +
  theme(axis.text.x = element_text(angle = 90)) +
  #coord_flip() +
  ggtitle("U.S. Mission Location vs. Number of Significant Earthquakes Near Location",
          subtitle = "Significant Earthquakes Count between 10 and 21") +
  labs(y="Number of Near Significant Earthquakes",x="U.S. Diplomatic Mission")

embEQRisk %>%
  filter(nearEarthQuakes>1 & nearEarthQuakes < 11) %>%
  arrange(desc(nearEarthQuakes)) %>%
  ggplot(aes(x=fct_reorder(Mission,nearEarthQuakes),y=nearEarthQuakes))+
  geom_col(fill="green") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("U.S. Mission Location vs. Number of Significant Earthquakes Near Location",
          subtitle = "Significant Earthquakes Count between 1 and 11") +
  theme_void()

embEQRisk %>%
  filter(nearEarthQuakes == 0) %>%
  ggplot(aes(x=Mission,y=nearEarthQuakes))+
  geom_col(fill="blue") +
  #theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("U.S. Mission Location vs. Number of Significant Earthquakes Near Location",
          subtitle = "Significant Earthquakes Count = 0")

write_csv(embEQRisk,paste0(path,"/embRisk.csv"))

##############
# SCRIPT END #
##############
