require(rgdal)
require(ggplot2)
require(rgeos)
require(ggmap)
require(shapefiles)
library(zoo)
options(digits = 20)

setwd("~/Dropbox/UCSD/POLI 170A")

# Zillow Median Home Value data
prices <- read.csv("Neighborhood_Zhvi_AllHomes.csv")
unemployment <- read.csv("unemployment.csv")
pricesSF <- subset(prices, CountyName == "San Francisco")
names(pricesSF)[8:dim(pricesSF)[2]] <-
  substr(as.character(names(pricesSF)[8:dim(pricesSF)[2]]),2,8) # fix column names

pricesCity <- subset(read.csv("City_Zhvi_AllHomes.csv"),RegionName == "San Francisco")
write.csv(pricesSF,"prices.csv")
write.csv(pricesCity,"pricesCity.csv")

## This section of the code maps eviction notices to its Zillow neighborhood.
#######################################
mapNotices <- function(notices)
{
  coords <-
    strsplit(substring(
      as.character(notices$Client_Location),2,nchar(as.character(notices$Client_Location)) -
        2
    ),", ")
  notices$lon = rep(NA, dim(notices)[1])
  notices$lat = rep(NA, dim(notices)[1])
  for (i in 1:dim(notices)[1])
  {
    notices$lat[i] = coords[[i]][1]
    notices$lon[i] = coords[[i]][2]
  }
  
  
  
  notices$Date <-
    as.Date(as.character(notices$File.Date),"%m/%d/%Y")
  notices <- notices[is.na(notices$Date) != TRUE,]
  
  shp <-
    paste(getwd(),"/ZillowNeighborhoods-CA/ZillowNeighborhoods-CA.shp",sep =
            "")
  CANeightborhoods_shp <-
    readOGR(shp, layer = "ZillowNeighborhoods-CA")
  
  Neighborhoods <-
    spTransform(CANeightborhoods_shp, CRS("+proj=longlat +datum=WGS84"))
  Neighborhoods <- Neighborhoods[790:823,]
  points <- data.frame(
    lon = as.numeric(notices$lon),
    lat = as.numeric(notices$lat),
    id = 1:dim(notices)[1]
  )
  
  notices$NeighborhoodZillow <- rep(NA,dim(notices)[1])
  for (a in 1:34)
  {
    print(a)
    for (i in 1:dim(notices)[1])
    {
      if (sum(is.na(points[i,])) == 0 &
          is.na(notices$NeighborhoodZillow[i]) == TRUE)
      {
        if (gContains(Neighborhoods[a,],SpatialPoints(points[i,1:2],proj4string = CRS(proj4string(
          Neighborhoods[a,]
        )))) ==
        TRUE)
        {
          notices$NeighborhoodZillow[i] <-
            as.character((Neighborhoods[a,]@data)$NAME)
          
        }
      }
    }
  }
  
  notices$NeighborhoodZillow <-
    as.character(notices$NeighborhoodZillow)
  notices$NeighborhoodZillow[notices$NeighborhoodZillow == "Ocean View"] <-
    "Oceanview"
  notices$NeighborhoodZillow[notices$NeighborhoodZillow == "Seacliff"] <-
    "Sea Cliff"
  notices$NeighborhoodZillow[notices$NeighborhoodZillow == "South Of Market"] <-
    "South of Market"
  return(notices)
}
#######################################
evictions <- mapNotices(read.csv("Eviction_Notices.csv"))
pricesSF <-
  pricesSF[unique(pricesSF$RegionName) %in% unique(evictions$NeighborhoodZillow),]

## Change true/false to numeric 1 or 0
for(i in 7:24)
{
  evictions[,i] <- ifelse(evictions[,i] == "true" | evictions[,i] == 1,1,0)
}

## Assign whether the eviction si a NoFault eviction
evictions$nofault <- NA
for(i in 1:dim(evictions)[1])
{
  evictions$nofault[i] <- ifelse(sum(evictions[i,c(7:13,22)]) < 1,1,0)
}

## Assign the median value of the specifc neighborhood for each eviction notice

getPrice <- function (neighborhood, monthyear)
{
  neighborhood <- as.character(neighborhood)
  monthyear <- as.yearmon(monthyear)
  pricesDate <- as.yearmon(names(pricesSF),"%Y.%m")
  pricesRegionName <- as.character(pricesSF$RegionName)
  if (neighborhood %in% pricesRegionName &
      monthyear >= min(na.omit(pricesDate)) &
      monthyear <= max(na.omit(pricesDate)))
  {
    checkName <-
      neighborhood == pricesRegionName
    checkDate <- monthyear == pricesDate
    checkDate[is.na(checkDate)] <- FALSE
    
    return(pricesSF[checkName,
                    checkDate])
  }
  else
  {
    return(NA)
  }
}
evictions$Price <- NA
for(i in 1:dim(evictions)[1])
{
  
  evictions$Price[i] <- getPrice(evictions$NeighborhoodZillow[i],as.yearmon(evictions$Date[i]))
}
# Ex: getPrice("Bayview","Jan 2015")
## Assign the median value of the specifc neighborhood for each eviction notice, 120 days after
evictions$Price120 <- NA
for(i in 1:dim(evictions)[1])
{
  
  evictions$Price120[i] <- getPrice(evictions$NeighborhoodZillow[i],as.yearmon(evictions$Date[i])+120/365)
}

evictions$unemployment <- NA
for(i in 1:dim(evictions)[1])
{
  evictions$unemployment[i] <- unemployment$Rate[as.yearmon(unemployment$Year,"%m/%Y") == as.yearmon(evictions$Date[i])]
}
## Get the change in price for each eviction notice
evictions$deltaPrice <- evictions$Price120 - evictions$Price
write.csv(evictions,"evictions.csv", row.names=FALSE)
