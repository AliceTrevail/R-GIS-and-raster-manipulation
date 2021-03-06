### Create a circular shape file around a central point ###

if(!"geosphere" %in% installed.packages())
  install.packages("geosphere")
library(geosphere)

if(!"raster" %in% installed.packages())
  install.packages("raster")
library(raster)

if(!"rgdal" %in% installed.packages())
  install.packages("rgdal")
library(rgdal)

if(!"plyr" %in% installed.packages())
  install.packages("plyr")
library(plyr)

######## Functions ##########

create.spPolygon <- function(x){
  p = SpatialPolygons(list(Polygons(list(Polygon(x)),1)))
  projection(p) <- CRS("+proj=longlat +datum=WGS84")
  p
}

create.circle.spPolygon <- function(long, lat, dist.m){
  
  bearing <- c(1:360)
  circle <- data.frame(bearing)
  
  circle[,2:3] <- destPoint(c(long, lat), circle$bearing, dist.m)
  circle$bearing <- NULL
  names(circle)[1] <- "lon"
  names(circle)[2] <- "lat"
  
  circle.p <- create.spPolygon(circle)
  circle.p
}

# define longitude, latitude and radius of shape file (in m)
Longitude <- -2.5667
Latitude <- 56.1833
radius <- 50000 # 50km

# set working directory
setwd("C:/Users/atrevail/Desktop")

# make circular shape polygon, set projection, and create SPDF
circle <- create.circle.spPolygon(Longitude,Latitude,radius)
projection(circle) <- CRS("+proj=longlat +datum=WGS84")
df <- as.data.frame(cbind("Isle of May"))
spdf <- SpatialPolygonsDataFrame(circle, df, match.ID = FALSE)

# write to file, dsn = folder within working directory
writeOGR(spdf, dsn = "Shape files", layer = "Circle 50km", driver = "ESRI Shapefile")
