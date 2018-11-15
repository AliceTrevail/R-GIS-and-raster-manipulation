library(raster)
library(ggplot2)
library(rgdal)

# set working directory
setwd("F:/Data/R code/for sharing/Aliche")

#### Import bathymetry rasters ####
# Import raster of GEBCO bathymetry
GEBCO <- raster("GEBCO_IOM.grd")
plot(GEBCO)

# Import raster of UKHO bathymetry
UKHO <- raster("UKHO_IOM.grd")
plot(UKHO)

#### Combine to one raster ####
# to merge rasters, they need to be of the same extent and resolution

# create new raster to project both to highest extent and resolution
r <- raster()
extent(r) <- extent(GEBCO)
res(r)=res(UKHO)

# project both rasters to above extent and resolution
UKHO_projected <- projectRaster(UKHO, r, method = 'bilinear')
plot(UKHO_projected)

GEBCO_projected <- projectRaster(GEBCO, r, method = 'bilinear')
plot(GEBCO_projected)

# merge rasters
combined <- merge(UKHO_projected, GEBCO_projected)
plot(combined)

#### Plot bathymetry raster ####

# first, convert raster to dataframe
combined_spdf <- as(combined, "SpatialPixelsDataFrame")
combined_df <- as.data.frame(combined_spdf)
colnames(combined_df) <- c("bath", "long", "lat")

# ggplot for raster
ggplot(combined_df, aes(x=long, y=lat))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  geom_raster(aes(fill=bath))+
  coord_cartesian()+
  scale_fill_continuous(trans = "reverse")

#### Import circular shape file around colony #####

radius <- readOGR("Shape files/Circle 50km.shp")

# plot
ggplot(combined_df, aes(x=long, y=lat))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  geom_raster(aes(fill=bath))+
  coord_cartesian()+
  scale_fill_continuous(trans = "reverse")+
  geom_polygon(data = radius, aes(x = long, y = lat, group = group),color = "black", fill = NA)
  
#### Crop raster to extent of shapefile ####

IOM_bath <- mask(combined, radius)
plot(IOM_bath)

#### Extract raster data to tracking file ####

# Import tracking data to data frame "Kit"

# Create spatial points object
Kit_sp <- Kit
coordinates(Kit_sp) <- ~ Longitude + Latitude

# Extract raster data

Kit$Depth <- raster::extract(IOM_bath, Kit_sp, method = 'simple')

#### Sample random points from raster ####

# samples without replacement
# size = number of points to sample
# na.rm = T <- doesn't sample NA values
# xy = T <- return lat and long of random points
random <- as.data.frame(sampleRandom(IOM_bath, size = 500, na.rm = T, xy = T))
head(random)
