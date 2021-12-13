#### Spatial Analysis in R workshop
#### Western Colorado University
#### November 10, 2021
#### Author: Hanna McCaslin (hanna.mccaslin@colostate.edu)
####      (some code/data provided by Mevin Hooten, Trevor Hefley, & Kyle Horton)


## Workflow 2: rgdal, sp, and raster
# This is the older approach to spatial R, but what many folks (incl. me) are most familiar with
# Will be 'retired' in 2023

########################
# Objectives for this module:
#   1. Load and visualize spatial data in R 
#   2. Basic manipulation of different spatial data types 
#   3. Extract spatial data to use in analysis

#### Note: I recommend setting working directory to "Spatial WCU"

####
####  Intro to spatial data & analysis in R
####
library(maps) #One (of many) packages with built in spatial data

map("state")

map("state", "Colorado")
map("state", c("Colorado", "Wyoming", "Utah", "Idaho"))

data(us.cities) #attach built-in data "us.cities"
CO.cities <- us.cities[c(322,247,360,728,192),] #pick out a few cities
CO.cities
CO.cities$name <- c("Fort Collins","Denver","Grand Junction","Pueblo","Colorado Springs")

map("state","Colorado",xlim=c(-111,-100),ylim=c(36.5,41.5))  #map with a few cities
map.cities(CO.cities, country="CO",pch=17,cex=1.5,label=TRUE)


####
####  Import spatial data
####
# Import shape file 
library(rgdal)
colorado_shp <- readOGR(dsn = "shapefiles/colorado/colorado.shp")
plot(colorado_shp)

# Import KML file
mtlion_kml <- readOGR(dsn = "mtlion.kml")
plot(mtlion_kml, pch = 1, cex = 0.75, col = "blue")

plot(colorado_shp)
points(mtlion_kml, pch = 1, cex = 0.75, col = "blue")

# Import points data
mtlion <- read.table("mtlion.csv", header=T, sep=",")  
View(mtlion)
points(mtlion$Longitude, y = mtlion$Latitude, pch = 1, cex = 0.3, col = "red")

# Base R plot
plot(colorado_shp)
points(mtlion$Longitude, y = mtlion$Latitude, pch = 1, cex = 0.3, col = "red")

# Let's zoom in on the mountain lion locations - use the data frame version for now
# Use the range of the lat/lons
range(mtlion$Longitude)
range(mtlion$Latitude)

plot(colorado_shp, xlim = c(-105.76, -105.25), ylim = c(39.24,39.75))
points(mtlion$Longitude, y = mtlion$Latitude, pch = 1, cex = 0.3, col = "red")

####
####  ggplot2/ggmap primer
####
library(ggplot2)

## ggplot2
ggplot() + 
     geom_point(aes(x=long, y=lat), data = CO.cities)

ggplot(data = CO.cities) + 
     geom_point(aes(x=long, y=lat, color=pop, size = pop)) 

map1 <- ggplot() + 
     geom_polygon(data = colorado_shp, aes(x=long, y=lat,group=group), fill=NA, color="gray") + 
     geom_point(data = CO.cities, aes(x=long, y=lat, color=pop, size = pop)) +
     labs(x = "Latitude", y = "Population") + 
     theme_classic() 
map1

ggplot(aes(x=Longitude, y=Latitude), data = mtlion) + 
     geom_point() + 
     geom_path()

map1 + 
     geom_point(aes(x=Longitude, y=Latitude), data = mtlion, color="orange") + 
     geom_path(aes(x=Longitude, y=Latitude), data = mtlion, color="orange")

##ggmap
library(ggmap)
register_google(key="") #add you google api key

colorado <- get_map("colorado", zoom = 6, color = "bw", legend = "topleft")

ggmap(colorado) + 
     geom_point(aes(x=long, y=lat, color=pop, size = pop), data = CO.cities) +
     labs(x = "Latitude", y = "Population") 


####
####  crs 
####
library(raster) # package for working with raster data (next), contains crs() function
library(sp) # package that contains data structures and functions for working with POINT & POLYGON spatial data

crs(colorado_shp) 
crs(mtlion_kml) 

# Project data into another projection (for making nice maps, matching different data up)
wgs <- crs(colorado_shp)
albers_projection <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

colorado_shp_aea <- spTransform(colorado_shp, albers_projection) #transform from one crs to another 
crs(colorado_shp_aea)
plot(colorado_shp_aea)
plot(colorado_shp)


####
####  Points & polygons data 
####
# Let's turn mtlion dataframe (csv) into spatial data 
mtlion_pts <- mtlion
coordinates(mtlion_pts) <-  ~Longitude + Latitude # ~ x coord + y coord

mtlion_pts
head(mtlion_pts@data)
head(mtlion)

# Need to define its crs() - **this is different from changing projection**
crs(mtlion_pts)
crs(mtlion_pts) <- wgs 
     # there are other options for setting this 
     # crs(mtlion_kml), proj string itself

# Plot
plot(colorado_shp, xlim = c(-105.76, -105.25), ylim = c(39.24,39.75))
points(mtlion$Longitude, y = mtlion$Latitude, pch = 1, cex = 0.3, col = "red") # from the dataframe
points(mtlion_pts, cex=0.2, col = "blue") #same, syntax different


####
####  Raster
####

# Basic raster 
r <- raster(ncol=3, nrow=3, xmn=0, xmx=3, ymn=0, ymx=3)
r

#populate the raster with a draw of values from a poisson distribution  
r[] = 1:9
res(r) #check the resolution of the raster cells
ncell(r) #check that the number of cells matches the defined size

plot(r, axes=F)
text(r)
r[1]
r[,1]
r[1,]

#Misc about this raster
cellStats(r, stat = mean)
cellStats(r, stat = max)

# Raster example 
nlcd <- raster("fort_collins.tif") 
crs(nlcd)
res(nlcd) #resolution in the units 
ncell(nlcd) # total number of cells in the raster

plot(nlcd) # may take a few seconds

#####
# Exercise: Plot CSU on top of this raster map of fort collins by turning it into a 
# spatial object with a crs and project 
#####

csu <- c(-105.081631,40.575047) # first try...
points(csu) # Why doesn't this work? 

csu <- data.frame(x=-105.081631, y=40.575047 )

## Add your code here ##


### Turn this into map of NLCD of Fort Collins with typical color scheme ###
# NLCD landcover types are categories, so first we'll convert the raster to 
# categorical data, and then associate each category with the names of the
# landcover type

summary(values(nlcd))

nlcd <- as.factor(nlcd)
unique(nlcd)

#add names of categories to raster layer
land_cover <-  levels(nlcd)[[1]]

## (This is code I copy-paste whenever I'm working with NLCD data)
#these are the names of the landcover types. The order here matters and aligns with factor order, i.e., 11, 21, 22...
land_cover[,"landcover"] <- c("Open Water", "Developed, Open Space","Developed, Low Intensity",
                             "Developed, Medium Intensity","Developed, High Intensity",
                             "Barren Land","Deciduous Forest", "Evergreen Forest","Mixed Forest",
                             "Shrub/Scrub","Grassland/Herbaceous","Pasture/Hay","Cultivated Crops",
                             "Woody Wetlands","Emergent Herbaceous Wetlands")
levels(nlcd) <- land_cover
print(land_cover)

#assign a color for each landcover type. This is a fairly standard cover scheme for NLCD. Again, the order matters. 
land_col <-  c("#4f6d9f", "#decece", "#d29b85", "#de3021", "#9d1f15",
             "#b2afa5", "#7aa76d", "#336338", "#c0cb99","#cebb89", "#edecd0",
             "#ddd75c", "#a67538", "#bfd7eb", "#7ba3be")

library(rasterVis)
nlcd_plot <- levelplot(nlcd, col.regions=land_col, xlab="", ylab="", main="Greater Fort Collins NLCD 2011")
nlcd_plot

####
####  Cropping 
####

# Crop to a smaller area using another shapefile
citypark_shp <- readOGR(dsn = "shapefiles/City_Park/City_Park.shp")
crs(citypark_shp)
crs(nlcd) #these are not identical, but very close, so will work for our use

plot(nlcd)
plot(citypark_shp, add = T)

nlcd_crop <- crop(nlcd, citypark_shp)
plot(nlcd_crop)
plot(citypark_shp, add = T)
levelplot(nlcd_crop, col.regions=land_col, xlab="", ylab="", main="City Park NLCD")

#Another handy cropping note
ext <- drawExtent() #this is handy but not reproducible unless you save the values
ext
ext_save <- c(ext[1:4])
# -763709.4 -761818.8 1988203.5 1990094.0
nlcd_crop2 <- crop(nlcd, ext)

plot(nlcd_crop2)

# Write this new raster out as an image file
writeRaster(nlcd_crop, filename="nlcd_cropped.tif", format = "GTiff")


####
####  'Case study'
####
## We are going to investigate the relationship between elevation and mountain 
## lion home range

# Read in elevation raster
elevation <- raster("elevation.tif")
plot(elevation)

# Project mtn lion data to match raster 
crs(elevation)
crs(mtlion_pts) #could also use kml, but regular dataframes don't have crs's

mtlion_proj <- spTransform(mtlion_pts, crs(elevation))

# Plot the elevation raster and mountain lion points
plot(elevation)
points(mtlion_proj) 

# crop to a smaller raster around the points
     ## Crop using a method of your choosing -fill in code here ##

plot(elev_crop)
points(mtlion_proj)

# To investigate if there a relationship between mountain lion home range and elevation, 
# we'll extract the elevation at each of the mountain lion points

# extract spatial data each mountain lion location
mtlion$elevation <- extract(elev_crop, mtlion_proj) 
View(mtlion) # have to be a little careful to keep the elevations associated with the right observations

# Now can use these values as a variable in analysis

# Compare the mt lion elevations to a random sample of elevations nearby
# Draw 1000 random elevations
ncell(elev_crop)

set.seed(11)
rand <- sample(1:1332, 1000,replace=F)
elev_rand <- elev_crop[rand]

# Create 'used' (1 for lion, 0 for no lion) & create data frame
mtlion$used <- 1
elev_rand <- cbind.data.frame(elevation=elev_rand, used=rep(0,1000))
dat <- rbind(mtlion[,4:5], elev_rand)
View(dat)

# Does it look like mt lion may be 'selecting' for certain elevations at this scale?
boxplot(dat$elevation ~dat$used)


## create a ggmap of lion ## 
cent <- c(mean(mtlion$Longitude), mean(mtlion$Latitude))

gg2 <- get_map(cent, zoom=10)
gg2

ggmap(gg2) + 
     geom_point(data = mtlion, aes(x=Longitude, y=Latitude), color="orange", size=0.8) + 
     theme(legend.position="none")

#####################
####  Resources  #### 
#####################
## This is a great resource that goes more in-depth on some topics:
# http://files.zevross.com/workshops/spatial/slides/html/0-deck-list.html

## One super ggmap tutorial:
# https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf     

########################################
### Code for Exercises ###

# plot csu 
coordinates(csu) <- ~x+y
crs(csu) <- wgs

csu_aea <- spTransform(csu, crs(nlcd))
points(csu_aea)

# crop elevation
elev_crop <- crop(elevation, extent(mtlion_pts))


