#### Spatial Analysis in R workshop
#### Western Colorado University
#### November 10, 2021
#### Author: Hanna McCaslin (hanna.mccaslin@colostate.edu)
####      (some code/data provided by Mevin Hooten, Trevor Hefley, & Kyle Horton)

## Workflow 1: sf and terra
# This is the newer approach to spatial R, and will eventually become what you have to do

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
## Import shape file ##
# option 1 #
library(sf) 
colorado_shp <- read_sf(dsn = "shapefiles/colorado/colorado.shp")
plot(colorado_shp)
plot(st_geometry(colorado_shp))

ggplot(data=colorado_shp) + 
     geom_sf() 

# option 2 #
library(terra) 
co_shp2 <- vect("shapefiles/colorado/colorado.shp")
plot(co_shp2)

## Import KML file ##
mtlion_kml <- st_read(dsn = "mtlion.kml") #sf package
plot(mtlion_kml, pch = 1, cex = 0.75, col = "blue")

plot(st_geometry(colorado_shp))
plot(mtlion_kml, pch = 1, cex = 0.75, col = "blue", add=T)

# Import points data
mtlion <- read.table("mtlion.csv", header=T, sep=",")  
View(mtlion)
points(mtlion$Longitude, y = mtlion$Latitude, pch = 1, cex = 0.3, col = "red")

#...................
# Question: What is the difference between mtlion_kml and mtlion? 


#...................

# Base R plot 
plot(co_shp2) #terra object
points(mtlion$Longitude, y = mtlion$Latitude, pch = 1, cex = 0.3, col = "red")

# Let's zoom in on the mountain lion locations - use the data frame version for now
# Use the range of the lat/lons
range(mtlion$Longitude)
range(mtlion$Latitude)

plot(co_shp2, xlim = c(-105.76, -105.25), ylim = c(39.24,39.75))
points(mtlion$Longitude, y = mtlion$Latitude, pch = 1, cex = 0.3, col = "red") # non spatial mtlion pts


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
     geom_sf(data = colorado_shp) + 
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
register_google(key="") #add your api key

colorado <- get_map("colorado", zoom = 6, color = "bw", legend = "topleft")

ggmap(colorado) + 
     geom_point(aes(x=long, y=lat, color=pop, size = pop), data = CO.cities) +
     labs(x = "Latitude", y = "Population") 


####
####  crs 
####
crs(colorado_shp) 
crs(mtlion_kml) 

# Project data into another projection (for making nice maps, matching different data up)
# sf method
wgs <- crs(colorado_shp)
albers_projection <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

colorado_shp_aea <- st_transform(x=colorado_shp, crs=albers_projection) #transform from one crs to another 
crs(colorado_shp_aea)
plot(colorado_shp_aea)
plot(colorado_shp)

####
####  Points & polygons data 
####
# Let's turn mtlion dataframe (csv) into spatial data (sf package)
mtlion_pts <- st_as_sf(mtlion, coords = c("Longitude","Latitude"))
mtlion_pts
head(mtlion)

# Need to define its crs() - **this is different from changing projection**
crs(mtlion_pts)
st_crs(mtlion_pts) <- wgs 
     # there are other options for setting this 
     # crs(mtlion_kml), proj string itself

# SpatVector data (terra package)
mtlion_t <- vect(mtlion_pts)
mtlion_t

# Plot
plot(st_geometry(colorado_shp), xlim = c(-105.76, -105.25), ylim = c(39.24,39.75))
points(mtlion$Longitude, y = mtlion$Latitude, pch = 1, cex = 0.3, col = "red") # from the dataframe
plot(mtlion_pts, cex=0.2, col = "blue", add=T) #same, syntax different
points(mtlion_t, cex=0.3, col="green", pch=14)


####
####  Raster
####

# Basic raster 
r <- rast(ncol=3, nrow=3, xmin=0, xmax=3, ymin=0, ymax=3)
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
values(r)
mean(values(r))
max(values(r))

# Raster example 
nlcd <- rast("fort_collins.tif")  

nlcd
res(nlcd) #resolution in the units 
ncell(nlcd) # total number of cells in the raster

plot(nlcd) # may take a few seconds

#####
# Exercise: Plot CSU on top of this raster map of fort collins by turning it into a 
# spatial object with a crs and project 
#####

csu <- c(-105.081631,40.575047) # first try...
points(csu) 

#...................
# Question: Why didn't the point appear on the plot? 


# Write some code to fix the issue:
csu <- data.frame(x=-105.081631, y=40.575047 )

## Add your code here ##



#...................


### Next, turn this into map of NLCD of Fort Collins with typical color scheme ###
# NLCD landcover types are categories, so first we'll convert the raster to 
# categorical data, and then associate each category with the names of the
# landcover type

summary(values(nlcd))

values(nlcd) <- as.factor(values(nlcd))
unique(nlcd)

#add names of categories to raster layer
land_cover <-  levels(nlcd)[[1]]

## (This is code I copy-paste whenever I'm working with NLCD data)
#these are the names of the landcover types. The order here matters and aligns with factor order, i.e., 11, 21, 22...
types <- c("Open Water", "Developed, Open Space","Developed, Low Intensity",
                             "Developed, Medium Intensity","Developed, High Intensity",
                             "Barren Land","Deciduous Forest", "Evergreen Forest","Mixed Forest",
                             "Shrub/Scrub","Grassland/Herbaceous","Pasture/Hay","Cultivated Crops",
                             "Woody Wetlands","Emergent Herbaceous Wetlands")
land_cover <- cbind.data.frame(land_cover, types)

levels(nlcd) <- land_cover$types
print(land_cover)
plot(nlcd)

#assign a color for each landcover type. This is a fairly standard cover scheme for NLCD. Again, the order matters, and I copy-paste
land_col <-  c("#4f6d9f", "#decece", "#d29b85", "#de3021", "#9d1f15",
             "#b2afa5", "#7aa76d", "#336338", "#c0cb99","#cebb89", "#edecd0",
             "#ddd75c", "#a67538", "#bfd7eb", "#7ba3be")

plot(nlcd, col=land_col) #somewhat useful


####
####  Cropping 
####

# Crop to a smaller area using another shapefile
citypark_shp <- read_sf(dsn = "shapefiles/City_Park/City_Park.shp")
crs(citypark_shp)
nlcd #crs match

plot(nlcd)
plot(citypark_shp, add = T)

nlcd_crop <- crop(nlcd, citypark_shp)
plot(nlcd_crop, col=land_col)
plot(st_geometry(citypark_shp), add = T)

# Write this new raster out as an image file
writeRaster(nlcd_crop, filename="nlcd_cropped.tif")


####
####  'Case study'
####
## We are going to investigate the relationship between elevation and mountain 
## lion home range

# Read in elevation raster
elevation <- rast("elevation.tif")
plot(elevation)

# Project mtn lion data to match raster (terra method)
elevation
crs(mtlion_t) #could also use kml, but regular dataframes don't have crs's

mtlion_tproj <- project(mtlion_t, crs(elevation))

# Plot the elevation raster and mountain lion points
plot(elevation)
points(mtlion_tproj) 

# Crop using a method of your choosing to create smaller raster around mtn lion points, called elev_crop 
#...................
##
##fill in code here ##
##


plot(elev_crop)
points(mtlion_tproj)
#...................


## To investigate if there a relationship between mountain lion home range and elevation, 
## we'll extract the elevation at each of the mountain lion points

# extract spatial data each mountain lion location

elev_extr <-  terra::extract(elev_crop, mtlion_tproj) 
mtlion$elevation <- elev_extr$elevation
View(mtlion) # have to be a little careful to keep the elevations associated with the right observations

which(is.na(mtlion)) # check if your crop was large enough

## Now can use these values as a variable in analysis ##

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

## Handy pages for the terra and sf packages:     
# https://rspatial.org/terra/index.html   
# https://tlorusso.github.io/geodata_workshop/sf_package

     
########################################
### Code for Exercises ###

# plot csu 
csu <- st_as_sf(csu, coords = c("x","y"))
st_crs(csu) <- wgs

csu_aea <- st_transform(csu, crs(nlcd))
plot(csu_aea, add=T)

# crop elevation
elev_crop <- crop(elevation, ext(mtlion_tproj))

