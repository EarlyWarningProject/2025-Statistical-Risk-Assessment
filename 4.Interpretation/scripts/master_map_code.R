
#libraries
library(ggplot2)
library(maptools)   #plotting
library(rgeos)      #simplifying geometry
library(rgdal)      #projecting
library(spdep)      #spatial statistics
library(ggsn)       #scale bars
library(raster)


#color scales
library(viridis)
library(scales)
library(RColorBrewer)
library(shadowtext)

#some common projections
customproj <- list()
customproj$unproj <- CRS("+proj=longlat +datum=WGS84") #default WGS84 projection
customproj$afghanistan <- CRS("+init=epsg:32642")

#ggplot themes
if(!exists("customtheme")) customtheme <- list() #create a theme object if it doesn't exist already (i.e. from basic plots)
customtheme$map <- function() {theme(
    panel.border = element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = 'white')) }
