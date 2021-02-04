#rm(list = ls())
library(sp)
library(rgdal)
library(viridis)
library(raster)
library(deldir)
library(rgeos)
library(osmar) ## This will let us retrieve information from OpenStreetMap
library(data.table)
bbHalfWidth <- 1000
bbHalfHeight <- 1000
api <- osmsource_api(url = 'https://api.openstreetmap.org/api/0.6/')
trips = fread('../trips.csv')

bbCenterCoord <- c(lon = -71.277977, lat = 46.772500) ## This is a point on Laurier blvd.

bb <- center_bbox(bbCenterCoord[["lon"]], bbCenterCoord[["lat"]] , bbHalfWidth, bbHalfHeight)

ua <- get_osm(bb, source= api)

# If get_osm didn't work, load:

load("localOSMdataLaurier.Rdata") # Creates object ua.

codesForRoadsInMap <- find(ua, way(tags((k == "highway") & (v %in% c("primary", "residential", "motorway", "secondary", "trunk", "tertiary", "motorway_link", "trunk", "primary_link", "secondary_link", "tertiary_link")))))
nodesFromOSM <- as.matrix(subset(ua$nodes$attrs, select = c("lon", "lat")))
rownames(nodesFromOSM) <- ua$nodes$attrs$id

linesAndNodesInMap <- find_down(ua, way(codesForRoadsInMap))
mapFeaturesOfInterest <- subset(ua, ids = linesAndNodesInMap)

# SpatialLinesDataFrame object
mapFeaturesOfInterestAsSp <- as_sp(mapFeaturesOfInterest, "lines")

plot(mapFeaturesOfInterestAsSp)


plot(ua)
