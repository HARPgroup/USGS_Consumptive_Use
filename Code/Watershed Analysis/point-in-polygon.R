# point-in-polygon.R
#
# R script intended as a demonstration of simple point-in-polygon
# operations using the over() method in the sp package.
#
# Input Data
#  * CSV table of (fictionalized) brown bear sightings in Alaska, each
#    containing an arbitrary ID and spatial location specified as a
#    lat-lon coordinate pair. These are derived from occurrence data
#    obtained via GBIF search (http://data.gbif.org/occurrences/search.htm),
#    but with query results subsequently anonymized, associated spatial
#    coordinates randomly fuzzed, and records arbitrarily shuffled and
#    subset for demonstration purposes.
#  * Polygon shapefile containing the boundaries of US National Parks
#    greater than 100,000 acres in size. This is version 1.3.0 of the
#    Parks and Protected Lands dataset obtained from
#    naturalearthdata.com.
# 
# Workflow
#  1. Read in the bear sightings data and tell R to treat it as a set of
#     spatial points.
#  2. Read in the National Park polygons.
#  3. Identify which points lie within one of the National Parks.
#  4. For each point, get the name of the containing park (if any), and
#     add it to the bear sighting data table.
#  5. Write results to file, in both CSV and ESRI Shapefile formats,
#     and draw a map of the bear sightings and parks.
# 
# Output
#  * CSV table similar to the input dataset, but with an additional
#    column specifying the park (if any) in which the bear was sighted.
#  * Point shapefile identical to the CSV, but in a format more amenable
#    to direct manipulation in a GIS.
#  * Map visualization.
#
# Notes
#  * The 'over' method used here has replaced the now-deprecated
#    'overlay' method used in previous incarnations of this demo script.
#    This new method is quite similar in spirit, but differs somewhat in
#    the combinations of arguments it accepts, and in the form of the
#    answer returned in each case.
# 
# Author: Jim Regetz
# Last modified: 08-Sep-2011
# National Center for Ecological Analysis and Synthesis (NCEAS),
# http://www.nceas.ucsb.edu/scicomp

require(sp)
require(rgdal)
require(maps)

# read in bear data, and turn it into a SpatialPointsDataFrame
bears <- read.csv("bear-sightings.csv")
coordinates(bears) <- c("longitude", "latitude")

# read in National Parks polygons
parks <- readOGR(".", "10m_us_parks_area")

# tell R that bear coordinates are in the same lat/lon reference system
# as the parks data -- BUT ONLY BECAUSE WE KNOW THIS IS THE CASE!
proj4string(bears) <- proj4string(parks)

# combine is.na() with over() to do the containment test; note that we
# need to "demote" parks to a SpatialPolygons object first
inside.park <- !is.na(over(bears, as(parks, "SpatialPolygons")))

# what fraction of sightings were inside a park?
mean(inside.park)
## [1] 0.1720648

# use 'over' again, this time with parks as a SpatialPolygonsDataFrame
# object, to determine which park (if any) contains each sighting, and
# store the park name as an attribute of the bears data
bears$park <- over(bears, parks)$Unit_Name

# draw a map big enough to encompass all points (but don't actually plot
# the points yet), then add in park boundaries superimposed upon a map
# of the United States
plot(coordinates(bears), type="n")
map("world", region="usa", add=TRUE)
plot(parks, border="green", add=TRUE)
legend("topright", cex=0.85,
    c("Bear in park", "Bear not in park", "Park boundary"),
    pch=c(16, 1, NA), lty=c(NA, NA, 1),
    col=c("red", "grey", "green"), bty="n")
title(expression(paste(italic("Ursus arctos"),
    " sightings with respect to national parks")))

# now plot bear points with separate colors inside and outside of parks
points(bears[!inside.park, ], pch=1, col="gray")
points(bears[inside.park, ], pch=16, col="red")

# write the augmented bears dataset to CSV
write.csv(bears, "bears-by-park.csv", row.names=FALSE)

# ...or create a shapefile from the points
writeOGR(bears, ".", "bears-by-park", driver="ESRI Shapefile")

