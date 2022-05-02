# Packages ----------------------------------------------------------------

library(sf)
library(tidyverse)


# Geometry type transformations for SINGLE geometries -------------------------------------------

# st_cast will convert:
# XX to MULTIXX
st_point(c(1,1)) %>% st_cast("MULTIPOINT")

# MULTIXX to XX (if MULTIXX has length one)
st_multipoint(rbind(c(1,1))) %>% st_cast("POINT")

# MULTIXX to XX if MULTIXX does not have length one but will warn about the loss of information
st_multipoint(rbind(c(1,1), c(2,2))) %>% st_cast("POINT")

# GEOMETRYCOLLECTION of length one to its component
st_geometrycollection(list(st_point(c(1,1)))) %>% st_cast("POINT")


# Geometry type transformations for collections of geometries -------------

# When reading geometries using st_read the type argument can be used to control the class of the returned geometry (handled by the GDAL library)

(shp <- system.file("shape/nc.shp", package="sf"))

shp %>% st_read(quiet = T) %>% st_geometry() %>% class()

shp %>% st_read(quiet = T, type = 3) %>% st_geometry() %>% class()

shp %>% st_read(quiet = T, type = 1) %>% st_geometry() %>% class()


# Coordinate reference systems conversion and transformation --------------

# Getting and setting cordinate reference systems of sf objects

# Create a sfc
(geom <- st_sfc(st_point(c(0,1), st_point(c(11, 12)))))

# Create a sf
(s <- st_sf(a = 15:16, geometry = geom))

st_crs(s)

s1 <- s
# `st_crs<-`(s1, 4326)
st_crs(s1) <- 4326 # EPSG

s2 <- s

st_crs(s2) <- "+proj=longlat +datum=WGS84" # proj4string

# A more pipe-friendly version of st_crs is st_set_crs

s1 %>% st_set_crs(4326)

# Coordinate reference system transformations

# If we change the coordinate reference system from one non-missing value into another non-missing value the CRS is changed without modifying the coordinates but without reprojecting values

# To carry out conversion or transformation we use st_transform

(s3 <- s1 %>% st_transform(3857))



# Geometrical operations --------------------------------------------------

# ALL GEOMETRICAL OPERATIONS st_op(x) or st_op2(x,y) WORK BOTH FOR SF OBJECTS AND SFC OBJECTS X AND Y SINCE THESE OPERATIONS WORK ON THE GEOMETRIES; the non-geometry parts of an sf object are simply discarded.

(b0 <- st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1)))))

(b1 <- b0 + 2)

(b2 <- b0 + c(-0.2, 2))

(x <- st_sfc(b0, b1, b2))

(a0 <- b0 * 0.8)
(a1 <- a0 * 0.5 + c(2, 0.7))
a2 = a0 + 1
a3 = b0 * 0.5 + c(2, -0.5)
y = st_sfc(a0,a1,a2,a3)

# Unary operations --------------------------------------------------------

# st_area returns the area of polygon geometries while st_length the length of line geometries

st_area(x)

st_length(st_sfc(st_linestring(rbind(c(0,0),c(1,1),c(1,2))), st_linestring(rbind(c(0,0),c(1,0)))))


# Binary operations: distance and relate ----------------------------------

# st_distance computes the shortest distance matrix between geometries







