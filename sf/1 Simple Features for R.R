# Packages ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(cubelyr)
library(units)

# Notes -------------------------------------------------------------------

# https://r-spatial.github.io/sf/articles/sf1.html

# Simple features or simple feature access refers to a formal standard (ISO 19125-1:2004) that describes how objects in the real world can de represented in computers, with emphasis on the spatial objects. It also describes how such objects can be stored in and retrieved from databases, and which geometrical operations should be defined for them. 

# Although R has well-supported classes for storing spatial data (sp) and interfacing to the above mentioned environments (rgdal, rgeos) it has so far lacked a complete implementation of simple features makind conversions at times convoluted, inefficient or incomplete. The package sf tries to fill this gap and aims at succeeding sp in the long run.

# WHAT IS A FEATURE?
# A feature is a thing or an object in the real world that often consist of other objects. This means that a set of features can form. A forest, city, satellite image pixel or a complete image can be a feature.

# These features have a geometry describing where on Earth it is located. ALong with the geometry features have attributes which describe other properties.

# "A simple feature is defined by the OpenGIS Abstract specification to have both spatial and non-spatial attributes"

# DIMENSIONS
# All geometries are composed of points which are coordinates in a 2, 3 or 4 dimensional space. In addition to X and Y coordinates there are two optional additional dimensions: Z (altitude) and M (measure associated with the point). The latter one is rarely used. With this, the four possible cases of dimensions in a sf are; XY (2D), XYZ (3D), XYM (3D) and XYZM (4D). 

# SIMPLE FEATURE GEOMETRY TYPES
# The 7 most common simple features types are:
#* (1) POINT: single point with zero-dimensional geometry.
#* (2)LINESTRING: sequence of points connected by straight, non-self intersecting line pieces; 1D geometry.
#* (3) POLYGON: 2D geometry (positive area); sequence of points form a closed, non-self intersecting ring. The first ring denotes the exterior ring, zero or more subsequente rings denotes holes in this exterior ring.
#* (4) MULTIPOINT: Set of points. A MULTIPOINT is simple if no two POINTS in it are equal.
#* (5) MULTILINESTRING: Set of linestrings
#* (6) MULTIPOLYGON: Set of polygons
#* (7) GEOMETRYCOLLECTION: Set of geometries of ANY type except GEOMETRYCOLLECTION

# There are in total 17 geometry types in sf, the remaining 10 are rarer but with increasingly find implementations.

# COORDINATE REFERENCE SYSTEM
# Coordinates can only be places in Earth's surface when their coordinate reference system (CRS) is known.

# HOW SIMPLE FEATURES IN R ARE ORGANIZED
# Package sf represents simple features as native R objects. Similar to the database PostGIS, all functions and methods in sf that operate on spatial data are prefixed by "st_", which refers to spatial type, this makes them easily findable by command-line completion. As attributes are typically stored in data.frame objects (or the very similar tbl_df) we will also store feature geometries in a data.frame column. Since geometries are not-single valued, they are put in a list-column (each list element holding the simple feature geometry). There are 3 classes used to represent simple features:
#* (1) sfg: the feature geometry of an individual simple feature (SIMPLE FEATURE GEOMETRY).
#* (2) sfc: the list-column with geometries for each feature (record) which is composed of the series of sfgs (SIMPLE FEATURE GEOMETRY LIST-COLUMN)
#* (3) sf: the table (data.frame) with feature attributes (columns) and the feature geometries and that contains sfc, The simple feature is a single record, or data.frame row, consisting of attributes and geometry.


# SF: OBJECT WITH SIMPLE FEATURES -----------------------------------------

system.file("shape/nc.shp", package = "sf") %>% 
  st_read() -> nc

# The short report printed when calling a sf object gives: 
#* (1) the file name
#* (2) the driver (ESRI Shapefile)
#* (3) mentions that there are 100 features (objects, records represented as rows) and 14 fields (attributes, represented as columns)

nc 

class(nc)

# An sf object extends (and is) a data.frame, tbl and even a tbl_df

nc %>% as.data.frame()
nc %>% as_tibble()

# the single list-column with geometries
attr(nc, "sf_column")

# Methods for sf objects
methods(class = "sf")


# SFC: SIMPLE FEATURE GEOMETRY LIST-COLUMN --------------------------------


# The column in the sf data.frame that contains the geometries is a list of class sfc. We can retrieve the geometry lit-column in this case by different methods:

nc$geometry

nc[[15]]

nc_geom <- sf::st_geometry(obj = nc) # More general way

# Geometries are printed in abbreviated form but we can view a complete geometry by selecting it


nc_geom %>% pluck(1)

# The way this is printed is called well-known text and is part of the standards. The word of the geometry (MULTYPOLYGON) is followed by three parenthesis because it can consist of multiple polygons in the form of MULTIPOLYGON(POL1, POL2) where POL1 might consist of an exterior ring and zero or more interior rings, as of POL1: (EXT1, HOLE1, HOLE2). Sets of coordinates are held together with parentheses so we get ((crds_ext)(crds_hole1)(crds_hole2)) where crds_ is a comma-separated set of coordinates of a ring. 

par(mar = c(0,0,1,0))

plot(nc[1], reset = F) # reset = F: We want to add to a plot with a legend
plot(nc[1,1], col = "grey", add = T)

# Geometry columns have their own class
sf::st_geometry(obj = nc) %>% class()

# And the method that can be applied to sfc are

methods(class = "sfc")

# Coordinate reference systems (st_crs and st_transform) are discussed in the section on reference systems. 

# st_as_wkb and st_as_text convert geometry list-columns into well-known-binary or well-known-text, 



# st_bbox retrieved the coordinate bounding box

# Attributes include
attributes(nc_geom)

# MIXED GEOMETRY TYPES
# There are 2 "special" types of geometrues: GEOMETRYCOLLECTION and GEOMETRY. 

# GEOMETRYCOLLECTION indicates that each of the geometries may contain a mix of geometry types

mix <- st_sfc(st_geometrycollection(list(st_point(1:2))),
              st_geometrycollection(list(st_linestring(matrix(1:4,2)))))

class(mix)

# Still, the geometries here are of a single type

# In contrast, GEOMETRY indicates that the geometries in the geometry list-column are of varying types

mix <- st_sfc(st_point(1:2), st_linestring(matrix(1:4,2)))

class(mix)

# GEOMETRYCOLLECTION and GEOMETRY are fundamentally different. GOEMETRY is a superclass without instances while GEOMETRYCOLLECTION is a geometry instance. GEOMETRY list-columns occur when we read in a data source with a mix of geometry types. GEOMETRYCOLLECTION is a single feature's geometry.


# SFG: SIMPLE FEATURE GEOMETRY --------------------------------------------

# Simple feature geometry (sfg) objects carry the geometry for a SINGLE feature (e.g. a point, linestring or polygon).

# Simple feature geometries are implemented as R native data structures, using the following rules:
#* (1) a single POINT is a numeric VECTOR
#* (2) a set of points in a LINESTRING or a ring of a POLYGON is a MATRIX, each ROW containing a point
#* (3) any other set is a list

# Creator functions are rarely used in practice since we typically bulk read and write spatial data. They are useful for ilustration.

# CREATING SINGLE POINT WITH VARYING DIMENSIONS

x <- st_point(x = c(1,2), dim = "XY")
str(x)

x <- st_point(x = c(1,2,3), dim = "XYZ")
str(x)

x <- st_point(x = 1:3, dim = "XYM")
str(x)

x <- st_point(x = 1:4, dim = "XYZM")
str(x)

# Drop Z or M geometries
st_zm(x = x, drop = T, what = "ZM")

# This means that we can represent 2, 3 or 4 dimensional coordinates. All geometry inherit from sfg (simple feature geometry) but also have a type (e.g. POINT) and a dimension (e.g. XYM) class name

# With the exception of the POINT which has a single point as geometry, the remaining six common single simple features (single records, or rows in a data.frame) are created like this.

# MULTIPOINT
p <- rbind(c(3.2,4), c(3,4.6), c(3.8,4.4), c(3.5,3.8), c(3.4,3.6), c(3.9,4.5))

(mp <- st_multipoint(p)); plot(mp)

# LINESTRING
s1 <- rbind(c(0,3),c(0,4),c(1,5),c(2,5))

(ls <- st_linestring(s1)); plot(ls)

# MULTILINESTRING
s2 <- rbind(c(0.2,3), c(0.2,4), c(1,4.8), c(2,4.8))
s3 <- rbind(c(0,4.4), c(0.6,5))

(mls <- st_multilinestring(list(s1, s2, s3))); plot(mls)

# POLYGON
p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
p2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))

(pol <- st_polygon(list(p1, p2))); plot(pol)

# MULTIPOLYGON
p3 <- rbind(c(3,0), c(4,0), c(4,1), c(3,1), c(3,0))
p4 <- rbind(c(3.3,0.3), c(3.8,0.3), c(3.8,0.8), c(3.3,0.8), c(3.3,0.3))[5:1,]
p5 <- rbind(c(3,3), c(4,2), c(4,3), c(3,3))

(mpol <- st_multipolygon(list(list(p1, p2), list(p3, p4), list(p5)))); plot(mpol)

# GEOMETRYCOLLECTION
(gc <- st_geometrycollection(list(mp, mpol, ls))); plot(gc)

# ... geometries can also be empty
(st_geometrycollection() -> x)


# WELL-KNOWN TEXT, WELL-KNOWN BINARY, PRECISION ---------------------------

# well-known text (WKT) and well-known binary (WKB) are two encoding for simple feature geometries 

matrix(10:1, 5) %>% st_linestring() -> x

# Well-known text (WKT)
st_as_text(x)

# Coordinates as floating point numbers, moving large amounts of information as text is slow and imprecise. For that reason we use well-known binary (WKB) encoding
st_as_binary(x)

# WKT and WKB can both be transformed back into R native objects by

st_as_sfc("LINESTRING (10 5, 9 4, 8 3, 7 2, 6 1)")[[1]]

st_as_sfc(structure(list(st_as_binary(x)), class = "WKB"))[[1]]

# PRECISION
# One of the attributes of a geometry list-column (sfc) is the precision: a double number that, when non-zero, causes some rounding during conversion to WKB, which might help certain geometrical operations succed that would otherwise fail due to floating point representation


# Reading and writing -----------------------------------------------------

# Reading
system.file("shape/nc.shp", package = "sf") %>% 
  # read_sf(): more silent
  st_read(quiet = F) -> nc

# Writing
# write_sf(): silently overwrites
st_write(obj = nc, 
         dsn = "sf/nc.shp",
         # overwrite
         delete_layer = T)

# Driver specific options
# returns a data.frame listing available drivers and their metadata: names, wheteher a driver can write, and whether is a raster and/or vector driver
st_drivers()



# Coordinate reference systems and transformations ------------------------

# CRS are the measurement units for coordinates specifying which location on Earth a particular coordinate pair refers to. sfc objects (geometry list-columns) have two attributes to store a CRS: epsg and proj4string. This implies that all geometries in a geometry list-column must have the same CRS.

# proj4string is a generic string-based description of a CRS that is understood by the PROJ library.It defines projection types and (often) defines parameter values for particular projections, and hence can cover an infinite amount of different projections. This library (also used by GDAL) provides functions to convert or transform between different CRS. epsg is the integer ID for a particular, known CRS that can be resolved into a proj4string. Some proj4string values can resolved back into their corresponding espg, but this does not always work.

# proj4strings
# "+proj=longlat +datum=NAD27 +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat"

# espg
# ESPG: 32719

# The importance of having epsg values stored with data besides proj4string values us that the epsg refers to particular, well-known CRS, whose parameters may change (improve) over time; fixing only the proj4string may remove the possibility to benefit from such improvementes, and limits some of the provenance of datasets, but may help reproducibility.

# Coordinate reference system transformations can be carried out using st_transform

st_transform(x = nc, crs = 3857)

# Conversion, including to and from sp ------------------------------------

methods::showMethods("coerce", classes = "sf")

methods(st_as_sf)

methods(st_as_sfc)

# anticipate that sp::CRS will expand proj4strings:
p4s <- "+proj=longlat +datum=NAD27 +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat"

st_crs(nc)
st_crs(nc) <- p4s

# anticipate geometry column name changes:
nc.sp <- as(nc, "Spatial")
class(nc.sp)

nc2 <- st_as_sf(nc.sp, forceMulti = F)
all.equal(nc, nc2)

# Why are they not equal?
# The Spatial objects only support MULTILINESTRING and MULTIPOLYGON, LINESTRING and POLYGON geometries are automatically coerced into their MULTI form. When converting Spatial into sf, if all geometries consist of a single POLYGON, this and otherwise all geometries are returned as a MULTI. Argument forceMulti = T will override this, and create MULTIPOLYGON in all cases. For lines the situation is identical.


# Geometrical operations --------------------------------------------------

# The standars for simple features access defines a number of geometrical operations. 

# VALID AND SIMPLE GEOMETRIES
# st_is_valid and st_is_simple return boolean indicating whether a geometry is valid or simple, respectively.

st_is_valid(nc)
st_is_simple(nc)

# DISTANCE BETWEEN GEOMETRIES
# st_distance returns a dense numeric matrix with distanes between geometries.

nc[2:3, ] # Is a way of subsetting the 2nd and third row (simple features) maintaining all the columns (including the list-column with sfc)

st_distance(nc[c(1, 4, 22),], nc[c(1, 33, 55, 56),])

# st_relate returns a character matrix with the DE9-IM values for each pair of geometries

st_relate(nc[1:5, ], nc[1:4,])

# LOGICAL COMMANDS
# The next set of commands return a sparse matrix with matching (TRUE) indexes, or a full logical matrix
#* st_intersects
#* st_disjoint
#* st_touches
#* st_crosses
#* st_within
#* st_contains
#* st_overlaps
#* st_equals
#* st_covers
#* st_covered_by
#* st_equals_exact
#* st_is_within_distance

st_intersects(nc[1:5, ], nc[1:4,])

st_intersects(nc[1:5, ], nc[1:4, ], sparse = F)

# NEW GEOMETRIES COMMANDS
# The next set of functions return new geometries as a result
#* st_buffer
#* st_boundary
#* st_convexhull
#* st_union_cascaded
#* st_simplify
#* st_triangulate
#* st_polygonize
#* st_centroid
#* st_segmentize
#* st_union

# BUFFER
nc[c(1, 5, 14), ] %>% 
  st_geometry() %>% 
  assign(x = "geom", envir = .GlobalEnv) %>% 
  st_buffer(dist = 30000) %>% 
  assign(x = "buf", envir = .GlobalEnv)

plot(buf, border = "red")
plot(geom, add = T)

# Commands st_intersection, st_union, st_difference, st_sym_difference return new geometries that are a function of PAIRS of geometries

a <- st_polygon(list(cbind(c(0,0,7.5,7.5,0),c(0,-1,-1,0,0))))
b <- st_polygon(list(cbind(c(0,1,2,3,4,5,6,7,7,0),c(1,0,.5,0,0,0.5,-0.5,-0.5,1,1))))

plot(a, yli = c(-1, 1))
plot(b, add = T, border = "red")

(i <- st_intersection(a,b))
plot(i, add = T, col = "green", lwd = 2)


# Non-simple and non-valid geometries -------------------------------------

# Non-simple geometries are for instance self-intersecting lines; non-valid geometries are for instance polygons with slivers (no clean union) or self-intersections.

x1 <- st_linestring(cbind(c(0,1,0,1),c(0,1,1,0)))
x2 <- st_polygon(list(cbind(c(0,1,1,1,0,0),c(0,0,1,0.6,1,0))))
x3 <- st_polygon(list(cbind(c(0,1,0,1,0),c(0,1,1,0,0))))

par(mfrow = c(1, 3))
plot(x1)
plot(x2)
plot(x3)

st_is_simple(st_sfc(x1, x2, x3))

st_is_valid(st_sfc(x1, x2, x3))


# Units -------------------------------------------------------------------

# Where possible, geometric operations such as st_distance, st_length, st_area report results with a units attribute appropiate for the CRS

(a <- st_area(nc[1,]))

attributes(a)

# The units package can be used to convert between units

set_units(a, km^2) # result in square kilometers

set_units(a, ha) # result in hectares

# The result can also de stripped of their attributes if it's needed
as.numeric(a)


# How attributes relate to geometries -------------------------------------

# The standars documents about simple features are very detailed about the geometric aspects of features, but say nearly nothing about attributes, except that their values should be understood in another reference system (their units of measurement). 

# Not all attributes are the same and it depends on the variables that they measure and the context.

# For geometries that have non-zero size (positive length or area) attributes values may refer to every sub-geometry (every point) or may al well summarize the geometry.

# Some properties are spatially extensive, meaning that attributes would summed up when two geometries are merged (population is an example) while other are spatially intensive, and should be averaged (population density as an example).

# simple features object of class sf have an agr attribute that points to the attribute-geometry-relationship, how attributes relate to their geometry. It can be defined at creation time.

(system.file("shape/nc.shp", package = "sf") %>%
  st_read(
    agr = c(
      AREA = "aggregate",
      PERIMETER = "aggregate",
      CNTY_ = "identity",
      CNTY_ID = "identity",
      NAME = "identity",
      FIPS = "identity",
      FIPSNO = "identity",
      CRESS_ID = "identity",
      BIR74 = "aggregate",
      SID74 = "aggregate",
      NWBIR74 = "aggregate",
      BIR79 = "aggregate",
      SID79 = "aggregate",
      NWBIR79 = "aggregate"
    )
  ) -> nc)

st_agr(nc)

data("meuse", package = "sp")

(meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, agr = "constant"))

st_agr(meuse_sf)

# When not specified, this field is filled with NA values but if non-NA, it has one of three possibilities:
#* (1) constant: A variable that has a constant value at every location over a spatial extent
#* (2) aggregate: values are summary values (aggregates) over geometry, e.g. population density, dominant land use
#* (3) values identofy the geometry: they refer to (the whole of) this and only this geometry
