# Packages ----------------------------------------------------------------

library(sf)
library(tidyverse)

# Notes -------------------------------------------------------------------

# https://r-spatial.github.io/sf/articles/sf2.html

# GDAL stands for "The Geospatial Data Abstraction Library" and is considered to be an important tool for spatial data (it reads and writes vector data from and to practically every file format, or database, or significance). Package sf reads and writes using GDAL by the functions st_read and st_write.

# The data model GDAL uses needs:
#* a DATA SOURCE (file, directory or database)
#* a LAYER (single geospatial dataset inside a file, directory or table)
#* specification of a DRIVER (which format)
#* DRIVER-SPECIFIC reading or writing data sources or layers


# Using st_read -----------------------------------------------------------

(nc <- system.file("shape/nc.shp", package = "sf") %>% 
   st_read())

# A single argument is used to find both the datasource and the layer. This works when the datasource contains a SINGLE layer. In case there are more layers than one, the first layer is returned but a message and warning are given

# List the driver and layer in a datasource with st_layers

st_layers(dsn = "PG:dbname=postgis")

# Now, a particular layer can be read by using the argument "layer" in st_read

st_read("PG:dbname=postgis", layer = "sids")

# Character data is read by default as factor (conventions of base R). For retrieving character data as character vector the argument stringAsFactors can be set to FALSE

(fname <- system.file("shape/nc.shp", package = "sf"))

st_read(dsn = fname, stringsAsFactors = F)

# Alternatively, a user can set the global option stringAsFactors

base::options(stringAsFactors = F)


# Using st_write ----------------------------------------------------------

# To write a simple feature object we need at least two arguments, the object and a filename

st_write(obj = nc, dsn = "sf/nc1.shp")

# The file name is taken as the data source name. st_write needs to guess the driver.

# Guessing a driver for output is based on the datasource name, either from it's extension (.shp: ESRI Shapefile) or its prefix (PG: PostgreSQL)


# Conversion to other formats: WKT, WKB, sp -------------------------------

# Conversion to and from well-known text (WNT)
# The usual form in which we see simple features printed is well-known text

st_point(c(0, 1))
st_linestring(matrix(0:9, ncol = 2, byrow = T))

# We can create well-known text strings explicitly using st_as_text

(str <- st_linestring(matrix(0:9, ncol = 2, byrow = T)) %>% 
    st_as_text())

# And convert it back from WKT by using st_as_sfc (list-column of simple features geometries)

st_as_sfc(str)

# Conversion to and from well-known binary (WNB)
(str <- st_linestring(matrix(0:9, ncol = 2, byrow = T)) %>% 
    st_as_binary())

# The object returned is either a list with raw vectors or a single raw vector. These can be converted into a hexadecimal character string vector using rawToHex

rawToHex(str)

# Converting WKB back to sf uses the same function as WKT; st_as_sfc

st_as_sfc(str)

# Conversion to and from sp
# Spatial objects as maintained by package sp can be converted into simple feature objects or list-column simple feature geometries by st_as_sf and st_as_sfc

methods(st_as_sf)

methods(st_as_sfc)












