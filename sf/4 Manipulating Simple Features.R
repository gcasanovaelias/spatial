# Packages ----------------------------------------------------------------

library(sf)
library(tidyverse)


# Notes -------------------------------------------------------------------

# Simple features, as the records that come with a geometry (row), can be manipulated by aggregating, summarising and joining feature sets.

# Features or records = feature attributes + feature geometry

# Features are represented by records in a sf object and have feature attributes (all non-geometry fields) and a feature geometry.

# Since sf objects are a subclass of data frame, operations that work on the latter one also work on sf objects.


# Manipulation ------------------------------------------------------------

(nc <- system.file("shape/nc.shp", package="sf") %>% 
  st_read() %>% 
  st_transform(crs = 2264))

nc[1,] # prints the first record (row)

nc %>% as_tibble() %>% select(NWBIR74)

# The geometry is sticky, and gets added automatically unless we coerce it into a data.frame or tibble first

nc %>% select(NWBIR74)

glimpse(nc)


















