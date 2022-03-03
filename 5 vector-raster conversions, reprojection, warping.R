# Library -----------------------------------------------------------------

library(stars)
library(sf)
library(tidyverse)
library(units)

# Notes -------------------------------------------------------------------

# https://r-spatial.github.io/stars/articles/stars5.html

# Tutorial de como los objetos stars pueden cambiar su representación en vector o raster.


# Rasterizing an sf vector object -----------------------------------------

system.file("gpkg/nc.gpkg", package = "sf") %>% 
  read_sf() %>% 
  st_transform(crs = 32119) -> nc

nc %>% 
  # Calcular dist eucli entre par de geometrias
  st_area() %>% 
  # Cambiar las unidades de medida
  set_units(km^2) -> areas_km

# Rasterizing

nc %>% 
  mutate(dens = BIR79 / areas_km) %>% 
  dplyr::select(dens) %>% 
  # Rasterizar
  stars::st_rasterize(dx = 5000, dy = 5000) -> nc.st

plot(nc.st)

# The algorith used is the GDAL rasterize and all options of this utility can be passed to st_rasterize. The geometry of the final raster can be controlled by passing a target bounding box and either the raster dimensions nx and ny, or pixel size by the dx and dy parameters.



# Vectorizing a raster object to an sf object -----------------------------

# Los objetos stars pueden ser convertidos a un sf empleando la función st_as_sf la cual presenta varias opciones.

round(read_stars(system.file("tif/L7_ETMs.tif", package = "stars"))[, 1:50, 1:50, 1:2][[1]]/5)

system.file("tif/L7_ETMs.tif", package = "stars") %>% 
  read_stars() %>% 
  slice(band, index = 1:2,) %>% 
  slice(x, index = 1:50) %>% 
  slice(y, index = 1:50) %>% 
  mutate(L7_ETMs.tif = round(L7_ETMs.tif/5)) %>% 
  pull(L7_ETMs.tif) %>% 
  st_as_stars() -> x
  
plot(x)

# Polygonizing

x %>% stars::st_contour(contour_lines = T, breaks = 11:15) -> l

plot(l, key.pos = 1, pal = sf.colors, lwd = 2, key.length = 0.8)

p <- st_polygonize(l)

plot(p)

# EXPORTING TO POINTS: Alternatively, we can simply export all teh pixels as points and get them either as a wode table woth all bands per point and no replicated point geometries...

x %>% st_as_sf(as.points = T, merge = F) %>% plot()

# ... or as a long table with a single attribute and all points replicated. The additional variable band indicates which band is concerned between the two.

x %>% st_as_sf(as_points = T, merge = F, long = T)

# EXPORTING TO POLYGONS: We can also export the stars object to polygons

# x %>% st_as_sf(as_points = F, merge = F) %>% plot()
x %>% st_as_sf(as_polygons = T, merge = F) %>% plot()

# ... or merge polygons that have identical pixel values
x %>% st_as_sf(as_points = F, merge = T) %>% plot()


# Switching between vector and raster in stars objects ---------------------

# Convert a raster dimension to a vector dimension while keeping other dimensions as they are in a stars object

x %>% 
  # Replace a xy raster to a sf
  st_xy2sfc(as_points = T)

# Podemos observar que las dimensiones x e y se trsnaforman a una dimensión de geometry cuando el raster pasa a ser un sf dentro del objeto stars.



# Reprojecting a raster ---------------------------------------------------

# Reprojecting a raster is no longer a problem if we have in mind that regular and rectilinear grids are special cases of curvilinear ones. This means we can just recompute new coordinates for every raster cell (generally resulting in a curvilinear cell)

nc.st %>% 
  st_transform("+proj=laea +lat_0=34 +lon_0=-60") -> nc.curv

nc.curv

plot(nc.curv, border = NA, graticule = TRUE)

# The dimensionality of the grid didn't change; the same set of raster cells has been replotted in the new CRS but now in a curvilinear grid


# Warping a raster --------------------------------------------------------

# FORMA HABITUAL
# Warping a raster means creating a nw REGULAR grid in a new CRS based on a (usually regular) grid in another CRS.

# First, we create a target grid
nc %>% 
  st_transform("+proj=laea +lat_0=34 +lon_0=-60") %>% 
  st_bbox() %>% 
  st_as_stars() -> newgrid

# that we can use to warp the old one in
nc.st %>% 
  st_warp(newgrid) -> nc_new

plot(nc_new)

# We can see that this causes changes in the dimensionality of the object compared to the original unwarped version.

