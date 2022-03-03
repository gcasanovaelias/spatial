# Packages ----------------------------------------------------------------

library(stars)
library(sf)
library(tidyverse)

# Notes -------------------------------------------------------------------

# https://r-spatial.github.io/stars/articles/stars2.html

# QUEDARON COSAS PENDIENTES DEBIDO A QUE NO SE PUDIERON LEER DATOS

# If the array od the data or the imagery is too large (or for other reasons you want to work with smaller chunks od data than the files in which they come) we can apply different procedures.


# Reading chunks, change resolution, select bands (low-level interface) -------------------------

# read_stars() has an argument called RasterIO which controls how a dataset is read. By default, all pixels and all bands are read in memory.

tif <- system.file("tif/L7_ETMs.tif", package = "stars")

rasterio <- list(
  # x from
  nXOff = 6,
  # y from
  nYOff = 6,
  # x dim
  nXSize = 100,
  # y dim
  nYSize = 100,
  # bands to read
  bands = c(1, 3, 4)
)

x <- read_stars(tif, RasterIO = rasterio)

dim(x)

plot(x)

# Observamos 3 bandas distintas (dimension bands) dentro del atributo L7_ETMs con un tamaño de pixel (delta) igual a 28.5.

# Compared to...
tif %>% read_stars() -> x

x %>% st_dimensions()

dim(x)

plot(x)

# We see that:
#* (1) delta, offset (x/y coordinates of origin) of the grid remains the same
#* (2) from and to reflect the new area
#* (3) dim(x) reflects the new size
#* (4) only three bands were read

# Reading at a different resolution ---------------------------------------

# Reading at a lower (but also higher!) resolution can be done by setting nBufXSize and nBufYSize

# Reading at a lower resolution (nBufSize < nSize): Disminuir la resolucion espacial

rasterio <- list(
  nXOff = 6,
  nYOff = 6,
  nXSize = 100,
  nYSize = 100,
  nBufXSize = 20,
  nBufYSize = 20,
  bands = 1
)

tif %>% read_stars(RasterIO = rasterio) -> x

x

dim(x)

plot(x)

# ¿Qué significa una dim x = 20? El valor de dim corresponde a la diferencia entre los valores numéricos de to y from.

# We see that:
#* (1) el tamaño del pixel (delta) ha incrementado con un factor de 5 debido a que los argumentos de nBufXSize u nBufYSize son más pequeños (20) que los valores indidcados en nXSize y nYSize (100).
#* (2) offset coordinates se mantienen igual
#* (3) from y to reflejan la nueva area pero relacionada con los nuevos valores de delta.

# Reading at a higer resolution (nbufSixe > nSize): Incrementar la resolucion espacial 

rasterio <- list(
  nXOff = 6,
  nYOff = 6,
  nXSize = 3,
  nYSize = 3,
  nBufXSize = 100,
  nBufYSize = 100,
  bands = 1,
  # default sampling method is narest neighbour
  resample = "cubic_spline"
)

tif %>% read_stars(RasterIO = rasterio) -> x

x

dim(x)

plot(x)

# What other methods are allowed for parameter resample?
#* nearest_neighbour, bilinear, cubi, cubic_spline, lanczos, average, mode, Gauss. These methods are implemented in GDAL. 


# Stars proxy objects (higher level interface) -----------------------------------------------------

# stars proxy object take another approach: upon creation they contain no data at all but only pointers to where the data can be read. DATA IS ONLYE READ WHEN IT IS NEEDED AND ONLY AS MUCH AS NEEDED: if we plot a proxy object the data is read at the resolution of pixels on the screen rather than at the native resolution. For example, if we have a 10000 x 10000 S2 level 1C image...

# stars objects are "in-memory" while the stars_proxy ones are "out-of memory".

p <- read_stars(tif, proxy = T) # only read metadata

class(p)

p

# La información disponible en un objeto stars-proxy es parcial en relación a la disponible en los objetos stars. Sólo aparecen detalladas las dimensiones pero sólo se indica el nombre y la cantidad de atributos que se encuentran dentro del objeto. A cambio de esto, la activación de un objeto stars proxy permite la lectura rápida y superficial de los datos (casi instantáneamente) al focalizarse unicamente en el metadata (de las dimensiones; st_dimensions()). 

plot(p)

# No todas las funciones que son aplicables a los objetos stars pueden ser aplicadas a los objetos stars proxy pero son comparativamente más rápidas en estas últimas. Sólo se leen los datos necesarios para realizar una determinada acción.

methods(class = "stars_proxy")

# Selección de atributos en los objetos stars-proxy (análogo a lo que se podía realizar con los objetos stars)

names(p) # Nombre de los atributos

p["L7_ETMs.tif"]
p %>% dplyr::select(L7_ETMs.tif)

# Selección de un área en un objeto stars-proxy
# stars_proxy objects can be cropped or select a rectangular region based on a spatial object. This can be done by passing a bbox, sf, sfc or stars object from which the bounding box will be taken.

st_bbox(c(xmin = 291000, ymin = 9112500, xmax = 294000, ymax = 9115000)) -> bb

psub <- p[bb]

st_dimensions(psub)

class(psub)

plot(psub, reset = F)

bb %>% st_as_sfc() %>% plot(add = T, lwd = .5, border = "red")
