# Packages ----------------------------------------------------------------

library(stars)
library(sf)
library(tidyverse)


# Notes -------------------------------------------------------------------

# https://r-spatial.github.io/stars/articles/stars3.html

# Some of the tidyverse verbs that are available in the package dplyr can be applied to the stars objects.

utils::methods(class = "stars")

# tidy data is based on the distinction between rows and columns where the different verbs let you work with each of this parts of the data frame. For the stars object is helpful to associate dimensions as rows (being possible to apply slice() and filter() based on this dimensions) while the columns would be the different attributes (pull(), select(), mutate(), transmute() and rename() work on them).


# stars object ------------------------------------------------------------


# loading the image in stars format
"tif/L7_ETMs.tif" %>% 
  system.file(package = "stars") %>% 
  read_stars() -> x

# x is a dataset that contains a single attribute (values from the ETM sensor in the L7 mission) in thrre possible dimensions: long(x), lat(y) and the 6 bands


# slice() -----------------------------------------------------------------

# slices a SUB-ARRAY out of the cube specifying the dimension on which to act and the slice number (permite obtener resultados reduciendo las dimensiones basado en un índice de posición).

x %>% slice(
  # dimension
  band,
  # slice by index (6th band)
  index = 6
) -> x6

# This way, we slice out a single band out of the band dimension. The new dataset maintains the single attribute but decreases in terms of dimension.



# filter() ----------------------------------------------------------------

# filter(), al igual que slice(), permite seleccionar dentro de dimensiones pero esto lo realiza según una EVALUACIÓN de estos valores según una condición y no por índice de posición. Cada una de las dimensiones puede ser empleada en distintas evaluaciones lógicas

x %>% filter(x > 289000, x < 291000, band > 3) -> x7

# the subarray is created based on the x coordinates (similar to masking) and the band value. If we put band == 3 this will not drop the band dimension (it will have an unique one).
# Applying filter may have limitations on stars object with rectilinear, curvilinear or simple feature geometries, for such cases using regular [] selection or st_crop may be an alternative.


# pull() ------------------------------------------------------------------

# pull() in the tidyverse lets you extract a single column out of the data frame. Similarly, pull() for the stars object lets you pull out an array from the attributes based on the index position of it.

x %>% pull(1) -> x8

dim(x)
dim(x8)

class(x)
class(x8)

# pull() is equivalent to
x[[1]]

# or
x$L7_ETMs.tif


# mutate() and transmute() ------------------------------------------------

# mutate() permits the creation of new attributes based on the "mutation" of existing ones

x %>% mutate(band2 = 2 * L7_ETMs.tif) -> x2

# transmute() is similar to mutate() with the exception that the latter does keep the original attribute while transmute deletes it.
x %>% transmute(band2 = 2 * L7_ETMs.tif)



# rename() ----------------------------------------------------------------

# rename() lets you rename the attribute

x %>% dplyr::rename(etm = L7_ETMs.tif)

# select() ----------------------------------------------------------------

# select() lets you keep the attributes specified

x2 %>% select(band2) -> x9


# replace_na() ------------------------------------------------------------

# reemplazar NAs con valores específicos

x %>% replace_na(replace = list(1))


# geom_stars() ------------------------------------------------------------

# geom_stars() is a ggplot2 geom function that accepts stars objects as its data argument and...
#* sets up the raster or vector spatial coordinates as plot dimensions
#* the first attribute as the fill variable
#* allows for downsampling (without choosing a suitable downsampling level)
#* chooses between using geom_raster, geom_rect and geom_sf depending on whether the geometry is regular, rectilinear or has vector geometries

ggplot() +
  geom_stars(data = x) +
  coord_equal() +
  facet_wrap(~ band) +
  theme_void()










