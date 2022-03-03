# Packages ----------------------------------------------------------------

library(stars)
library(sf)
library(tidyverse)

# Notes -------------------------------------------------------------

# El paquete stars es una nueva versión del paquete raster (y del más reciente paquete terra) que permite trabajar con datos espaciales raster de manera tidy (con un tibble ó data frame de base), por lo que las herramientas del tidyverse son aplicadas.
# stars (Spatiotemporal Arrays Raster and Vector Data Cubes) provee de la infraestructura para trabajar con data cubes; arreglos de datos con al menos 2 dimensiones espaciales que forman el raster tesseldado. Vector data cubes pueden ser incluidos los cuales son representados por feature geometries (paquete sf).
# Estos arreglos espaciotemporales son guardados en objetos del tipo stars.

# Funciona en base a dimensiones y atributos. Mientras las dimensiones pueden ser del tipo long(x), lat(y), tiempo, bandas o sensores los atributos son aquellos valores que fueron medidos. De esta manera, los atributos son los valores de interés y el cómo estos se arreglen espaciotemporalmente depende de las dimensiones.


# INTRODUCCIÓN ------------------------------------------------------------


# https://r-spatial.github.io/stars/articles/stars1.html

# Métodos (funciones) disponibles
utils::methods(class = "stars")

# READ SATELLITE IMAGES
# (system.file() permite determinar la dirección COMPLETA de un archivo disponible en un paquete)
tif <- base::system.file("tif/L7_ETMs.tif",
                   package = "stars")

# read_stars() puede leer la totalidad de bandas pertenecientes a un set de datos o un subseteo de estas (en un arreglo stars único)

x <- stars::read_stars(tif) 
# stars con 3 dimensiones: long, lat y valor además de presentar 1 atributo (L7_ETM.tif)

# Mientras que los ATRIBUTOS son caracterizados de acuerdo a estadígrafos de posición y dispersión las DIMENSIONES, aquel aspecto de los datos que indica como estos se arreglan de manera espaciotemporal y/o en bandas, pueden ser descritas a partir de distintas propiedades.

# Dimensiones espaciales, espectrales y temporales.

# ¿Qué significa c/u de los campos designados para las dimensiones?
{
  # from: índice inicial del arreglo (número inicial de donde empieza el raster)
  # to: índice final del arreglo
  # dim: se refiere a la diferencia entre to y from, es decir, cuantos cuadros o celdas posee la dimensión x e y por separado
  # offset: the start value for this dimension (pixel boundary), if regular
  # delta: Se refiere al tamaño de la celda o pixel para la determinada dimension (ssi es regular)
  # refsys: Sistema de referencia en proj4string
  # point: logical, whether cells refer to points or intervals+¿
  # values: the sequence of values for this dimension (e.g., geometries), if regular
}

# ¿Por qué se presenta un offset negativo?
{
  # Para este set de datos (y tambien es generalizado) se observa un offset negativo para la dimensión y: esto significa que valores consecutivos del arreglo presentan una tendencia decreciente en los valores de y (los indices de celda incrementan de arriba hacia abajo, en la dirección opuesta al eje Y).
}
# Gráfico
plot(x, 
     # Agregar las coordenadas a c/u de los gráficos
     axes = T)


# SWITCHING BETWEEN ATTRIBUTES AND DIMENSIONS -----------------------------


# Pasar de dimensión a atributo: La dimensión "band" se transforma a un atributo de los datos, es decir, ahora se presentan las 6 bandas como atributos (aumento con respecto al 1 anterior) mientras que la dimensión pasa de 3 a 2. Esto significa que los valores de las bandas pasan a ser posibles de graficar mientras pero se pierde la posibilidad de ordenar el dataset en el espacio de acuerdo a las 6 dimensiones contenidas anteriormente.

x.spl <- base::split(x = x,
                     f = "band")

# Pasar de atributos a dimensión.
base::merge(x = x.spl,
            names = "attributes")

# Los sets de atributos y dimensiones perdieron sus nombres, fueron otorgados ciertos nombres por default por lo que sería apropiado cambiarlos

x.spl %>% 
  base::merge() %>% 
  # Attribute name
  setNames("L7_ETMs.tif") %>% 
  # Dimension value
  st_set_dimensions(which = 3,
                    values = paste0("band", 1:6)) %>% 
  # Dimension names
  st_set_dimensions(names = c("x", "y", "band"))


# SUBSETTING --------------------------------------------------------------


# ¿Cual es la estructura de datos de stars? Los objetos stars corresponden a una lista de arreglos, lo que se muestra en la consalo al ser llamados corresponde a una tabla de metadatos que describe las dimensiones. Debido a esto, la estracción y asignación como uns estructura de lista funciona de manera adecuada.

# stars (full)
x
class(x)

# stars, únicamente con el primer atributo
x[1]
class(x[1])

# arrays
x$L7_ETMs.tif
x[[1]]
class(x[[1]])

dim(x[[1]])

# Creación de un nuevo atributo (2do)
x$two <- 2 * x[[1]]
x$two
x[[2]]

# The stars subset operator [] works a bit different. x[1, 2, 3, 4]; the first argument selects attributes, second argument selects the first dimension (columns or x) while the third argument selects the second dimension (rows or y), lastly, the fourth argument refers to the third dimension that, in this case, is defined by the different bandas that are inside the cube.

x[
  # Segundo atributo
  "two", 
  # primeras 10 columnas (coordenadas x)
  1:10, 
  # todas las columnas (coordenadas y)
  ,
  # bandas 2 a 4
  2:4
  ]


# SUBSETTING & CROP -------------------------------------------------------


# Cuando un objeto stars es subseteado [] con otro objeto sd, sfc o bbox la acción se traduce en un crop

sf::st_point(x = c(293749.5, 9115745)) %>%
  # aplicar un buffer al punto para generar un círculo
  sf::st_buffer(dist = 400) %>%
  # Agrupar los datos y convertirlos en un objeto MULTYPOLYGON con un crs igual a las imágenes L7 (x) anteriores
  sf::st_sfc(crs = sf::st_crs(x)) %>%
  assign(x = "circle",
         envir = .GlobalEnv)

# Crop
x[circle]

plot(x[circle][, , , 1], reset = F)
plot(circle, col = NA, border = "red", add = T, lwd = 2)



# OVERVIEWS ---------------------------------------------------------------

# When rasters contain overviews we can read them at a lower resolution speeding the analysis. The course resolution versions are added by using the average resampling method to compute values based on block of pixels. We can access these overvies when reading a raster adding the argument "options".

x1 <- read_stars(tif, options = c("OVERVIEW_LEVEL=1"))
x2 <- read_stars(tif, options = c("OVERVIEW_LEVEL=2"))
x3 <- read_stars(tif, options = c("OVERVIEW_LEVEL=3"))

dim(x1)
dim(x2)
dim(x3)

par(mfrow = c(1, 4), 
    # margins(b, l, t, r)
    mar = rep(0.2, 4))

image(x[,,,1])
image(x1[,,,1])
image(x2[,,,1])
image(x3[,,,1])

# By applying the incresing levels of overview the result in the image is a decrease in spatial resolution in the form of bigger pixel size.



# READING A RASTER TIME SERIES: NetCDF ------------------------------------

# stars object are valuable when we read raster time series models like for example in a NetCDF file

system.file("nc/bcsd_obs_1999.nc", package = "stars") %>% 
  # read_ncdf()
  read_stars() -> w

# Tha dataset presents 2 attr (precipitation and temperature) and 3 dimensions (long, lat and time). De esta manera, el tiempo pasa a ser una dimensión con unidades e incremento característico. 

# Si analizamos en detalle, podemos observar que los atributos poseen unidades (y que de hecho la correspondiente a temperatura es inadecuada). Además



# READING DATASETS FROM MULTIPLE FILES ------------------------------------

# read_stars() is a vectorized function! that means can put a vector of paths to different datasets and it will work

x <- c(
  "avhrr-only-v2.19810901.nc",
  "avhrr-only-v2.19810902.nc",
  "avhrr-only-v2.19810903.nc",
  "avhrr-only-v2.19810904.nc",
  "avhrr-only-v2.19810905.nc",
  "avhrr-only-v2.19810906.nc",
  "avhrr-only-v2.19810907.nc",
  "avhrr-only-v2.19810908.nc",
  "avhrr-only-v2.19810909.nc"
)

install.packages("starsdata", repos = "http://pebesma.staff.ifgi.de", type = "source")

file_list <- system.file(paste0("netcdf/", x), package = "starsdata")

y = read_stars(file_list, quiet = T)

# The dataset has 4 attributes; sea surface temperature (sst), anom, err, ice (%), and 4 dimensions; long, lat, zlev or depth and time.

# We can select the sst and drop the singular depth (zlev) dimension using 

y %>% dplyr::select(sst) %>% abind::adrop() -> z

# GRAPHIC WITH ggplot2
# Necesitamos cambiar el tipo de datos que se encuentra en la dimensión tiempo (a un character). Para eso ocuparemos las funciones st_set_dimensions() y st_get_dimension_values().

# Obtener los valores de la dimensión tiempo (3)
st_get_dimension_values(.x = z, which = 3)

# Crear un nuevo objeto con el cambio asociado de la dimensión tiempo
z1 <- st_set_dimensions(
  # Object to retrieve dimensions from
  .x = w,
  # index or name of the dimension (in this case it is time)
  which = 3,
  values = as.character(st_get_dimension_values(.x = w, which = 3))
)

# grafico
ggplot() +
  geom_stars(data = z1[1], alpha = 0.8, downsample = c(10, 10, 1)) +
  facet_wrap("time") +
  # Ratio de long_lat fijo (fuerza una determinada representación física del mapa)
  ggplot2::coord_equal()


# WRITTING STARS OBJECT TO DISK -------------------------------------------

# We can create a stars object to disj by using write_stars(), Currently writes only a single attribute.

stars::write_stars(adrop(w[1]), "w.tif")

# A pesar de que sólo se puedan guardar tifs con un sólo atributo debemos recordar de que existen formas en las que podemos convertir multiples atributos en una única dimensión (merge()) tal como lo hicimos al intervacmbiar dimensiones y atributos.



# CROPPING A RASTER'S EXTENT ----------------------------------------------

system.file("nc/test_stageiv_xyt.nc", package = "stars") %>% 
  # curvilinear grid
  read_ncdf(curvilinear = c("lon", "lat")) -> prec

plot(prec) # Gives error about unique breaks

# Eliminar NAs, 0s, y dar un gran numero de breaks 

qu_0_omit = function(x, ..., n = 22){
  if(inherits(x, "units"))
    x = units::drop_units(na.omit(x))
  c(0, quantile(x[x > 0], seq(0, 1, length.out = n)))
}

# Slice
prec %>% 
  # Index rows by integer location
  slice(index = 17, along = "time") -> prec_slice

nc <- sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf"), "nc.gpkg")

plot(prec_slice, border = NA, breaks = qu_0_omit(prec_slice[[1]]), reset = F)
plot(st_geometry(nc), add = T, reset = F, col = NA, border = "red")

# CROPPED SELECTION

prec_slice[nc] # the two datasets are not in the same crs and they need to be


nc %>% st_transform(
  crs = st_crs(prec_slice)
  ) -> nc_d

# Perform a crop of a stars object by a sf
prec_slice[nc_d]
st_crop(prec_slice, nc_d)

prec_slice %>% 
  st_crop(nc_d) %>% 
  plot(border = NA, breaks = qu_0_omit(prec_slice[[1]]), reset = F)

nc %>% 
  # Get, set, replace or rename geometry
  st_geometry() %>% 
  plot(add = T, reset = F, col = NA, border = "red")



# VECTOR DATA CUBE EXAMPLE ------------------------------------------------

# stars arrays, like tbl_cube, HAVE NO LIMITS TO THE NUMBER OF DIMENSIONS THEY HANDLE. An example of this is the origin-destination (OD) matrix, by time and travel mode.

# OD: space x space x travel mode x time x time

# We create a 5-dimensional matrix of traffic between regions by day, by time of day and by travel mode. Having day and time of day each as individual dimensions is an advantage when we want to compute patterns over the day, for a certain period. (CREAMOS NUESTRAS PROPIAS DIMENSIONES Y SUS PARÁMETROS)

nc <- system.file("gpkg/nc.gpkg", package="sf") %>% 
  st_read()

# 100 polygons: O (origin) and D (destination) regions
nc %>% st_geometry() -> to -> from

mode <- c("car", "bike", "foot") # travel mode

day = 1:100 # arbitrary

# udunits database from /usr/share/xml/udunits2.xml
units(day) <- units::as_units("days since 2015-01-01")
hour <- units::set_units(0:23, h) # hour of day

dims <- st_dimensions(origin = from, destination = to, mode = mode, day = day, hour = hour)

n <- dim(dims)

n %>% prod() %>% rpois(lambda = 10) %>% array(dim = n) -> traffic # simulated traffic counts

list(traffic = traffic) %>% 
  st_as_stars(dimensions = dims) -> st

# THIS ARRAY CONTAINS THE SF GEOMETRIES OF ORIGIN AND DESTINATION. With this we can plot every slice directly without additional table joins. If we want to represent such an array as a tbl_cube the sf geometry dimensions need to be replaced by indexes.

# We can make analysis of the array we have at hand by applying functions of the dplyr package. For example, to compute mean bike traffic by hour of day...

st %>% 
  as.tbl_cube.stars() %>% 
  filter(mode == "bike") %>% 
  group_by(hour) %>% 
  summarise(traffic = mean(traffic)) %>% 
  as_tibble() -> b

require(ggforce) # for plotting a units variable

ggplot() +
  geom_line(data = b, aes(x = hour, y = traffic))




# Extra
LandUse_cau %>% 
  st_as_stars() %>% 
  as.tbl_cube.stars() %>% 
  as_tibble() %>% 
  filter(!is.na(LandCover_rf_2018_v4.3)) %>% 
  st_as_stars() -> l

ggplot() +
  geom_stars(data = l) +
  ggplot2::coord_equal()


LandUse_cau %>% as("SpatialPixelsDataFrame") %>% as.data.frame() -> data

ggplot() +
  geom_tile(data = data, aes(x = x, y = y, fill = LandCover_rf_2018_v4.3))




