# Packages ----------------------------------------------------------------

library(stars)
library(sf)
library(tidyverse)


# Link --------------------------------------------------------------------

# https://r-spatial.github.io/stars/articles/stars4.html

# NO COMPLETO, MUY ESPECIFICO E INNECESARIO PARA ESTE MOMENTO.

# Grid type ---------------------------------------------------------------

# Regular grids ----

m <- matrix(1:20, nrow = 5, ncol = 4)

dim(m) <- c(x = 5, y = 4) # Putting names in the dimension values

s <- st_as_stars(m) # Se observa un arreglo espacial con un tamaño de pixel de 1 y dimensiones (5,4) con valores de indices iniciales (1 y 1) y finales (5 y 4) para x e y, respectivamente.

image(s, text_values = T, axes = T)

# IMPORTANTE: las filas (5) son mapeadas en la primera dimensión (coordenada x) mientras que las columnas (4) van a la segunda dimensión (coordenada y)

# Al observar el gráfico, podemos observar una grilla regular donde el tamaño de las celdas es constante siempre

# Muchos sets de datos presentan un eje y que va desde norte a sur (de abajo hacia arriba). Esto puede corregirse de acuerdo a una alteración al signo de delta

# attr(s, "dimensions")[[2]]$delta = -1
st_dimensions(s)[[2]]$delta <- -1

image(s, text_values = T, axes = T)


# Raster attributes, rotated and sheared grids ----------------------------


