# Packages ----------------------------------------------------------------
library(stars)


# Notes -------------------------------------------------------------------

# https://r-spatial.github.io/stars/articles/stars7.html

# NOT FINISHED. No se pudo realizar la parte final de Supervised Learning ya que los datos empleados no se encontraron (el paquete del que provenían ya no existe)...

# The steps to obtain prediction for all pixels of a stars object:
#* (1) Use full dataset or a sample of it to train the model using as_tibble() or as.data.frame()
#* (2) use stats::predict(star_object, model) to predict for all pixels of stars_object. This uses a stars-wrapper of the predict method for model.

# USUAL version
# model(formula = , data = )
# stats::predict(object = , newdata = )

# RASTER version
# raster::predict(object = , model = )

# STARS version
# stats::predict(object =, model = )

# Analogy of stars object to data.frame -----------------------------------

# ¿Qué ocurre cuando un objeto stars se transforma a un tibble o un data frame? ¿Cómo se organiza el arreglo, sus astributos y dimensiones? R: Cada uno de sus atributos se convierte en una columna única mientras que las distintas dimensiones (que no sean e o y) formarán una columna de identificación (en dicha columna se indicara si el dato pertenece a la banda 1 o 2 con un determinado valor). Además, en la tabla se observan las columnas de las dimensiones x e y que corresponden a la longitud y latitud, respectivamente.

system.file("tif/L7_ETMs.tif", package = "stars") %>% 
  read_stars() -> l7

l7 %>% as_tibble()
l7 %>% as_tibble() %>% pull(band) %>% unique()

# Podríamos requerir que las bandas no esten en un formato long (observaciones o filas x e y duplicadas) sino que los valores de las 6 bandas se distribuyan de acuerdo a una única observación (fila) para cada par x e y.

l7 %>% base::split(f = "band")



# Unsupervised learners ---------------------------------------------------

set.seed(1)

# Principal Components Analysis

system.file("tif/L7_ETMs.tif", package = "stars") %>% 
  read_stars() %>% 
  split() -> r

class(r)
  
r %>% as_tibble() %>% 
  dplyr::select(-c(x,y)) %>% 
  stats::prcomp() -> pc

class(pc)

out <- stats::predict(object = r, model = pc)

# Now, the attributes are no longer the bands but the principal components that were predicted

out %>% 
  merge() %>% 
  plot(breaks = "equal", join_zlim = F)


# k_means clustering ------------------------------------------------------

library(clue)

predict.kmeans <- function(object, newdata, ...) {
  unclass(clue::cl_predict(object, newdata[,-c(1:2)], ...))
}

i <- system.file("tif/L7_ETMs.tif", package = "stars") %>% 
  read_stars(proxy = T) %>% 
  split()

k <- i %>% 
  st_sample(size = 1000) %>% 
  as_tibble() %>% 
  na.omit() %>% 
  dplyr::select(-c(x,y)) %>%
  # K-means (k = 5)
  stats::kmeans(centers = 5)

out <- stats::predict(object = i, model = k)

plot(out, col = sf.colors(5, categorical = T))

