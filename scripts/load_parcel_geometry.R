library(tidyverse)
library(sf)
library(lwgeom)

file <- "data/shapefiles/AlleghenyCounty_Parcels201904/AlleghenyCounty_Parcels201904.shx"
file
parcels <- st_read(file)

nrow(parcels)

#parcels <- parcels %>% 
#  mutate(valid = st_is_valid(.))

parcels <- parcels %>% 
  st_make_valid()

parcels <- parcels %>% 
  mutate(valid = st_is_valid(.))