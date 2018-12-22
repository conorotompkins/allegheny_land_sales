library(tidyverse)
library(sf)

file <- "data/shapefiles/AlleghenyCounty_Parcels201812.shx"
file
parcels <- st_read(file)

nrow(parcels)

parcels <- parcels %>% 
  mutate(valid = st_is_valid(.))

parcels %>% 
  ggplot(aes(CALCACREAG)) +
  geom_density()

parcels %>% 
  ggplot(aes(Shape_Leng, Shape_Area)) +
  geom_point()

plot <- parcels %>% 
  filter(valid,
         CALCACREAG > 50) %>% 
  ggplot() +
  geom_sf(aes(fill = CALCACREAG)) +
  scale_fill_viridis_c()

plot
