library(tidyverse)
library(sf)
library(gganimate)

theme_set(theme_bw())

#file <- "data/shapefiles/AlleghenyCounty_Parcels201812.shx"
#file
#parcels <- st_read(file)

source("scripts/load_parcel_sales_combined.R")
source("scripts/load_parcel_geometry.R")

parcel_geometry <- parcels %>% 
  left_join(df, by = c("PIN" = "pin"))

nrow(parcels)

parcels <- parcels %>% 
  mutate(valid = st_is_valid(.))

#parcels %>% 
#  ggplot(aes(CALCACREAG)) +
#  geom_density()

#parcels %>% 
#  ggplot(aes(Shape_Leng, Shape_Area)) +
#  geom_point()

plot <- parcel_geometry %>% 
  select(valid, yearblt_asmt) %>% 
  filter(valid,
         !is.na(yearblt_asmt)) %>% 
  ggplot() +
  geom_sf()

plot
