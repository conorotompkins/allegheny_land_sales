library(tidyverse)
library(janitor)
library(broom)
library(sf)

options(scipen = 999, digits = 4)

theme_set(theme_bw())

source("scripts/load_parcel_sales.R")
source("scripts/load_parcel_geometry.R")

parcel_geometry <- parcels %>% 
  inner_join(df, by = c("PIN" = "parid"))

centroids <- parcel_geometry %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble()

parcel_geometry <- bind_cols(parcel_geometry, centroids) %>% 
  clean_names()

parcel_geometry %>% 
  ggplot(aes(shape_area, price)) +
  geom_point() +
  geom_smooth()

fit <- lm(price ~ shape_area + saledesc, data = parcel_geometry)
fit %>%
  tidy() %>% 
  arrange(desc(estimate))

fit %>% 
  glance()

parcel_geometry %>% 
  ggplot(aes(x, y, fill = price)) +
  #geom_point() +
  geom_density_2d()
