library(tidyverse)
library(janitor)
library(broom)
library(sf)
library(scales)

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

parcel_exploratory <- parcel_geometry %>% 
  st_set_geometry(NULL)

parcel_exploratory %>% 
  ggplot(aes(price, fill = instrtypdesc)) +
  geom_histogram() +
  facet_wrap(~instrtypdesc, scales = "free_x")

parcel_exploratory %>% 
  count(parid, sort = TRUE)

parcel_exploratory %>% 
  count(propertyzip, sort = TRUE)

parcel_exploratory %>% 
  count(propertycity, sort = TRUE)

parcel_exploratory %>% 
  count(munidesc, sort = TRUE)

parcel_exploratory %>% 
  count(saledesc, sort = TRUE)

parcel_exploratory %>% 
  count(instrtypdesc, sort = TRUE)

parcel_geometry %>% 
  ggplot(aes(shape_area, price)) +
  geom_point() +
  geom_smooth()