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

is_tibble(parcel_exploratory)

#write_csv(parcel_exploratory, "data/parcel_exploratory.csv")