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
  clean_names() %>% 
  distinct()

plot <- parcel_geometry %>% 
  filter(instrtypdesc == "DEED") %>% 
  st_set_geometry(NULL) %>% 
  ggplot(aes(x, y)) +
  geom_point(alpha = .01, size = .1) +
  labs(title = "Allegheny County Deed Sales",
       subtitle = "2013-present") +
  #scale_color_viridis_c("Sale price", labels = scales::dollar) +
  #scale_size_continuous("Sale price", range = c(1, 3), labels = scales::dollar) +
  theme_void() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())

plot
#ggsave(plot, "output/images/alleghent_deed_sales.png", device = "png")