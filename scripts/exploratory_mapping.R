library(tidyverse)
library(janitor)
library(broom)
library(sf)
library(scales)

options(scipen = 999, digits = 4)

theme_set(theme_bw())

source("scripts/load_parcel_sales_combined.R")
source("scripts/load_parcel_geometry.R")

parcel_geometry <- parcels %>% 
  inner_join(df, by = c("PIN" = "pin"))

centroids <- parcel_geometry %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble()

centroids %>% 
  ggplot(aes(X, Y)) +
  geom_point()

parcel_geometry_cleaned <- bind_cols(parcel_geometry, centroids) %>% 
  clean_names() %>% 
  select(pin, x, y, yearblt_asmt, classdesc_asmt) %>% 
  filter(classdesc_asmt == "RESIDENTIAL",
         !is.na(yearblt_asmt)) %>% 
  st_set_geometry(NULL)

plot1 <- parcel_geometry_cleaned %>% 
  ggplot(aes(x, y, color = yearblt_asmt)) +
  geom_point(alpha = .3, size = .1) +
  scale_color_viridis_c("Year house was built") +
  theme_void() +
  theme(axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(title = "City of Pittsburgh housing parcels",
       subtitle = "Residential parcels",
       caption = "@conor_tompkins, data from @WPRDC")
#plot1
ggsave("output/images/pittsburgh_parcels_year_built.png", plot1, height = 12, width = 12, dpi = 300)

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