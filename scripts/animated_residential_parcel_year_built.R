library(tidyverse)
library(janitor)
library(broom)
library(sf)
library(scales)
library(gganimate)

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
  mutate(yearblt_asmt = as.integer(yearblt_asmt)) %>% 
  filter(classdesc_asmt == "RESIDENTIAL",
         !is.na(yearblt_asmt)) %>% 
  st_set_geometry(NULL)


p <- parcel_geometry_cleaned %>% 
  ggplot(aes(x, y, color = yearblt_asmt, group = pin)) +
  geom_point(alpha = .3, size = .1) +
  scale_color_viridis_c("Year house was built") +
  theme_void() +
  theme(axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(title = "City of Pittsburgh residential housing parcels",
       subtitle = "Year built: {frame_along}",
       caption = "@conor_tompkins, data from @WPRDC")
p <- p + transition_reveal(yearblt_asmt)
anim_save("output/images/pittsburgh_residential_parcels_yearblt.gif")
