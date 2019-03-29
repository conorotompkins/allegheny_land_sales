library(tidyverse)
library(janitor)
library(broom)
library(sf)
library(scales)
library(gganimate)

options(scipen = 999, digits = 4)

theme_set(theme_bw())

#read in data
df <- read_csv("data/parcel_data_allegheny_county.csv", progress = FALSE) %>% 
  clean_names() %>% 
  select(-geom)

df %>% 
  ggplot(aes(yearblt_asmt)) +
  geom_density() +
  geom_rug()

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
  select(pin, x, y, yearblt_asmt) %>%
  mutate(yearblt_asmt = as.integer(yearblt_asmt)) %>% 
  filter(!is.na(yearblt_asmt),
         yearblt_asmt > 1800) %>% 
  st_set_geometry(NULL)

p <- parcel_geometry_cleaned %>% 
  ggplot(aes(x, y, color = yearblt_asmt, group = pin)) +
  geom_point(alpha = .3, size = .1) +
  scale_color_viridis_c("Year structure was built") +
  theme_void() +
  theme(axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(title = "Allegheny County land parcels",
       subtitle = "Year built: {frame_along}",
       caption = "@conor_tompkins, data from @WPRDC")
p <- p + transition_reveal(yearblt_asmt)

anim_save("output/images/pittsburgh_all_parcels_yearblt.gif", p)

p
