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

test <- parcel_geometry %>% 
  filter(valid,
         neighdesc_asmt == "PITTSBURGH URBAN")

plot <- test %>% 
  ggplot() +
  geom_sf(aes(fill = classdesc_asmt), color = NA)
plot

df %>% 
  count(neighdesc_asmt, sort = TRUE) %>% 
  View()

df %>%
  count(classdesc_asmt, count = TRUE) %>% 
  View()
df %>% 
  count(neighdesc_asmt, classdesc_asmt, sort = TRUE) %>% 
  arrange(neighdesc_asmt, desc(n)) %>% 
  View()
