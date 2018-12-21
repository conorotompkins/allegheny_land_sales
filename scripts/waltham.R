library(tidyverse)
library(janitor)
library(ggmap)
library(ggrepel)

source("scripts/load_parcel_sales.R")

api_key <- "AIzaSyBTJxUwvLUY3GiJgJ5cBSLEb_n1TPRjJPE"
ggmap::register_google(key = api_key)

theme_set(theme_bw())

options(scipen = 999, digits = 4)

df %>% 
  count(parid, sort = TRUE)

df %>% 
  count(propertyzip, sort = TRUE)

df %>% 
  count(propertycity, sort = TRUE)

df %>% 
  count(munidesc, sort = TRUE)

df %>% 
  count(saledesc, sort = TRUE)

#df %>% 
#  count(saledesc, saledate) %>% 
#  ggplot(aes(saledate, n, color = saledesc)) +
#  geom_point()

#df %>% 
#  ggplot(aes(price, fill = saledesc, color = saledesc)) +
#  geom_density()

#multi_sale <- df %>% 
#  count(parid) %>% 
#  filter(n > 1) %>% 
#  pull(parid)
source("scripts/load_parcel_geometry.R")

parcel_geometry <- parcels %>% 
  inner_join(df, by = c("PIN" = "parid"))

nrow(parcel_geometry)

#test %>% 
#  count(PIN, sort = TRUE)

dormont <- get_map(location= "3263 Waltham Ave, Dormont, PA", crop = F,
                maptype="toner",
                source="stamen",
                zoom=17)

dormont <- ggmap(dormont)
dormont


ours <- parcel_geometry %>% 
  filter(munidesc == "Dormont",
         price < 400000) %>% 
  mutate(ours = case_when(propertyaddressstreet == "WALTHAM" & propertyhousenum == 3263 ~ "ours",
                          propertyaddressstreet != "WALTHAM" | propertyhousenum != 3263 ~ "not_ours"))

centroids <- ours %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble()

ours <- bind_cols(ours, centroids)

ours %>% 
  count(ours, sort = TRUE)

ours %>% 
  ggplot(aes(fill = price)) +
  geom_sf() +
  scale_fill_viridis_c()

ours %>% 
  ggplot(aes(X, Y, color = ours)) +
  geom_point()

ours_flag <- ours %>% 
  select(ours, X, Y) %>% 
  filter(ours == "ours") %>% 
  st_set_geometry(NULL)

waltham <- dormont +
  #annotate(geom = "text", x = 1331694, y = 391260, label = "Ours") +
  geom_sf(data = ours, aes(fill = price, color = ours), lwd = .5, inherit.aes = FALSE) +
  coord_sf(crs = 4326) +
  scale_fill_viridis_c("Sale price") +
  scale_color_manual("", values = c("white", "red")) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())
ggsave(waltham, filename = "waltham.png", device = "png")