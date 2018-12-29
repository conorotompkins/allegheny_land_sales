library(tidyverse)
library(janitor)

df <- read_csv("data/parcel_data.csv", progress = FALSE) %>% 
  clean_names() %>% 
  select(-geom) %>% 
  mutate(munidesc_asmt = str_replace(munidesc_asmt, " - PITTSBURGH", ""))
