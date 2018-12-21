library(tidyverse)
library(janitor)

url <- "https://data.wprdc.org/datastore/dump/5bbe6c55-bce6-4edb-9d04-68edeb6bf7b1"

df <- read_csv(url, progress = FALSE) %>% 
  clean_names()