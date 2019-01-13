library(tidyverse)
library(janitor)

df <- read_csv("data/parcel_data.csv", progress = FALSE) %>% 
  clean_names() %>% 
  select(-geom) %>% 
  mutate(munidesc_asmt = str_replace(munidesc_asmt, " - PITTSBURGH", "")) %>% 
  mutate(finishedlivingarea_asmt_log10 = log10(finishedlivingarea_asmt),
         lotarea_asmt_log10 = log10(lotarea_asmt),
         price_sales_log10 = log10(price_sales),
         saleprice_asmt_log10 = log10(saleprice_asmt)) %>% 
  select(pin, classdesc_asmt, munidesc_asmt, schooldesc_asmt, neighdesc_asmt, taxdesc_asmt,
         usedesc_asmt, homesteadflag_asmt, farmsteadflag_asmt, styledesc_asmt,
         yearblt_asmt, extfinish_desc_asmt, roofdesc_asmt,  basementdesc_asmt,
         gradedesc_asmt, conditiondesc_asmt, stories_asmt, totalrooms_asmt, bedrooms_asmt,
         fullbaths_asmt, halfbaths_asmt, heatingcoolingdesc_asmt, fireplaces_asmt, 
         bsmtgarage_asmt, finishedlivingarea_asmt, finishedlivingarea_asmt_log10,
         lotarea_asmt, lotarea_asmt_log10, saledate_sales, price_sales, price_sales_log10,
         saleprice_asmt, saleprice_asmt_log10)