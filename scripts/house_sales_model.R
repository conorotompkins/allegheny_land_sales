library(tidyverse)
library(scales)
library(caret)

options(scipen = 999, digits = 5)

theme_set(theme_bw())

source("scripts/load_parcel_sales_combined.R")

df <- df %>% 
  filter(classdesc_asmt == "RESIDENTIAL",
         saleprice_asmt > 100,
         str_detect(munidesc_asmt, "Ward")) %>% 
  select(pin, munidesc_asmt, schooldesc_asmt, neighdesc_asmt, taxdesc_asmt,
         usedesc_asmt, homesteadflag_asmt, farmsteadflag_asmt, styledesc_asmt,
         yearblt_asmt, extfinish_desc_asmt, roofdesc_asmt,  basementdesc_asmt,
         gradedesc_asmt, conditiondesc_asmt, stories_asmt, totalrooms_asmt, bedrooms_asmt,
         fullbaths_asmt, halfbaths_asmt, heatingcoolingdesc_asmt, fireplaces_asmt, 
         bsmtgarage_asmt, finishedlivingarea_asmt, lotarea_asmt, price_sales, saleprice_asmt,
         saledate_sales)

df <- df %>% 
  mutate_if(is.character, replace_na, "missing") %>% 
  mutate_if(is.character, as.factor)


df <- df %>% 
  select(munidesc_asmt, bedrooms_asmt, saleprice_asmt) %>% 
  na.omit()

#df <- df %>% 
#  group_by(munidesc_asmt) %>% 
#  mutate(median_sale = median(saleprice_asmt),
#         diff_from_muni_median = saleprice_asmt - median_sale) %>% 
#  ungroup() %>% 
#  select(-median_sale)

#df %>% 
#  count(munidesc_asmt, sort = TRUE) %>% 
#  View()

#findLinearCombos(df)

model <- train(
  saleprice_asmt ~ ., df,
  method = "lm",
  trControl = trainControl(
    method = "repeatedcv", number = 5, repeats = 5, verboseIter = TRUE
  )
)
model
warnings()

