library(tidyverse)
library(scales)
library(caret)

options(scipen = 999, digits = 5)

theme_set(theme_bw())

source("scripts/load_parcel_sales_combined.R")

df <- df %>% 
  filter(classdesc_asmt == "RESIDENTIAL",
         saleprice_asmt > 100,
         str_detect(munidesc_asmt, "Ward"),
         finishedlivingarea_asmt > 0,
         lotarea_asmt > 0) %>% 
  select(pin, munidesc_asmt, schooldesc_asmt, neighdesc_asmt, taxdesc_asmt,
         usedesc_asmt, homesteadflag_asmt, farmsteadflag_asmt, styledesc_asmt,
         yearblt_asmt, extfinish_desc_asmt, roofdesc_asmt,  basementdesc_asmt, 
         heatingcoolingdesc_asmt, gradedesc_asmt, conditiondesc_asmt, stories_asmt, 
         totalrooms_asmt, bedrooms_asmt, fullbaths_asmt, halfbaths_asmt, fireplaces_asmt, 
         bsmtgarage_asmt, finishedlivingarea_asmt_log10, lotarea_asmt_log10, price_sales_log10, 
         saleprice_asmt_log10, saledate_sales)

df <- df %>% 
  mutate_if(is.character, replace_na, "missing") %>% 
  mutate_if(is.character, as.factor)

glimpse(df)


df <- df %>% 
  select(neighdesc_asmt, finishedlivingarea_asmt_log10, lotarea_asmt_log10,
         yearblt_asmt, bedrooms_asmt, fullbaths_asmt, halfbaths_asmt, 
         extfinish_desc_asmt, roofdesc_asmt, basementdesc_asmt, heatingcoolingdesc_asmt,
         gradedesc_asmt, conditiondesc_asmt, saleprice_asmt_log10) %>% 
  na.omit()

glimpse(df)

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
  saleprice_asmt_log10 ~ ., df,
  method = "lm",
  trControl = trainControl(
    method = "repeatedcv", number = 5, repeats = 5, verboseIter = TRUE
  )
)
model
varImp(model)
