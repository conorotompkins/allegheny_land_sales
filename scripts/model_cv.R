library(tidyverse)
library(scales)
library(caret)
library(broom)
library(ranger)
library(rsample)

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
  select(finishedlivingarea_asmt_log10, lotarea_asmt_log10,
         yearblt_asmt, bedrooms_asmt, fullbaths_asmt, halfbaths_asmt, 
         extfinish_desc_asmt, roofdesc_asmt, basementdesc_asmt, heatingcoolingdesc_asmt,
         gradedesc_asmt, conditiondesc_asmt, saleprice_asmt_log10) %>% 
  na.omit()

glimpse(df)


# Prepare the initial split object
df_split <- initial_split(df, prop = .75)

# Extract the training dataframe
training_data <- training(df_split)

# Extract the testing dataframe
testing_data <- testing(df_split)

# Calculate the dimensions of both training_data and testing_data
dim(training_data)
dim(testing_data)

set.seed(42)

# Prepare the dataframe containing the cross validation partitions
cv_split <- vfold_cv(training_data, v = 5)

cv_data <- cv_split %>% 
  mutate(
    # Extract the train dataframe for each split
    train = map(splits, ~training(.x)), 
    # Extract the validate dataframe for each split
    validate = map(splits, ~testing(.x))
  )

# Use head() to preview cv_data
head(cv_data)

# Build a model using the train data for each fold of the cross validation
cv_models_lm <- cv_data %>% 
  mutate(model = map(train, ~lm(formula = saleprice_asmt_log10 ~ ., data = .x)))

#problem with factors split across training/validation
#https://stats.stackexchange.com/questions/235764/new-factors-levels-not-present-in-training-data

cv_prep_lm <- cv_models_lm %>% 
  mutate(
    # Extract the recorded life expectancy for the records in the validate dataframes
    validate_actual = map(validate, ~.x$saleprice_asmt_log10),
    # Predict life expectancy for each validate set using its corresponding model
    validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y))
  )

library(Metrics)
# Calculate the mean absolute error for each validate fold       
cv_eval_lm <- cv_prep_lm %>% 
  mutate(validate_mae = map2_dbl(___, ___, ~mae(actual = .x, predicted = .y)))

# Print the validate_mae column
cv_eval_lm$___

# Calculate the mean of validate_mae column
___






# Build a random forest model for each fold
cv_models_rf <- cv_data %>% 
  mutate(model = map(___, ~ranger(formula = ___, data = ___,
                                  num.trees = 100, seed = 42)))

# Generate predictions using the random forest model
cv_prep_rf <- cv_models_rf %>% 
  mutate(validate_predicted = map2(.x = ___, .y = ___, ~predict(.x, .y)$predictions))

library(ranger)

# Calculate validate MAE for each fold
cv_eval_rf <- cv_prep_rf %>% 
  mutate(validate_mae = map2_dbl(___, ___, ~mae(actual = .x, predicted = .y)))

# Print the validate_mae column
___

# Calculate the mean of validate_mae column
___

# Prepare for tuning your cross validation folds by varying mtry
cv_tune <- cv_data %>% 
  crossing(mtry = ___:___) 

# Build a model for each fold & mtry combination
cv_model_tunerf <- cv_tune %>% 
  mutate(model = map2(.x = ___, .y = ___, ~ranger(formula = life_expectancy~., 
                                                  data = .x, mtry = .y, 
                                                  num.trees = 100, seed = 42)))

# Build the model using all training data and the best performing parameter
best_model <- ranger(formula = ___, data = ___,
                     mtry = ___, num.trees = 100, seed = 42)

# Prepare the test_actual vector
test_actual <- testing_data$___

# Predict life_expectancy for the testing_data
test_predicted <- predict(___, ___)$predictions

# Calculate the test MAE
mae(___, ___)