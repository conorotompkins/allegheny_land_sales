library(tidyverse)
library(scales)
library(caret)
library(broom)
library(modelr)
library(rsample)
library(Metrics)

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
  select_if(is.numeric) %>% 
  na.omit()


glimpse(df)


# Prepare the initial split object
df_split <- initial_split(df, prop = .75)
df_split
# Extract the training dataframe
training_data <- training(df_split)
training_data

# Extract the testing dataframe
testing_data <- testing(df_split)
testing_data
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
cv_models_lm
#problem with factors split across training/validation
#https://stats.stackexchange.com/questions/235764/new-factors-levels-not-present-in-training-data

cv_prep_lm <- cv_models_lm %>% 
  mutate(
    # Extract the recorded life expectancy for the records in the validate dataframes
    validate_actual = map(validate, ~.x$saleprice_asmt_log10),
    # Predict life expectancy for each validate set using its corresponding model
    validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y))
  )

head(cv_prep_lm)


# Calculate the mean absolute error for each validate fold       
cv_eval_lm <- cv_prep_lm %>% 
  mutate(validate_mae = map2_dbl(validate_actual, validate_predicted, ~mae(actual = .x, predicted = .y)),
         validate_rmse = map2_dbl(model, validate, modelr::rmse))

cv_eval_lm %>% 
  mutate(fit = map(model, ~glance(.x))) %>% 
  unnest(fit)

head(cv_eval_lm)

# Print the validate_mae column
cv_eval_lm$validate_mae

# Calculate the mean of validate_mae column
mean(cv_eval_lm$validate_mae)

#visualize model on training data
cv_prep_lm %>% 
  unnest(validate_actual, validate_predicted) %>% 
  ggplot(aes(validate_actual, validate_predicted)) +
  geom_jitter(alpha = .1) +
  geom_smooth(method = "lm") +
  geom_abline() +
  scale_x_continuous(limits = c(2, 7)) +
  scale_y_continuous(limits = c(2, 7)) +
  coord_equal()

model <- lm(saleprice_asmt_log10 ~ ., data = training_data)

augment_train <- augment(model)

augment_train %>% 
  ggplot(aes(.resid)) +
  geom_density() +
  geom_vline(xintercept = 0, color = "red", linetype = 2)

augment_train %>% 
  ggplot(aes(.resid, saleprice_asmt_log10)) +
  geom_jitter(alpha = .1) +
  geom_vline(xintercept = 0, color = "red", linetype = 2)

#calculate fit of test data
head(testing_data)

modelr::rmse(model, testing_data)
modelr::mae(model, testing_data)
modelr::rsquare(model, testing_data)

#visualize model on test data
test_augment <- augment(model, newdata = testing_data)

test_augment %>% 
  ggplot(aes(saleprice_asmt_log10, .fitted)) +
  geom_jitter(alpha = .1) +
  geom_smooth(method = "lm") +
  geom_abline() +
  scale_x_continuous(limits = c(2, 7)) +
  scale_y_continuous(limits = c(2, 7)) +
  coord_equal()

test_augment %>% 
  mutate(.resid = saleprice_asmt_log10 - .fitted) %>% 
  ggplot(aes(.resid)) +
  geom_density() +
  geom_vline(xintercept = 0, color = "red", linetype = 2)

test_augment %>% 
  mutate(.resid = saleprice_asmt_log10 - .fitted) %>% 
  ggplot(aes(.resid, saleprice_asmt_log10)) +
  geom_jitter(alpha = .1) +
  geom_vline(xintercept = 0, color = "red", linetype = 2)