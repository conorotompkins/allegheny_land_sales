#load libraries
library(tidyverse)
library(scales)
library(caret)
library(broom)
library(modelr)
library(rsample)

#set up environment
options(scipen = 999, digits = 5)

theme_set(theme_bw())

#read in data
source("scripts/load_parcel_sales_combined.R")

#create grade vectors
grades_standard <- c("average -", "average", "average +",
                     "good -", "good", "good +",
                     "very good -", "very good", "very good +")

grades_below_average_or_worse <- c("poor -", "poor", "poor +",
                                   "below average -", "below average", "below average +")

grades_excellent_or_better <- c("excellent -", "excellent", "excellent +",
                                "highest cost -", "highest cost", "highest cost +")

#subset and engineer features
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
         saleprice_asmt_log10, saledate_sales) %>% 
  mutate(usedesc_asmt = fct_lump(usedesc_asmt, n = 5),
         styledesc_asmt = fct_lump(styledesc_asmt, n = 10),
         #clean up and condense gradedesc_asmt
         gradedesc_asmt = str_to_lower(gradedesc_asmt),
         gradedesc_asmt = case_when(gradedesc_asmt %in% grades_below_average_or_worse ~ "below average + or worse",
                                    gradedesc_asmt %in% grades_excellent_or_better ~ "excellent - or better",
                                    gradedesc_asmt %in% grades_standard ~ gradedesc_asmt),
         gradedesc_asmt = fct_relevel(gradedesc_asmt, c("below average + or worse", "average -", "average", "average +",
                                                        "good -", "good", "good +",
                                                        "very good -", "very good", "very good +", "excellent - or better")))

#replace missing character rows with "missing", change character columns to factor
df <- df %>% 
  mutate_if(is.character, replace_na, "missing") %>% 
  mutate_if(is.character, as.factor)

#select response and features
df <- df %>% 
  select(munidesc_asmt, usedesc_asmt, styledesc_asmt, conditiondesc_asmt, gradedesc_asmt,
         finishedlivingarea_asmt_log10, lotarea_asmt_log10, yearblt_asmt, bedrooms_asmt, 
         fullbaths_asmt, halfbaths_asmt, saleprice_asmt_log10) %>% 
  na.omit()

#view data
glimpse(df)

#create initial split object
df_split <- initial_split(df, prop = .75)
df_split

#extract training dataframe
training_data_full <- training(df_split)
training_data_full

#extract testing dataframe
testing_data <- testing(df_split)
testing_data

#find dimensions of training_data_full and testing_data
dim(training_data_full)
dim(testing_data)

set.seed(42)

#prep the df with the cross validation partitions
cv_split <- vfold_cv(training_data_full, v = 5)

cv_data <- cv_split %>% 
  mutate(
    #extract train dataframe for each split
    train = map(splits, ~training(.x)), 
    #extract validate dataframe for each split
    validate = map(splits, ~testing(.x))
  )

#view df
cv_data

#build model using the train data for each fold of the cross validation
cv_models_lm <- cv_data %>% 
  mutate(model = map(train, ~lm(formula = saleprice_asmt_log10 ~ ., data = .x)))
cv_models_lm
#problem with factors split across training/validation
#https://stats.stackexchange.com/questions/235764/new-factors-levels-not-present-in-training-data

cv_prep_lm <- cv_models_lm %>% 
  mutate(
    #extract recorded life expectancy for the records in the validate dataframes
    validate_actual = map(validate, ~.x$saleprice_asmt_log10),
    #predict response variable for each validate set using its corresponding model
    validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y))
  )

#View data
cv_prep_lm

#calculate fit metrics for each validate fold       
cv_eval_lm <- cv_prep_lm %>% 
  mutate(validate_rmse = map2_dbl(model, validate, modelr::rmse),
         validate_mae = map2_dbl(model, validate, modelr::mae))

cv_eval_lm <- cv_eval_lm %>% 
  mutate(fit = map(model, ~glance(.x))) %>% 
  unnest(fit)

#view data
cv_eval_lm

#summarize fit metrics
cv_eval_lm %>% 
  select(validate_mae, validate_rmse, adj.r.squared) %>% 
  summarize_all(mean)

#glance at statistics for validate data
cv_prep_lm %>% 
  mutate(glance_validate = map(model, ~glance(.x))) %>% 
  unnest(glance_validate)

#create dfs for train_data_small and validate_data
train_data_small <- cv_prep_lm %>% 
  unnest(train) %>% 
  select(-id)

validate_data <- cv_prep_lm %>% 
  unnest(validate)

#create model object
model <- lm(saleprice_asmt_log10 ~ ., data = train_data_small)

#visualize model on small training data
tidy_validate <- tidy(model) %>% 
  arrange(-estimate)

#10th ward is the base factor for muni_desc term
tidy_validate %>% 
  filter(str_detect(term, "10th"))

#visualize estimates for terms
tidy_validate %>% 
  filter(term != "(Intercept)") %>% 
  mutate(term = fct_reorder(term, estimate)) %>% 
  ggplot(aes(term, estimate)) +
  geom_point() +
  coord_flip()

#visualize model on validate data
augment_validate <- augment(model, newdata = validate_data) %>% 
  mutate(.resid = saleprice_asmt_log10 - .fitted)

#actual vs. fitted
cv_prep_lm %>% 
  unnest(validate_actual, validate_predicted) %>% 
  ggplot(aes(validate_actual, validate_predicted)) +
  geom_abline() +
  #geom_jitter(alpha = .1) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits = c(2, 7)) +
  scale_y_continuous(limits = c(2, 7)) +
  coord_equal() +
  scale_fill_viridis_c()

#distribution of residuals
augment_validate %>% 
  ggplot(aes(.resid)) +
  geom_density() +
  geom_vline(xintercept = 0, color = "red", linetype = 2)

#sale price vs. residuals
augment_validate %>% 
  ggplot(aes(.resid, saleprice_asmt_log10)) +
  #geom_jitter(alpha = .1) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  geom_vline(xintercept = 0, color = "red", linetype = 2) +
  scale_fill_viridis_c()

#calculate fit of model on test data
modelr::rmse(model, testing_data)
modelr::mae(model, testing_data)
modelr::rsquare(model, testing_data)

#visualize model on test data
test_augment <- augment(model, newdata = testing_data)

#actual vs. fitted
test_augment %>% 
  ggplot(aes(saleprice_asmt_log10, .fitted)) +
  geom_abline() +
  #geom_jitter(alpha = .1) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits = c(2, 7)) +
  scale_y_continuous(limits = c(2, 7)) +
  coord_equal() +
  scale_fill_viridis_c()

#distribution of residuals
test_augment %>% 
  mutate(.resid = saleprice_asmt_log10 - .fitted) %>% 
  ggplot(aes(.resid)) +
  geom_density() +
  geom_vline(xintercept = 0, color = "red", linetype = 2)

#sale price vs. residuals
test_augment %>% 
  mutate(.resid = saleprice_asmt_log10 - .fitted) %>% 
  ggplot(aes(.resid, saleprice_asmt_log10)) +
  #geom_jitter(alpha = .1) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  geom_vline(xintercept = 0, color = "red", linetype = 2) +
  scale_fill_viridis_c()
