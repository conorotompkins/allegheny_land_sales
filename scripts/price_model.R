library(tidyverse)
library(janitor)
library(broom)
library(modelr)
library(scales)
library(caret)

options(scipen = 999, digits = 5)

theme_set(theme_bw())

df <- read_csv("data/parcel_exploratory.csv", progress = FALSE) %>%
  select(calcacreag, schooldesc, munidesc, instrtypdesc, price) %>% 
  filter(instrtypdesc == "DEED") %>% 
  na.omit()

summary(df)

fit <- lm(price ~ calcacreag + schooldesc, data = df)

model <- fit %>%
  tidy() %>% 
  arrange(desc(estimate))

model

model %>% 
  mutate(term = fct_reorder(term, estimate)) %>% 
  ggplot(aes(term, estimate)) +
  geom_point() +
  coord_flip() +
  scale_y_continuous(labels = dollar)

fit %>% 
  glance()

df_pred <- fit %>% 
  augment(df)

df_pred %>% 
  ggplot(aes(price, .fitted)) +
  geom_point()

rmse(fit, df)

sqrt(mean(error^2))
?rmse


