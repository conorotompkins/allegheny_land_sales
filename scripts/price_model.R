library(tidyverse)
library(janitor)
library(broom)
library(modelr)

options(scipen = 999, digits = 4)

theme_set(theme_bw())

df <- read_csv("data/parcel_exploratory.csv", progress = FALSE)

fit <- lm(price ~ calcacreag + instrtypdesc + schooldesc, data = df)

model <- fit %>%
  tidy() %>% 
  arrange(desc(estimate))
model

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

