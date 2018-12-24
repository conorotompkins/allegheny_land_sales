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

# Shuffle row indices: rows
rows <- sample(nrow(df))

# Randomly order data: Sonar
df <- df[rows, ]

# Identify row to split on: split
split <- round(nrow(df) * .80)

# Create train
train <- df[1:split, ]

# Create test
test <- df[(split + 1):nrow(df), ]

model <- train(
  price ~ calcacreag + schooldesc, df,
  method = "rf",
  trControl = trainControl(
    method = "repeatedcv", number = 5,
    repeats = 5, verboseIter = TRUE
  )
)
model