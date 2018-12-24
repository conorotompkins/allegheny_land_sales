library(tidyverse)
library(janitor)
library(broom)
library(sf)
library(scales)

options(scipen = 999, digits = 4)

theme_set(theme_bw())

df <- read_csv("data/parcel_exploratory.csv", progress = FALSE) %>% 
  select(calcacreag, shape_leng, shape_area, contains("property"), munidesc, schooldesc, saledesc, instrtypdesc, x, y, price) %>% 
  filter(instrtypdesc == "DEED")

instrument <- df %>% 
  group_by(instrtypdesc) %>% 
  summarize(price_mean = mean(price, na.rm = TRUE)) %>% 
  arrange(desc(price_mean)) %>% 
  pull(instrtypdesc)

df %>% 
  select(instrtypdesc, price) %>% 
  na.omit() %>% 
  group_by(instrtypdesc) %>% 
  mutate(price_mean = mean(price, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(instrtypdesc = fct_reorder(instrtypdesc, price_mean)) %>% 
  ggplot(aes(instrtypdesc, price)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(labels = dollar)

df %>% 
  select(instrtypdesc, price) %>% 
  na.omit() %>% 
  group_by(instrtypdesc) %>% 
  mutate(price_mean = mean(price, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(instrtypdesc = fct_reorder(instrtypdesc, price_mean)) %>% 
  ggplot(aes(instrtypdesc, price)) +
  #geom_jitter(alpha = .1) +
  geom_violin() +
  #geom_point(aes(y = price_mean), size = 3, color = "red") +
  coord_flip() +
  scale_y_continuous(labels = dollar)

df %>% 
  select(propertycity, price) %>% 
  na.omit() %>% 
  group_by(propertycity) %>% 
  mutate(price_mean = mean(price, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(propertycity = fct_reorder(propertycity, price_mean)) %>% 
  ggplot(aes(propertycity, price)) +
  geom_jitter(alpha = .1) +
  #geom_violin() +
  geom_point(aes(y = price_mean), size = 3, color = "red") +
  coord_flip() +
  scale_y_continuous(labels = dollar)

df %>% 
  select(propertyzip, price) %>% 
  mutate(propertyzip = as.factor(propertyzip)) %>% 
  na.omit() %>% 
  group_by(propertyzip) %>% 
  mutate(price_mean = mean(price, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(propertyzip = fct_reorder(propertyzip, price_mean)) %>% 
  ggplot(aes(propertyzip, price)) +
  geom_jitter(alpha = .1) +
  #geom_violin() +
  geom_point(aes(y = price_mean), size = 3, color = "red") +
  coord_flip() +
  scale_y_continuous(labels = dollar)

df %>% 
  select(munidesc, price) %>% 
  na.omit() %>% 
  group_by(munidesc) %>% 
  mutate(price_mean = mean(price, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(munidesc = fct_reorder(munidesc, price_mean)) %>% 
  ggplot(aes(munidesc, price)) +
  geom_jitter(alpha = .1) +
  #geom_violin() +
  geom_point(aes(y = price_mean), size = 3, color = "red") +
  coord_flip() +
  scale_y_continuous(labels = dollar)

df %>% 
  select(schooldesc, price) %>% 
  na.omit() %>% 
  group_by(schooldesc) %>% 
  mutate(price_mean = mean(price, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(schooldesc = fct_reorder(schooldesc, price_mean)) %>% 
  ggplot(aes(schooldesc, price)) +
  geom_jitter(alpha = .1) +
  #geom_violin() +
  geom_point(aes(y = price_mean), size = 3, color = "red") +
  coord_flip() +
  scale_y_continuous(labels = dollar)



#df %>% 
#  as_tibble() %>% 
#  select(calcacreag, shape_leng, shape_area, price) %>% 
#  gather(metric, measure, -price) %>% 
#  ggplot(aes(measure, price)) +
#  geom_point() +
#  facet_wrap(~measure)

df %>% 
  ggplot(aes(calcacreag, shape_area)) +
  geom_point()

df %>% 
  ggplot(aes(shape_leng, shape_area)) +
  geom_point()

df %>% 
  ggplot(aes(log(calcacreag, 10), log(price, 10))) +
  geom_point() +
  geom_smooth()

df %>% 
  ggplot(aes(shape_area, price)) +
  geom_point()

df %>% 
  ggplot(aes(shape_leng, price)) +
  geom_point()

df %>% 
  nrow()

df %>% 
  count(propertyzip, sort = TRUE)

df %>% 
  count(propertycity, sort = TRUE)

df %>% 
  count(munidesc, sort = TRUE)

df %>% 
  count(saledesc, sort = TRUE)

df %>% 
  count(instrtypdesc, sort = TRUE)

