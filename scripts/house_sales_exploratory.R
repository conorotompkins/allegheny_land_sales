library(tidyverse)
library(scales)

options(scipen = 999, digits = 5)

theme_set(theme_bw())

source("scripts/load_parcel_sales_combined.R")

df <- df %>% 
  filter(classdesc_asmt == "RESIDENTIAL",
         str_detect(munidesc_asmt, "Ward"),
         saleprice_asmt > 1) %>% 
  select(pin, munidesc_asmt, schooldesc_asmt, neighdesc_asmt, taxdesc_asmt,
         usedesc_asmt, homesteadflag_asmt, farmsteadflag_asmt, styledesc_asmt,
         yearblt_asmt, extfinish_desc_asmt, roofdesc_asmt,  basementdesc_asmt,
         gradedesc_asmt, conditiondesc_asmt, stories_asmt, totalrooms_asmt, bedrooms_asmt,
         fullbaths_asmt, halfbaths_asmt, heatingcoolingdesc_asmt, fireplaces_asmt, 
         bsmtgarage_asmt, finishedlivingarea_asmt, lotarea_asmt, price_sales, saleprice_asmt,
         saledate_sales)

n_ntile <- 40

df %>% 
  count(munidesc_asmt, sort = TRUE) %>% 
  View()

df %>%
  count(schooldesc_asmt, sort = TRUE)

df %>%
  count(neighdesc_asmt, sort = TRUE)

df %>% 
  count(usedesc_asmt, sort = TRUE) %>% 
  View()

df %>% 
  count(homesteadflag_asmt, sort = TRUE)

df %>% 
  count(farmsteadflag_asmt, sort = TRUE)

df %>% 
  count(styledesc_asmt, sort = TRUE) %>% 
  View()


df %>%
  count(yearblt_asmt, sort = TRUE)

df %>% 
  ggplot(aes(yearblt_asmt)) +
  geom_density()

df %>% 
  count(extfinish_desc_asmt, sort = TRUE) %>% 
  View()

df %>% 
  count(roofdesc_asmt, sort = TRUE)

df %>% 
  count(basementdesc_asmt, sort = TRUE)

df %>% 
  count(gradedesc_asmt, sort = TRUE)

df %>% 
  count(conditiondesc_asmt, sort = TRUE)

df %>% 
  count(stories_asmt, sort = TRUE)

df %>% 
  count(totalrooms_asmt, sort = TRUE)

df %>% 
  count(bedrooms_asmt, sort = TRUE)

df %>% 
  count(fullbaths_asmt, sort = TRUE)

df %>% 
  count(halfbaths_asmt, sort = TRUE)

df %>% 
  count(heatingcoolingdesc_asmt, sort = TRUE)

df %>% 
  count(fireplaces_asmt, sort = TRUE)

df %>% 
  count(bsmtgarage_asmt, sort = TRUE)

df %>% 
  select(finishedlivingarea_asmt) %>% 
  mutate(q = ntile(finishedlivingarea_asmt, n_ntile)) %>% 
  filter(q < n_ntile) %>% 
  ggplot(aes(finishedlivingarea_asmt)) +
  geom_density()

df %>% 
  select(lotarea_asmt) %>% 
  mutate(q = ntile(lotarea_asmt, n_ntile)) %>% 
  filter(q < n_ntile) %>% 
  ggplot(aes(lotarea_asmt)) +
  geom_density()

df %>% 
  select(lotarea_asmt, finishedlivingarea_asmt) %>% 
  na.omit() %>% 
  mutate(lot_ntile = ntile(lotarea_asmt, n_ntile),
         finished_ntile = ntile(finishedlivingarea_asmt, n_ntile)) %>% 
  filter(lotarea_asmt < 1000000) %>% 
  #filter(lot_ntile < n_ntile) %>% 
  #count(lotarea_asmt, finishedlivingarea_asmt) %>% 
  ggplot(aes(lotarea_asmt, finishedlivingarea_asmt)) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  scale_fill_viridis_c()

df %>% 
  select(price_sales) %>% 
  #filter(price_sales > 1000) %>% 
  mutate(price_ntile = ntile(price_sales, n_ntile)) %>% 
  filter(price_ntile < n_ntile) %>% 
  ggplot(aes(price_sales)) +
  geom_density() +
  scale_x_continuous(labels = dollar)

df %>% 
  select(saleprice_asmt) %>% 
  mutate(price_ntile = ntile(saleprice_asmt, n_ntile)) %>% 
  filter(price_ntile < n_ntile) %>% 
  ggplot(aes(saleprice_asmt)) +
  geom_density() +
  scale_x_continuous(labels = dollar)

df %>% 
  select(saleprice_asmt, price_sales) %>% 
  ggplot(aes(saleprice_asmt, price_sales)) +
  geom_point()

df %>% 
  count(saledate_sales, sort = TRUE) %>% 
  na.omit() %>% 
  ggplot(aes(saledate_sales, n)) +
  geom_point() +
  geom_smooth()

df %>% 
  count(saledate_sales, taxdesc_asmt, sort = TRUE) %>% 
  na.omit() %>% 
  ggplot(aes(saledate_sales, n, color = taxdesc_asmt)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  facet_wrap(~taxdesc_asmt, ncol = 1, scales = "free_y")

df %>% 
  filter(str_detect(taxdesc_asmt, "Taxable")) %>% 
  count(saledate_sales, sort = TRUE) %>% 
  na.omit() %>% 
  ggplot(aes(saledate_sales, n)) +
  geom_point(alpha = .1) +
  geom_smooth()

df %>% 
  select(bedrooms_asmt, fullbaths_asmt) %>% 
  count(bedrooms_asmt, fullbaths_asmt, sort = TRUE) %>% 
  ggplot(aes(bedrooms_asmt, fullbaths_asmt, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_continuous(expand = c(0,0), breaks = c(0:20)) +
  scale_y_continuous(expand = c(0,0), breaks = c(0:20)) +
  theme(panel.grid = element_blank())

df %>% 
  select(bedrooms_asmt, halfbaths_asmt) %>% 
  count(bedrooms_asmt, halfbaths_asmt, sort = TRUE) %>% 
  ggplot(aes(bedrooms_asmt, halfbaths_asmt, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_continuous(expand = c(0,0), breaks = c(0:20)) +
  scale_y_continuous(expand = c(0,0), breaks = c(0:20)) +
  theme(panel.grid = element_blank())

df %>% 
  select(finishedlivingarea_asmt, totalrooms_asmt) %>% 
  ggplot(aes(finishedlivingarea_asmt, totalrooms_asmt)) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  scale_fill_viridis_c()

df %>% 
  select(finishedlivingarea_asmt, bedrooms_asmt) %>% 
  ggplot(aes(finishedlivingarea_asmt, bedrooms_asmt)) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  scale_fill_viridis_c()

df %>% 
  select(finishedlivingarea_asmt, fullbaths_asmt) %>% 
  ggplot(aes(finishedlivingarea_asmt, fullbaths_asmt)) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  scale_fill_viridis_c() +
  scale_y_continuous(breaks = c(0:3))

df %>% 
  select(finishedlivingarea_asmt, halfbaths_asmt) %>% 
  ggplot(aes(finishedlivingarea_asmt, halfbaths_asmt)) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  scale_fill_viridis_c() +
  scale_y_continuous(breaks = c(0:3))

df %>% 
  ggplot(aes(yearblt_asmt, saleprice_asmt, color = bedrooms_asmt)) +
  geom_point(aes(size = bedrooms_asmt), alpha = .1) +
  geom_smooth() +
  scale_color_viridis_c()

df %>% 
  count(yearblt_asmt, totalrooms_asmt, sort = TRUE) %>% 
  ggplot(aes(yearblt_asmt, totalrooms_asmt, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(panel.grid = element_blank())

df %>% 
  filter(saleprice_asmt < 2000000) %>% 
  ggplot(aes(finishedlivingarea_asmt, saleprice_asmt)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "lm")

df %>% 
  select(munidesc_asmt, saleprice_asmt) %>%
  filter(saleprice_asmt < 20000000) %>% 
  group_by(munidesc_asmt) %>% 
  mutate(median_price = median(saleprice_asmt, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(munidesc_asmt = fct_reorder(munidesc_asmt, median_price)) %>%
  ggplot(aes(munidesc_asmt, saleprice_asmt)) +
  geom_jitter(alpha = .1) +
  scale_y_continuous(labels = dollar) +
  coord_flip()


