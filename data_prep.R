# Data Prep

library(tidyverse)
library(broom)
library(moments)
library(bestNormalize)

ames <- read_csv("ames_cleaned.csv") %>% 
   drop_na() %>% 
   select(sale_price, lot_frontage, lot_area, 
          total_bsmt_sf, garage_area, house_age, porch_x_deck, sqft_above_ground)


ames_plots <- ames %>% 
   mutate(across(
      everything(),
      .fns = list(
         log = ~log(.x + 1),
         sqrt = sqrt,
         yeo = ~predict(yeojohnson(.x)),
         best = ~predict(bestNormalize(.x))
      ),
      .names = "{.col}.{fn}"
   ))

ames_plots <- ames_plots %>% 
   pivot_longer(
      cols = everything(), 
      names_to = "variables", 
      values_to = "values"
   ) %>%
   separate(variables, into = c("variables", "metrics"), sep = "\\.", remove = TRUE) %>% 
   mutate(metrics = replace_na(metrics, "none")) 


ames_skew <- ames %>% 
   map_dfr(~tibble(
      "Raw Data" = round(skewness(.x, na.rm = TRUE), 3),
      "Natural Logarithm" = round(skewness(log(.x + 1), na.rm = TRUE), 3),
      "Square Root" = round(skewness(sqrt(.x), na.rm = TRUE), 3),
      "Yeo-Johnson" = round(skewness(predict(yeojohnson(.x)), na.rm = TRUE), 3), 
      "Best Normalize" = round(skewness(predict(bestNormalize(.x)), na.rm = TRUE), 3)
   ), .id = "Variable") 

ames_skew <- ames_skew %>% 
   mutate(Variable = case_when(
      Variable == "sale_price" ~ "Sale Price",
      Variable == "lot_frontage" ~ "Lot Frontage",
      Variable == "lot_area" ~ "Lot Area",
      Variable == "total_bsmt_sf" ~ "Basement Area",
      Variable == "garage_area" ~ "Garage Area",
      Variable == "house_age" ~ "Age of House",
      Variable == "porch_x_deck" ~ "Porch and Decking Area",
      Variable == "sqft_above_ground" ~ "Above Ground Area"
   ))

