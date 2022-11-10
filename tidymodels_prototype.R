library(tidymodels)
library(bestNormalize)

# ------------------------------------------------------------------------------
# Boilerplate options

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------
# Setup and partition data

data(ames)

ames <-
  ames %>%
  mutate(Sale_Price = log10(Sale_Price)) %>%
  select(-contains("_Cond"))

set.seed(12)
ames_split <- initial_split(ames, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

set.seed(382)
ames_rs <- vfold_cv(ames_train, strata = Sale_Price)

# ------------------------------------------------------------------------------
# pure tidymodels with interactive data analysis

ames_rec <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
  step_other(MS_SubClass, MS_Zoning, Neighborhood, threshold = 0.05) %>%
  step_orderNorm(Lot_Area, ends_with("_SF"), Gr_Liv_Area) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_interact(~ starts_with("Central"):Year_Built) %>%
  step_spline_natural(Longitude, deg_free = tune("latitude_df")) %>%
  step_spline_natural(Latitude, deg_free = tune("longitude_df"))

glmn_spec <-
  linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

ames_wflow <-
  workflow() %>%
  add_model(glmn_spec) %>%
  add_recipe(ames_rec)

ames_param <- 
  ames_wflow %>% 
  extract_parameter_set_dials() %>% 
  update(
    latitude_df = spline_degree(c(2L, 50L)),
    longitude_df = spline_degree(c(2L, 50L)) 
  )

set.seed(1)
res <- 
  ames_wflow %>% 
  tune_grid(resamples = ames_rs, grid = 20, param_info = ames_param)

autoplot(res, metric = "rmse")

show_best(res)
