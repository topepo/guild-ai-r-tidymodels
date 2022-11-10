# NOTE: notes are for Tomasz

library(tidymodels)
library(butcher)
library(bestNormalize)
library(sessioninfo)

# NOTE: Also needs glmnet installed but don't need to attach; not sure how to state that.

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

# NOTE: Not how I would normally split the data for validation. For many cases
# we have multiple holdout data sets (e.g. 10-fold CV or bootstrap). I'll use a
# validation set here for simplicity but don't know how I would do any other
# data splitting strategy with guild and how the results would be aggregated.

set.seed(12)
ames_split <- initial_split(ames, strata = Sale_Price)
ames_not_test <- training(ames_split)
ames_test <- testing(ames_split)

set.seed(382)
ames_rs <- validation_split(ames_not_test, strata = Sale_Price)
ames_train <- analysis(ames_rs$splits[[1]])
ames_val <- assessment(ames_rs$splits[[1]])

# ------------------------------------------------------------------------------
# Parameters

# NOTE :-O this is sooo unnatural. We would normally use values of tune() for
# them and _not_ include them as global variables.

#| description: spline degrees of freedom for longitude
#| min: 2L
#| max: 50L
longitude_df <- 40L
#| description: spline degrees of freedom for latitude
#| min: 2L
#| max: 50L
latitude_df <- 40L
#| description: L2 penalty
#| min: 0.0
#| max: 1.0
pen_val <- 0.001
#| description: Mixture of L1 and L2 penalty
#| min: 0.0
#| max: 1.0
mix_val <- 1.0

# ------------------------------------------------------------------------------

ames_rec <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
  step_other(MS_SubClass, MS_Zoning, Neighborhood, threshold = 0.05) %>%
  step_orderNorm(Lot_Area, ends_with("_SF"), Gr_Liv_Area) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_interact(~ starts_with("Central"):Year_Built) %>%
  # NOTE We splice the values in via !! so that the step uses their values
  # and not a reference to global variables (a very bad idea)
  step_spline_natural(Longitude, deg_free = !!longitude_df) %>%
  step_spline_natural(Latitude, deg_free = !!latitude_df)

# Note: parsnip models capture the environment and reference and will do the
# splicing themselves.
glmn_spec <-
  linear_reg(penalty = pen_val, mixture = mix_val) %>%
  set_engine("glmnet")

ames_wflow <-
  workflow() %>%
  add_model(glmn_spec) %>%
  add_recipe(ames_rec)

ames_fit <- fit(ames_wflow, data = ames_train)
ames_fit

# ------------------------------------------------------------------------------

glmnet_fit <-
  ames_fit %>%
  extract_fit_engine()

glmnet_pred <-
  glmnet_fit %>%
  coef(s = pen_val) %>%
  apply(2, function(x) sum(x != 0))

glmnet_fit %>% autoplot(best_penalty = pen_val)



# ------------------------------------------------------------------------------
# Validation set results

ames_pred <- augment(ames_fit, ames_val)

ames_pred %>%
  ggplot(aes(Sale_Price, .pred)) +
  geom_abline(col = "green", lty = 2) +
  geom_point(alpha = 1 / 3) +
  coord_obs_pred() +
  labs(x = "Observed (log-10)", y = "Predicted (log-10)")

val_results <- ames_pred %>% metrics(Sale_Price, .pred)

cat('validation_rmse:', val_results$.estimate[1], "\n")
cat('validation_R2:', val_results$.estimate[2], "\n")
cat('num_predictors:', glmnet_pred, "\n")

# ------------------------------------------------------------------------------
# Save object

# NOTE Normally we would save the result objects too (and not print them out)

# butcher the objects to make their install sizes smaller
ames_fit <- butcher(ames_fit)

# NOTE put it here just as an example
target_dir <- tempfile()

save(ames_fit, file = target_dir, compress = TRUE)

# ------------------------------------------------------------------------------

session_info()

if (!interactive()) {
  # q("no")
}

