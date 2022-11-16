# remotes::install_github("t-kalinowski/guildai-r")
# guildai::install_guild()

library(guildai)
library(tidymodels)
source("glue.R")

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------

guild_run("ames_glmnet_guild_script.R", echo = FALSE)
guild_view()

# ------------------------------------------------------------------------------

# Use tidymodels to generate a space-filling design for new
# hyperparameter combinations

parameter_info <- list(
  penalty = list(type = "double", range = c(0, 1)),
  mixture = list(type = "double", range = c(0, 1), inclusive = c(FALSE, FALSE)),
  latitude_df = list(type = "integer", range = c(2L, 50L)),
  longitude_df = list(type = "integer", range = c(2L, 50L))
)

params <- guild_to_parameter_set(parameter_info)

set.seed(1)
grid <- grid_latin_hypercube(params, size = 19)

# Run the new configurations by splicing their values into the seed script. 
guild_run(
  "ames_glmnet_guild_script.R",
  flags = grid,
  echo = FALSE,
  wait = TRUE
)


# -------------------------------------------------------------------------

# Use tidymodels and a Gaussian process model to create the next set of 
# candidate values based on past runs

# collect these from where Guild AI stores results of past runs
past_results <- data.frame(
  penalty = c(0.000512266023524435, 0.00261082769969077, 0.000224087518958133,
              0.00940059720190888, 3.97878817735671e-06),
  mixture = c(0.569303362320061, 0.428850342386868, 0.824966110821115, 
              0.22881132167764, 0.373991017234512),
  latitude_df = c(23L, 21L, 25L, 38L, 11L), 
  longitude_df = c(44L, 7L, 26L, 42L, 50L),
  rmse = c(0.0633960168515522, 0.0634472884678756, 0.0635053509670169, 
           0.0643535245578563, 0.064734873635114)
)

guild_gp(past_results, params, target = "minimize")
