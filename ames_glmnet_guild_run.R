# remotes::install_github("t-kalinowski/guildai-r")
# guildai::install_guild()

library(guildai)
library(tidymodels)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------

guild_run("ames_glmnet_guild_script.R")
guild_view()

# ------------------------------------------------------------------------------

# Use existing tidymodels code to generate a space-filling design for new
# hyperparameter combinations
params <- 
  parameters(
    list(
      pen_val = penalty(),
      mix_val = mixture(),
      latitude_df = spline_degree(c(2L, 50L)),
      longitude_df = spline_degree(c(2L, 50L)) 
    )
  )

set.seed(1)
grid <- grid_latin_hypercube(params, size = 19)

# Run the new configurations by splicing their values into the seed script. 
guild_run(
  "ames_glmnet_guild_script.R",
  flags = grid,
  echo = FALSE,
  wait = TRUE
)

