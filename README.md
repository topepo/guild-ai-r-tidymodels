# guild-ai-r-tidymodels

This is a demo project to see how the [R interface](https://github.com/t-kalinowski/guildai-r) to [guild.ai](https://guild.ai) works. 

It uses a small data set ([Ames housing](https://www.tmwr.org/ames.html)) to optimize a regularized regression model (via the glmnet package).

The data analysis code uses the [tidymodels](https://www.tidymodels.org) framework.

Files: 

 - `ames_glmnet_guild_script.R` the "seed script" that is used as the prototype for the analysis. It has some initial conditions that are formatted in the script in a way that guild will find them.
 - `ames_glmnet_guild_run.R` a sort of meta-script that processes the seed script, created new hyperparameter combinations (a.k.a. guild flags), and runs them.
 - `tidymodels_prototype.R` the original analysis as it would be done via tidymodels. There are some differences, mostly because I don't know how to do it with guild:
   - 10-fold CV is used instead of a validation set. 
   - The grid is made automatically by `tune_grid()`. 
   - The results are collected so that all 20 hyperparameter combinations are investigated simultaneously in real-time and their relationship with RMSE is shown via `autoplot()`. 
   
There are some notes in the seed script that are mostly about the different philosophies of interactive data analysis and a more batch driven processes. There are also notes about how this R script had to be configured so that guild can process it. 


