guild_to_parameter <- function(type, 
                               range = NULL,
                               inclusive = c(TRUE, TRUE), 
                               values = NULL) {
  type <- rlang::arg_match(type, c("double", "integer", "character", "logical"))
  is_quant <- type %in% c("double", "integer")
  
  if (is_quant) {
    if (rlang::is_missing(range)) {
      rlang::abort("Please provide a `range`.")
    }
    if (rlang::is_missing(range)) {
      rlang::abort("Please provide if the ends of the range are `inclusive`.")
    }
  } else {
    if (rlang::is_missing(values)) {
      rlang::abort("Please provide a set of `values`.")
    }
  }
  
  if (is_quant) {
    dials_param <- dials::new_quant_param(
      type = type,
      range = range,
      inclusive = inclusive,
      label = c(guild_parameter = "Parameter for Guild AI")
    )
  } else {
    dials_param <- dials::new_qual_param(
      type = type,
      values = values,
      label = c(guild_parameter = "Parameter for Guild AI")
    )
  }  
  
  dials_param
}

guild_to_parameter_set <- function(parameter_info) {
  
  dials_param_list <- purrr::map(
    parameter_info,
    function(x) {
      if (is.null(x$inclusive)) {
        guild_to_parameter(type = x$type, range = x$range, values = x$values)
      } else {
        guild_to_parameter(type = x$type, range = x$range, 
                           inclusive = x$inclusive, values = x$values)
      }
    }
  )
  
  dials::parameters(dials_param_list)
}

prep_inital <- function(past_results, parameter_set) {
  parameter_names <- parameter_set$id
  metric_name <- setdiff(names(past_results), parameter_names)
  if (length(metric_name) > 1) {
    rlang::abort("Please only include one metric column.")
  }
  
  gp_fit_input <- past_results %>% 
    mutate(.metric = metric_name,
           .iter = 0L) %>% 
    rename(mean = all_of(metric_name))
  gp_fit_input
}

guild_gp <- function(past_results,
                     parameter_set,
                     target = "minimize",
                     objective = tune::exp_improve(),
                     control = tune::control_bayes(),
                     ...) {
  
  # prep guild inputs to mimic tune inputs as closely as necessary
  mean_stats_almost <- prep_inital(past_results, parameter_set)
  metrics_name <- unique(mean_stats_almost$.metric)
  maximize <- target == "maximize"
  
  score_card <- tune:::initial_info(mean_stats_almost, metrics_name, maximize)
  
  set.seed(control$seed[1])
  gp_mod <- tune:::fit_gp(
    mean_stats_almost,
    pset = parameter_set,
    metric = metrics_name,
    control = control,
    ...
  )
  gp_mod <- tune:::check_gp_failure(gp_mod, NULL)
  
  set.seed(control$seed[1] + 1)
  candidates <- tune:::pred_gp(
    gp_mod, parameter_set,
    control = control,
    current = mean_stats_almost
  )
  
  candidates <- dplyr::bind_cols(
    candidates,
    stats::predict(
      objective, 
      candidates,
      iter = i,
      maximize = maximize,
      score_card$best_val
    )
  )
  
  candidates <- tune:::pick_candidate(candidates, score_card, control)
  
  candidates
}