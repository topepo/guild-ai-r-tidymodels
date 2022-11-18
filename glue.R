require(dials)
require(tune)

require(dplyr)
require(rlang)
require(purrr)

#' Generate a dials parameter from minimal arguments
#'
#' @param type The type of the parameter. It can be `"double"` or `"integer"` 
#' for quantitative parametes and `"character"` or `"logical"`for qualitative 
#' parameters.
#' @param range The range of the parameter as a two-element vector. Required for
#' quantitative parameters. 
#' @param inclusive A two-element vector specifying if the ranges are inclusive. 
#' @param values The set of possible values. Required for qualitative parameters,
#' ignored for quantitative parameters.
#'
#' @return A dials parameter object of class `"param"`.
#'
#' @examples
#' guild_to_parameter(type = "double", range = c(0, 1))
#' guild_to_parameter(type = "character", values = letters[1:4])
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

#' Generate a dials parameter set from a named list
#'
#' @param parameter_info A named list, see Details for structure.
#' 
#' @details Input is a named list with one list entry per parameter. That entry 
#' is a named list itself with the following entries
#' - `type`: The type of the parameter. It can be `"double"` or `"integer"` 
#' for quantitative parameters and `"character"` or `"logical"`for qualitative 
#' parameters.
#' - `range`: The range of the parameter as a two-element vector. Required for
#' quantitative parameters. 
#' - `inclusive`: A two-element vector specifying if the ranges are inclusive. 
#' - `values`: The set of possible values. Required for qualitative parameters,
#' ignored for quantitative parameters.
#'
#' @return A dials parameter set with class `"parameters"`.
#' @export
#'
#' @examples
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

#' Helper function for past tuning results
#'
#' @param past_results Tuning results in a data frame with one column for the 
#' performance measurements and one column each for the tuning parameters.
#' @param parameter_set A dials parameter set of class `"parameters"`.
#'
#' @return A data frame suitable to be used in tune's functions for Gaussian Process models.
#' @export
#' @keywords internal
#'
#' @examples
prep_inital <- function(past_results, parameter_set) {
  parameter_names <- parameter_set$id
  metric_name <- setdiff(names(past_results), parameter_names)
  if (length(metric_name) > 1) {
    rlang::abort("Please only include one metric column.")
  }
  
  gp_fit_input <- past_results %>% 
    dplyr::mutate(.metric = metric_name,
                  .iter = 0L) %>% 
    dplyr::rename(mean = all_of(metric_name))
  gp_fit_input
}


#' Generate tuning parameter values via a Gaussian Process Model
#'
#' @param target The optimization target.
#' @param objective The acquisition function, such as `tune::prob_improve()`, `tune::exp_improve()`, and `tune::conf_bound()`.
#' @param control A control object created by `tune::control_bayes()`.
#' @param ... Options to pass to `GPfit::GP_fit()`.
#' @inheritParams prep_inital
#' @inheritParams guild_to_parameter_set
#' 
#' @return A data frame with suggested values for the tuning parameters.
#' @export
#'
#' @examples
#' past_results <- data.frame( 
#' penalty = c(0.000512266023524435, 0.00261082769969077, 0.000224087518958133,
#'             0.00940059720190888, 3.97878817735671e-06), 
#' mixture = c(0.569303362320061, 0.428850342386868, 0.824966110821115, 
#'             0.22881132167764, 0.373991017234512),
#'             latitude_df = c(23L, 21L, 25L, 38L, 11L), 
#'             longitude_df = c(44L, 7L, 26L, 42L, 50L),
#'             rmse = c(0.0633960168515522, 0.0634472884678756, 0.0635053509670169, 
#'                      0.0643535245578563, 0.064734873635114)
#' )
#' parameter_info <- list(
#' penalty = list(type = "double", range = c(0, 1)),
#' mixture = list(type = "double", range = c(0, 1), inclusive = c(FALSE, FALSE)),
#' latitude_df = list(type = "integer", range = c(2L, 50L)),
#' longitude_df = list(type = "integer", range = c(2L, 50L))
#' )
#' guild_gp(past_results, parameter_info, target = "minimize")
guild_gp <- function(past_results,
                     parameter_info,
                     target = "minimize",
                     objective = tune::exp_improve(),
                     control = tune::control_bayes(),
                     ...) {
  
  # prep guild inputs to mimic tune inputs as closely as necessary
  parameter_set <- guild_to_parameter_set(parameter_info)
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
