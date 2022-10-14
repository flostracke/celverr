#' Title
#'
#' @param forecasts_resamples The forecasts against the resample set.
#' @param metricset A metricset created with ```yardstick::metric_set()```
#' @param group_col A grouping column for calculating the metrics on a certain
#' level
#'
#' @return A tibble with the calculated metrics.
#' @export
#'
#' @examples
eval_validation_set <- function(
    forecasts_resamples,
    metricset,
    group_col = NULL
  ) {

  # check arguments
  check_colnames_resample_results(forecasts_resamples)

  # check if all models have the same number of forecasts
  n_lengths <- forecasts_resamples %>%
    dplyr::count(.model_desc) %>%
    dplyr::distinct(n) %>%
    dplyr::pull() %>%
    length()

  nr_forecasts_equal <- dplyr::if_else(n_lengths == 1, TRUE, FALSE)

  if(nr_forecasts_equal) {

    metrics <- forecasts_resamples %>%
      group_by(.model_desc, {{group_col}}) %>%
      metricset(truth = value, estimate = prediction) %>%
      arrange(.estimate) %>%
      select(-.estimator)  %>%
      pivot_wider(names_from = .metric, values_from = .estimate)

    return(metrics)


  } else {
    print("DIFFERENT FORECAST METHODS HAVE DIFFERENT NUMBER OF FORECASTS !!!")
    print(count(forecasts_resamples, .model_desc))
  }

}
