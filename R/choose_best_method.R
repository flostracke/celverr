

#' Choose per group the best performing forecast method.
#'
#' Assuming we created forecasts with multiple different algorithms we want
#' to choose the best performing algorithm based on a metric, calculated from
#' the resample set. The forecast then chooses the out of sample forecasts. Both the choosen
#' forecasts against the resample set and the out of sample set are returned
#' in a list.
#'
#' @param forecasts_resamples The forecasts against the resample set.
#' @param forecasts_oos The forecasts against the out of sample set.
#' @param metric The metric used for deciding which is the best method. Has to
#' be a ```yardstick``` metric.
#' @param group_col The column on which level the decision should be made.
#' Default = series_id
#'
#' @return
#' @export
#'
#' @examples
choose_best_method <- function(
    forecasts_resamples,
    forecasts_oos,
    metric,
    group_col = series_id
  ) {

  # check arguments
  check_colnames_resample_results(forecasts_resamples)
  check_colnames_oos_results(forecasts_oos)

  best_method_per_group <- forecasts_resamples %>%
    dplyr::group_by(.model_desc, {{group_col}}) %>%
    metric(value, prediction) %>%
    dplyr::group_by({{group_col}}) %>%
    dplyr::slice_min(order_by = .data$.estimate, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(.model_desc, {{group_col}})

  # get a string represantion
  group_name <- as.character(substitute(group_col))

  best_forecasts_per_group_resamples <- forecasts_resamples %>%
    dplyr::inner_join(
      best_method_per_group,
      by = c(group_name, ".model_desc")
    ) %>%
    dplyr::mutate(.model_desc_orig = .model_desc) %>%
    dplyr::mutate(
      .model_desc = glue::glue("best_method_per_{group_name}")
    )

  best_forecasts_per_group_oos <- forecasts_oos %>%
    dplyr::inner_join(
      best_method_per_group,
      by = c(group_name, ".model_desc")
    ) %>%
    dplyr::mutate(.model_desc_orig = .model_desc) %>%
    dplyr::mutate(.model_desc = glue::glue("best_method_per_{group_name}"))

  return(
    list(
      validation = best_forecasts_per_group_resamples,
      out_of_sample = best_forecasts_per_group_oos
    )
  )

}

#' Plots the choosen models based on the validation ouput of `choose_best_method`
#'
#' @param best_method_val_results The output validation dataframe of `choose_best_method`
#' @param subtitle specifiy the subtitle for the plot
#'
#' @return
#' @export
#'
#' @examples
plot_best_choosen_method <- function(best_method_val_results, subtitle = "") {

  method_counts <- best_method_val_results %>%
    dplyr::distinct(series_id, .data$.model_desc_orig) %>%
    dplyr::count(.data$.model_desc_orig)

  best_method_val_results %>%
    dplyr::left_join(method_counts, by = ".model_desc_orig") %>%
    dplyr::distinct(series_id, .data$.model_desc_orig, .data$n) %>%
    ggplot2::ggplot(ggplot2::aes(forcats::fct_reorder(.data$.model_desc_orig, .data$n))) +
    ggplot2::geom_bar(fill = "#3E606F") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Best choosen Forecasting Model",
      x = "Forecasting Model",
      subtitle = subtitle
    )

}
