#' Averages non determistic forecast models
#'
#' If the created forecasts of a forecast function are not deterministic this
#' function creates a mean ensemble of ```n_iterations```.
#'
#' @param model_table A modeltime table with the trained models.
#' @param out_of_sample_data The data which should be forecasted.
#' @param n_iterations Number of how many forecasts are averaged.
#'
#' @return The summarised forecast as a tibble.
#' @export
#'
#' @examples
stabilize_forecasts <- function(
  model_table,
  out_of_sample_data,
  n_iterations = 10) {

  all_forecasts <- tibble::tibble()

  for (i in 1:n_iterations) {

    print(glue::glue("current iteration: {i} / {n_iterations}"))
    current_forecasts <- model_table %>%
      modeltime::modeltime_forecast(out_of_sample_data, keep_data = TRUE)

    all_forecasts <- dplyr::bind_rows(all_forecasts, current_forecasts)
  }

  final_forecasts <- all_forecasts %>%
    dplyr::group_by(.model_id, .model_desc, .key, .index, series_id) %>%
    dplyr::summarise(.value = median(.value), .groups = "drop")

  return(final_forecasts)

}


#' Ensures that forecasts are in the correct data format. Only the needed
#' columns are selected.
#'
#' We have some assumptions about column names produced by the forecasts for
#' downstream useage. This function checks the data format.
#'
#' @param x The dataframe with the produced forecasts.
#' @param type Can be "validation" or "forecast". There are minor differences
#' in the expected columns between the 2 types.
#'
#' @return The dataframe with the needed columns
#' @export
#'
#' @examples
guarante_output <- function(x, type = "validation") {

  if (type == "validation") {
    result <- x %>%
      dplyr::select(date, series_id, .model_desc, prediction, value, id)
  }

  if (type == "forecast") {
    result <- x %>%
      dplyr::select(date, series_id, .model_desc, prediction)
  }

  return(result)

}


#' Cleans some names of some forecasting models.
#'
#' The ETS models and the TBATS models don't have consistet naming. The f
#' unctions ensures that all ETS models are called ETS and all BATS models are
#' called TBATS.
#'
#' @param x The dataframe with the produced forecasts.
#'
#' @return The dataframe with the cleaned model names.
#'
#' @examples
#' @export

fix_model_names <- function(x) {

  x %>%
    dplyr::mutate(.model_desc = dplyr::if_else(
      stringr::str_detect(.model_desc, pattern = "ETS"),
      "ETS",
      .model_desc)) %>%
    dplyr::mutate(.model_desc = dplyr::if_else(
      stringr::str_detect(.model_desc, pattern = "BATS "),
      "TBATS",
      .model_desc)
    )

}

#' Combine the validation forecasts with the split object.
#'
#' @param split_obj A split object created by
#' ```modeltime.resample::time_series_cv```
#' @param fit_resamples_res The fitted resamples produced by
#' ````modeltime::modeltime_fit_resamples``
#'
#' @return The combined forecasts with the split object.
#' @export
#'
#' @examples
combine_splits_and_preds <- function(
    split_obj,
    fit_resamples_res # result of modeltime.resample::modeltime_fit_resamples
) {

  # tibble for storing the final output
  combined_data <- tibble::tibble()

  # get a list with the split ids for iterating
  splits <- split_obj$id

  for (current_split in splits) {

    # get the predictions for the current split
    current_predictions <- fit_resamples_res %>%
      tidyr::unnest(.resample_results) %>%
      tidyr::unnest(.predictions) %>%
      dplyr::filter(id == current_split) %>%
      dplyr::select(.model_id, .model_desc, id, .pred, .row, .config)

    # get the data from the current slit
    current_slice <- split_obj %>%
      dplyr::filter(id == current_split)


    data_for_current_split_train <- current_slice$splits[[1]] %>%
      rsample::training()
    data_for_current_split_asses <- current_slice$splits[[1]] %>%
      rsample::assessment()

    data_for_current_split <- dplyr::bind_rows(
      data_for_current_split_train,
      data_for_current_split_asses
    ) %>%
    dplyr::mutate(.row = dplyr::row_number())

    # combine the extracted data with the predictions
    combined_data_current_split <- current_predictions %>%
      dplyr::left_join(data_for_current_split, by = ".row")

    # append the data to the result tibble
    combined_data <- combined_data %>%
      dplyr::bind_rows(combined_data_current_split)

  }

  return(combined_data %>%
           dplyr::transmute(
             date,
             series_id,
             .model_id,
             .model_desc,
             prediction = .pred,
             value,
             id
           )
  )

}
