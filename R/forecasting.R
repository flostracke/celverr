#' Title
#'
#' @param modellist A list of modeltime workflows.
#' @param cv_plan A split object created by ```modeltime.resample::time_series_cv```
#' @param forecast_horizon Number of timestamps we have to forecast
#' @param train_data The used training data for adding back the features.
#' @param use_parallel Should parallel processing be used.
#' @param inner_loop_var Used for optimising memory usage. Data is chunked by this variable.
#'
#' @return
#' @export
#'
#' @examples
fcst_local_models_groups <- function(
    modellist,
    cv_plan,
    forecast_horizon,
    train_data,
    use_parallel = TRUE,
    inner_loop_var
) {



  inner_loop_ids <- train_data %>%
    dplyr::distinct({{inner_loop_var}}) %>%
    dplyr::pull()

  all_forecasts <- tibble::tibble()
  all_oos_forecasts <- tibble::tibble()

  for (current_split_id in cv_plan$id) {

    current_slice <- cv_plan %>%
      dplyr::filter(id == current_split_id)

    slice_forecasts <- tibble::tibble()
    oos_forecasts <- tibble::tibble()

    for(current_inner_loop_id in inner_loop_ids) {

      current_train <- current_slice$splits[[1]] %>%
        rsample::training() %>%
        dplyr::filter({{inner_loop_var}} == current_inner_loop_id)

      current_dates_train <- current_train %>%
        dplyr::summarise(min = min(date), max = max(date))

      current_oos <- current_slice$splits[[1]] %>%
        rsample::assessment() %>%
        dplyr::filter({{inner_loop_var}} == current_inner_loop_id)

      current_dates_oos <- current_train %>%
        dplyr::summarise(min = min(date), max = max(date))

      print(glue::glue("Current Split: {current_split_id}; Current Inner Loop: {current_inner_loop_id}; Train: {current_dates_train$min} - {current_dates_train$max}; Out Of Sample: {current_dates_oos$min} - {current_dates_oos$max} "))

      # now we have filtered the data for the current slice and the current inner
      # loop and can prepare it for forecasting

      tbl_ts_nested <- current_train %>%
        dplyr::bind_rows(current_oos) %>%
        modeltime::extend_timeseries(.date_var = date, .id_var = series_id, .length_future = forecast_horizon) %>%
        modeltime::nest_timeseries(.id_var = series_id, .length_future = forecast_horizon) %>%
        modeltime::split_nested_timeseries(.id_var = series_id, .date_var = date, .length_test = forecast_horizon)

      modeltime::parallel_start(parallel::detectCores() - 1)
      # fit the models
      nested_modeltime_tbl <- tbl_ts_nested %>%
        modeltime::modeltime_nested_fit(
          model_list = modellist,
          control = modeltime::control_nested_fit(
            verbose   = TRUE,
            allow_par = use_parallel,
            packages = "thief"
          )
        )
      modeltime::parallel_stop()

      current_inner_forecats <- nested_modeltime_tbl %>%
        modeltime::extract_nested_test_forecast(.include_actual = FALSE) %>%
        dplyr::mutate(id = current_split_id) %>%
        dplyr::rename(date = .index)


      slice_forecasts <- slice_forecasts %>%
        dplyr::bind_rows(current_inner_forecats)

      # if it is the final slice we want to use ist for oos forecasting
      #the final fit

      if(current_split_id == "Slice1") {

        modeltime::parallel_start(parallel::detectCores() - 1)
        tbl_forecasts_oos <- nested_modeltime_tbl %>%
          modeltime::modeltime_nested_refit(control = modeltime::control_nested_refit(
            allow_par = TRUE,
            packages = "thief"
          )
          ) %>%
          modeltime::extract_nested_future_forecast() %>%
          dplyr::filter(.model_desc != "ACTUAL") %>%
          celverr::fix_model_names()
        modeltime::parallel_stop()

        oos_forecasts <- oos_forecasts %>%
          dplyr::bind_rows(tbl_forecasts_oos)
      }

      rm(nested_modeltime_tbl)
      gc()

    }

    # outer loop
    all_forecasts <- all_forecasts %>%
      dplyr::bind_rows(slice_forecasts)

    all_oos_forecasts <- all_oos_forecasts %>%
      dplyr::bind_rows(oos_forecasts)
  }



  all_forecasts <- all_forecasts %>%
    dplyr::transmute(
      date,
      series_id,
      .model_id,
      .model_desc,
      id,
      prediction = .value
    ) %>%
    dplyr::left_join(
      train_data %>% dplyr::select(date, series_id, value),
      by = c("date", "series_id")
    )

  return(list(cv_forecasts = all_forecasts, oos_forecasts = all_oos_forecasts))

}
