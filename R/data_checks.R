

#' Helper function which can check if the needed columns are inside a Dataframe.
#'
#' @param df A Dataframe. which should be checked.
#' @param expected_col_names A list of the column names which should be in the
#' Dataframe.
#'
#' @return Returns a Error Message if the the columns is not in the Dataframe.
#' @export
#'
#' @examples
check_colnames_present <- function(df, expected_col_names) {

  present_columns <- colnames(df)
  check_result <- setdiff(expected_col_names, present_columns)

  if(length(check_result) > 0) {
    missing_columns <- stringi::stri_paste(check_result, collapse=', ')
    stop(glue::glue("The following columns are missing: {missing_columns}"))
  }

}

#' Check a data frame containing forecasts against a resample set.
#'
#' @param df A Dataframe. which should be checked.
#'
#' @return Returns a Error Message if the the columns is not in the Dataframe.
#' @export
#'
#' @examples
check_colnames_resample_results <- function(df) {

  res <- check_colnames_present(
    df,
    c("date", "series_id", ".model_desc", "prediction", "value", "id")
  )
}

#' Check a data frame containing forecasts against a ot of sample set.
#'
#' @param df A Dataframe. which should be checked.
#'
#' @return Returns a Error Message if the the columns is not in the Dataframe.
#' @export
#'
#' @examples
check_colnames_oos_results <- function(df) {

  res <- check_colnames_present(
    df,
    c("date", "series_id", ".model_desc", "prediction")
  )
}
