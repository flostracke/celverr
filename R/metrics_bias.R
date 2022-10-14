
#' Bias
#'
#'A forecast bias occurs when there are consistent differences between actual
#'outcomes and previously generated forecasts of those quantities; that is:
#'forecasts may have a general tendency to be too high or too low. A normal
#'property of a good forecast is that it is not biased.
#'
#'For reference: https://en.wikipedia.org/wiki/Forecast_bias
#'
#' @param data A `data.frame` containing the columns specified by the `truth`
#' and `estimate` arguments.
#'
#' @param truth The column identifier for the true results
#' (that is `numeric`). This should be an unquoted column name although
#' this argument is passed by expression and supports
#' [quasiquotation][rlang::quasiquotation] (you can unquote column
#' names). For `_vec()` functions, a `numeric` vector.
#'
#' @param estimate The column identifier for the predicted
#' results (that is also `numeric`). As with `truth` this can be
#' specified different ways but the primary method is to use an
#' unquoted variable name. For `_vec()` functions, a `numeric` vector.
#'
#' @param na_rm A `logical` value indicating whether `NA`
#' values should be stripped before the computation proceeds.
#'
#' @param ... Not currently used.
#'
#' @author Florian Stracke
#'
#' @export
#'
bias <- function(data, ...) {
  UseMethod("bias")
}

bias <- yardstick::new_numeric_metric(bias, direction = "minimize")

#' @export
#' @rdname bias
bias.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {

  yardstick::metric_summarizer(
    metric_nm = "bias",
    metric_fn = bias_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ...
  )

}

#' @export
#' @rdname bias
bias_vec <- function(truth, estimate, na_rm = TRUE, ...) {

    bias_impl <- function(truth, estimate) {
      bias <- mean(truth - estimate)
      return(bias)
  }

  yardstick::metric_vec_template(
    metric_impl = bias_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )

}









