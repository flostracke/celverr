
#' wape
#'
#'The Weighted Absolute Percentage Error (WAPE) measures the overall deviation
#'of forecasted values from observed values. WAPE is calculated by taking the
#'sum of observed values and the sum of predicted values, and calculating the
#'error between those two values. A lower value indicates a more accurate model.
#'When the sum of observed values for all time points and all items is
#'approximately zero in a given backtest window, the weighted absolute
#'percentage error expression is undefined. In these cases, Forecast outputs
#'the unweighted absolute error sum, which is the numerator in the WAPE
#'expression. WAPE is more robust to outliers than
#'Root Mean Square Error (RMSE) because it uses the absolute error instead of
#'the squared error.
#'
#'For reference: https://www.baeldung.com/cs/mape-vs-wape-vs-wmape or
#'https://docs.aws.amazon.com/forecast/latest/dg/metrics.html#metrics-WAPE
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
wape <- function(data, ...) {
  UseMethod("wape")
}

wape <- yardstick::new_numeric_metric(wape, direction = "minimize")

#' @export
#' @rdname wape
wape.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {

  yardstick::metric_summarizer(
    metric_nm = "wape",
    metric_fn = wape_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ...
  )

}

#' @export
#' @rdname wape
wape_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  wape_impl <- function(truth, estimate) {
    error <- truth - estimate
    wape <- (sum(abs(error), na.rm = na_rm)/sum(abs(truth),na.rm = na_rm)) * 100
    return(wape)
  }

  yardstick::metric_vec_template(
    metric_impl = wape_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )

}









