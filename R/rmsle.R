
#' Root mean squared error
#'
#' Calculate the root mean squared log error. In the case of RMSE, the presence
#' of outliers can explode the error term to a very high value. But, in the
#' case of RMLSE the outliers are drastically scaled down therefore nullifying
#' their effect. RMSLE incurs a larger penalty for the underestimation of the
#' Actual variable than the Overestimation. This is especially useful for
#' business cases where the underestimation of the target variable is not
#' acceptable but overestimation can be tolerated.
#'
#' For reference check this explanation: https://medium.com/analytics-vidhya/root-mean-square-log-error-rmse-vs-rmlse-935c6cc1802a
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
#' @param case_weights The optional column identifier for case weights. This
#' should be an unquoted column name that evaluates to a numeric column in
#' `data`. For `_vec()` functions, a numeric vector.
#'
#' @param ... Not currently used.
#'
#' @author Florian Stracke
#'
#' @export
#'
rmsle <- function(data, ...) {
  UseMethod("rmsle")
}

rmsle <- yardstick::new_numeric_metric(rmsle, direction = "minimize")

rmsle_vec <- function(truth, estimate, na_rm = TRUE, ...) {

  rmsle_impl <- function(truth, estimate) {

    estimate <- ifelse(estimate < 0, 0, estimate)
    sqrt(mean(((log(estimate + 1) - log(truth + 1)))^2))

  }

  yardstick::metric_vec_template(
    metric_impl = rmsle_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )

}

rmsle.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "rmsle",
    metric_fn = rmsle_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    ...
  )

}
