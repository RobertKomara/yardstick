#' marked
#'
#' @description
#' Markedness is defined as:
#'
#' [ppv()] + [npv()] - 1
#'
#' A related metric is Informedness, see the Details section for the relationship.
#'
#' @details
#'
#' The value of markedness ranges from \[0, 1\] and is `1` when there are
#' no false positives and no false negatives.
#'
#' TODO: as defined in Powers, David M W (2011), equation (42).
#'
#' @family class metrics
#' @templateVar metric_fn marked
#' @template event_first
#' @template return
#'
#' @inheritParams sens
#'
#' @author Robert Komara
#'
#' @references
#'
#' Powers, David M W (2011). "Evaluation: From Precision, Recall and F-Score to
#' ROC, Informedness, Markedness and Correlation". Journal of Machine Learning
#' Technologies. 2 (1): 37-63.
#'
#' @template examples-class
#'
#' @export
marked <- function(data, ...) {
  UseMethod("marked")
}

class(marked) <- c("class_metric", "function")

#' @rdname marked
#' @export
marked.data.frame <- function(data, truth, estimate,
                               estimator = NULL,
                               na_rm = TRUE,
                               ...) {

  metric_summarizer(
    metric_nm = "marked",
    metric_fn = marked_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    ... = ...
  )

}

#' @export
marked.table <- function(data, estimator = NULL, ...) {
  check_table(data)
  estimator <- finalize_estimator(data, estimator)

  metric_tibbler(
    .metric = "marked",
    .estimator = estimator,
    .estimate = marked_table_impl(data, estimator)
  )

}

#' @export
marked.matrix <- function(data, estimator = NULL, ...) {

  data <- as.table(data)
  marked.table(data, estimator)

}

#' @rdname marked
#' @export
marked_vec <- function(truth, estimate, estimator = NULL,
                        na_rm = TRUE, ...) {

  estimator <- finalize_estimator(truth, estimator)

  marked_impl <- function(truth, estimate) {

    xtab <- vec2table(
      truth = truth,
      estimate = estimate
    )

    marked_table_impl(xtab, estimator)

  }

  metric_vec_template(
    metric_impl = marked_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "factor",
    estimator = estimator,
    ...
  )

}

marked_table_impl <- function(data, estimator) {

  if(is_binary(estimator)) {
    marked_binary(data)
  } else {
    w <- get_weights(data, estimator)
    out_vec <- marked_multiclass(data, estimator)
    weighted.mean(out_vec, w)
  }

}

marked_binary <- function(data) {
  # ppv + npv - 1
  ppv_binary(data) + npv_binary(data) - 1
}

marked_multiclass <- function(data, estimator) {
  ppv_multiclass(data, estimator) + npv_multiclass(data, estimator) - 1
}
