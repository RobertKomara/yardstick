context("Markedness")

# ------------------------------------------------------------------------------

lst <- data_altman()
pathology <- lst$pathology
path_tbl <- lst$path_tbl

pred_ch <- quote(scan)

test_that('Two class', {
  expect_equal(
    marked(pathology, truth = "pathology", estimate = "scan")[[".estimate"]],
    (231/263) + (54/81)  - 1
  )
  expect_equal(
    marked(path_tbl)[[".estimate"]],
    (231/263) + (54/81)  - 1
  )
  expect_equal(
    marked(pathology, pathology, scan)[[".estimate"]],
    (231/263) + (54/81)  - 1
  )
})

# ------------------------------------------------------------------------------

multi_ex <- data_three_by_three()
micro <- data_three_by_three_micro()

test_that('Three class', {

  expect_equal(
    marked(multi_ex, estimator = "macro")[[".estimate"]],
    macro_metric(marked_binary)
  )
  expect_equal(
    marked(multi_ex, estimator = "macro_weighted")[[".estimate"]],
    macro_weighted_metric(marked_binary)
  )
  expect_equal(
    marked(multi_ex, estimator = "micro")[[".estimate"]],
    with(micro, sum(tp) / sum(p) + sum(tn) / sum(n) - 1)
  )
})
