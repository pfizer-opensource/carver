# Test stat Utils functions

# Function: fmtrd
test_that("Test fmtrd function for calculation and precision", {
  # 1: Applying fmtrd function with mean
  # Test Data
  sample_data <- c(10.23, 20, NA, 40, 50, 75.567)
  mean_1 <- round_f(mean(sample_data, na.rm = TRUE), 2)
  max_1 <- round_f(max(sample_data, na.rm = TRUE), 2)
  expect_equal(fmtrd("mean")(sample_data), mean_1)
  expect_equal(fmtrd("max")(sample_data), max_1)
  # Significant decimal resolved correctly
  mean_2 <- round_f(mean(sample_data, na.rm = TRUE), 3)
  expect_equal(fmtrd("mean", d = 3)(sample_data), mean_2)
})

test_that("Test parse_stats", {
  actual <- parse_stats(
    statvar = c("mean(sd)", "median(minmax)", "stderr"),
    statdec = c("3(2)", "2(1)", "3")
  )
  expected <- setNames(
    c("3", "2", "2", "1", "1", "3"),
    c("mean", "sd", "median", "min", "max", "stderr")
  )
  expect_equal(actual, expected)
  actual1 <- parse_stats(
    statvar = c("mean(sd)", "sd"),
    statdec = ""
  )
  expect_equal(actual1, c("mean" = "2", "sd" = "2"))
  actual2 <- parse_stats(
    statvar = c("mean", "sd"),
    statdec = 2
  )
  expect_equal(actual2, c("mean" = 2, "sd" = 2))
})

dist <- rnorm(20)
test_that("Test summary_functions", {
  stats <- c(
    "mean", "q1", "geomean", "geosd", "geomean_lowci", "geomean_upci",
    "outliers", "n", "newstat"
  )
  actual <- summary_functions(
    statvar = stats,
    statdec = rep(2, 8)
  )
  expect_named(actual, stats)
  expect_equal(actual$newstat(), "_NO_STAT")
  logx <- log(dist)
  margin <- qt(0.975, df = length(logx) - 1) * sd(logx, na.rm = TRUE) / sqrt(length(logx))
  expected <- round_f(exp(mean(logx, na.rm = TRUE) + c(-1, 1) * margin), 2)
  expect_equal(actual$geomean_lowci(dist), expected[1])
  expect_equal(actual$geomean_upci(dist), expected[2])
  expect_true(is.character(actual$outliers(dist)))
})

test_that("Test summary_functions outputs", {
  stats <- c(
    "mean", "q1", "geomean", "geosd", "geomean_lowci", "geomean_upci",
    "outliers", "n", "newstat"
  )
  actual <- summary_functions(
    statvar = stats,
    statdec = rep(2, 8)
  )
  expect_named(actual, stats)
  expect_equal(actual$newstat(), "_NO_STAT")
})

test_that("Test Tukey's stats", {
  exp <- min(dist[(dist >= (quantile(dist, 0.25, na.rm = TRUE) - 1.5 * IQR(dist, na.rm = TRUE))) &
    (dist <= quantile(dist, 0.25, na.rm = TRUE))], na.rm = TRUE)
  expect_equal(whiskerlow(dist), exp)
  exp2 <- max(dist[(dist <= (quantile(dist, 0.75, na.rm = TRUE) + 1.5 * IQR(dist, na.rm = TRUE))) &
    (dist >= quantile(dist, 0.75, na.rm = TRUE))], na.rm = TRUE)
  expect_equal(whiskerup(dist), exp2)
})

test_that("Test derv_stats", {
  actual <- msumstat(adsl,
    dptvar = "AGE",
    statvar = "stderr~mean(sd)",
    sigdec = "2~3(2)"
  )
  testout <- msumstat(adsl,
    dptvar = "AGE",
    statvar = "stderr~mean~sd",
    sigdec = "2~3~2"
  )
  expected <- testout$gsum |>
    dplyr::mutate(`mean(sd)` = paste0(.data[["mean"]], " (", .data[["sd"]], ")")) |>
    select(all_of(names(actual$gsum)))
  expect_equal(actual$gsum, expected)
})
