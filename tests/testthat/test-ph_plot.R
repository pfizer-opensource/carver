data("survival")

ph_pre <- surv_pre_processor(
  dataset_adsl = survival$adsl,
  adsl_subset = "SAFFL=='Y'",
  dataset_analysis = survival$adtte,
  split_by = NA_character_,
  analysis_subset = "PARAMCD=='PFS'",
  trtsort = "TRT01PN",
  censor_var = "CNSR",
  censor_val = 1,
  trtvar = "TRT01P",
  time_var = "AVAL"
)


series_opts <- plot_aes_opts(ph_pre,
  series_color = "red~blue~green",
  series_shape = c(1, 1, 1),
  series_size = c(1.5, 1.5, 1.5)
)

test_that("checking the pair_id arg", {
  case1 <- ph_plot(
    datain = ph_pre,
    axis_opts = plot_axis_opts(),
    series_opts = series_opts,
    pair_id = "1-2"
  )
  expect_match(class(case1), "list")
  expect_equal(length(case1), 1)
  purrr::walk(case1, \(x) expect_snapshot(x[["mapping"]]))
})


test_that("checking the pair_id arg with NA", {
  case2 <- ph_plot(
    datain = ph_pre,
    axis_opts = plot_axis_opts(),
    series_opts = series_opts,
    pair_id = NA
  )
  expect_equal(length(case2), 1)
  expect_match(class(case2), "list")
  purrr::walk(case2, \(fig) expect_s3_class(fig, "gg"))
})

test_that("check whether the fucntion throw NULL output when the data has 0 observation.", {
  expect_true(is.null(ph_plot(
    datain = data.frame(),
    axis_opts = plot_axis_opts(),
    series_opts = series_opts,
    pair_id = NA
  )))
})
