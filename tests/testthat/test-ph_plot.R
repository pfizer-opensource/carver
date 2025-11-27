data("survival")

survival$adsl <- survival$adsl |>
  dplyr::mutate(
    TRT01PN = dplyr::case_when(
      TRT01P == "Xanomeline Low Dose" ~ 1,
      TRT01P == "Placebo" ~ 2,
      TRT01P == "Screen Failure" ~ 3,
      TRUE ~ NA_real_
    )
  )

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
  expect_equal(length(case1), 1)
  expect_match(class(case1), "list")
  purrr::walk(case1, \(x) purrr::walk(c("mapping", "labels"), \(y) expect_snapshot(x[[y]])))
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
  expect_true(nrow(case2[[1]][[1]]) > 0)
})

test_that("check whether the fucntion throw NULL output when the data has 0 observation.", {
  expect_true(is.null(ph_plot(
    datain = data.frame(),
    axis_opts = plot_axis_opts(),
    series_opts = series_opts,
    pair_id = NA
  )))
})

