data(tornado_plot_data)

tornado_df <- process_tornado_data(
  dataset_adsl = tornado_plot_data[["adsl"]],
  dataset_analysis = tornado_plot_data[["adae"]],
  adsl_subset = "SAFFL == 'Y'",
  analysis_subset = NA_character_,
  ae_filter = "Treatment emergent",
  obs_residual = "30",
  fmq_data = NA,
  ae_catvar = "AESEV",
  trtvar = "ARMCD",
  trt_left = "A",
  trt_right = "A",
  pop_fil = "Overall Population",
  pctdisp = "TRT",
  denom_subset = NA_character_,
  legendbign = "N",
  yvar = "AESOC"
)

series_opts <- g_seriescol(tornado_df, "blue~yellow~red", "BYVAR1")

plot_out <- tornado_plot(
  tornado_df,
  trt_left_label = "DRUG B",
  trt_right_label = "DRUG C",
  bar_width = 0.5,
  axis_opts = plot_axis_opts(
    xaxis_label = "Primary System Organ Class",
    yaxis_label = "% of Subjects",
    ylinearopts = list(
      breaks = seq(-100, 100, 10),
      labels = c(seq(100, 0, -10), seq(10, 100, 10))
    )
  ),
  legend_opts = list(
    label = "Severity",
    pos = c(0.15, 0.15),
    dir = "vertical"
  ),
  series_opts = series_opts,
  griddisplay = "N"
)

test_that("Test Case 1: process_tornado_data throws expected error message", {
  expect_error(
    process_tornado_data(
      dataset_adsl = tornado_plot_data[["adsl"]],
      dataset_analysis = data.frame(),
      adsl_subset = "SAFFL == 'Y'",
      analysis_subset = NA_character_,
      ae_filter = "Treatment emergent",
      obs_residual = "30",
      fmq_data = NA,
      ae_catvar = "AESEV",
      trtvar = "ARMCD",
      trt_left = "A",
      trt_right = "A",
      pop_fil = "Overall Population",
      pctdisp = "TRT",
      denom_subset = NA_character_,
      legendbign = "N",
      yvar = "AESOC"
    ),
    "Analysis data is empty"
  )
})

test_that("Test Case 2: tornado data works with expected inputs", {
  expect_true(is.data.frame(tornado_df))
  expect_true(all(c("BYVAR1", "XVAR", "trt_left", "trt_right") %in% colnames(tornado_df)))
  expect_true(nrow(tornado_df) > 0)
  expect_true(length(tornado_df) > 0)
})

test_that("Test Case 3: tornado_plot works with expected inputs", {
  legendgroups <- unique(plot_out[["data"]][["BYVAR1"]])

  expect_true(is.ggplot(plot_out))
  expect_type(plot_out, "list")
  expect_equal(legendgroups, unique(plot_out[["data"]][["BYVAR1"]]))
  expect_true(nrow(plot_out$data) > 0)
  expect_true(length(plot_out) > 0)
})

test_that("Test Case 4: tornado_plot throws expected error message", {
  trt_left <- "`Placebo123`"
  trt_right <- "`Xanomeline Low Dose321`"
  expect_error(
    tornado_plot(
      adae,
      series_opts = series_opts
    ),
    "XVAR Treatment values not in data"
  )
})

test_that("Test Case 5: tornado_plot creates tornado plot", {
  purrr::walk(
    plot_out, c("mapping", "theme", "labels"),
    \(y) expect_snapshot(plot_out[[y]])
  )
})
