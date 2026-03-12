data("survival")

sh_pre <- surv_pre_processor(
  dataset_adsl = survival$adsl,
  adsl_subset = "SAFFL=='Y'",
  dataset_analysis = survival$adtte,
  split_by = NA_character_,
  analysis_subset = "PARAMCD=='OS' & FASFL=='Y'",
  trtsort = "TRT01PN",
  censor_var = "CNSR",
  censor_val = 1,
  trtvar = "TRT01P",
  time_var = "AVAL"
)

sh_plot <- schoenfeld_plot(
  datain = sh_pre,
  disp_clmband = "Y",
  ties_method = "efron",
  time_unit = "Months",
  markersize = 1.5,
  marker_symbol = shape_to_sym(
    shape = "circle"
  ),
  axis_opts = plot_axis_opts(
    xlinearopts = list(
      breaks = ggplot2::waiver(),
      limits = NULL
    ),
    ylinearopts = list(
      breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
      limits = c(0.001, 1000)
    ),
    xaxis_label = "Overall Survival",
    yaxis_label = "Schoenfeld Residuals"
  ),
  pvalue_decimal = 4,
  pair_id = NA
)

test_that("schoenfeld_plot throws expected error message", {
  expect_error(
    schoenfeld_plot(
      datain = sh_pre,
      disp_clmband = "Y",
      ties_method = "efron",
      time_unit = "Months",
      markersize = 1.5,
      marker_symbol = shape_to_sym("circle"),
      axis_opts = plot_axis_opts(),
      pvalue_decimal = 4,
      pair_id = "1~4"
    ),
    "pair_id mismatch with values of TRTSORT from datain"
  )
})

test_that("schoenfeld Plot Works with standard inputs", {
  # Check that expected  outputs type are returned:
  expect_type(sh_plot, "list")

  # check the x and y labels of the graph.
  expect_equal(
    sh_plot[[1]]$s_plot$labels$x,
    "Overall Survival (Months)"
  )
  expect_equal(
    sh_plot[[1]]$s_plot$labels$y,
    "Schoenfeld Residuals"
  )
  # check the length of the output objects
  expect_equal(length(sh_plot), 1)
})

test_that("schoenfeld Plot works as expected with empty dataset", {
  dt <- tibble::tibble()
  pt_empty <- schoenfeld_plot(
    datain = dt,
    disp_clmband = "Y",
    ties_method = "efron",
    time_unit = "Months",
    markersize = 1.5,
    marker_symbol = shape_to_sym("circle"),
    axis_opts = plot_axis_opts(),
    pvalue_decimal = 4,
    pair_id = NA
  )

  expect_length(length(pt_empty), 1)
  expect_equal(names(pt_empty[[1]]), c("pval", "pair", "s_plot"))
})

test_that("scatter_plot works as expected", {
  pt <- map(seq_along(sh_plot), function(i) {
    sh_plot[[i]][[3]]
  })
  purrr::walk(pt, \(x) {
    expect_snapshot(x[["mapping"]])
  })
})
