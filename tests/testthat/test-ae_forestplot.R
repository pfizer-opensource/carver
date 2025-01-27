data("ae_pre_process")
ae_entry <- mentry(
  datain = ae_pre_process$data,
  subset = NA,
  byvar = "AEBODSYS",
  trtvar = "TRTA",
  trtsort = "TRTAN",
  subgrpvar = NA,
  trttotalyn = "N",
  add_grpmiss = "N",
  sgtotalyn = "N",
  pop_fil = "SAFFL"
)
ae_risk_forest <- risk_stat(
  datain = ae_entry,
  a_subset = ae_pre_process$a_subset,
  summary_by = "Patients",
  eventvar = "AEDECOD",
  ctrlgrp = "Placebo",
  trtgrp = "Xanomeline High Dose",
  statistics = "Risk Ratio",
  alpha = 0.05,
  cutoff_where = "PCT > 5",
  sort_opt = "Ascending",
  sort_var = "Count"
) |>
  plot_display_bign(ae_entry)
series_opts <- ae_risk_forest |>
  plot_aes_opts(series_color = c("black", "royalblue2"))
# AE Forest Plot

axis_opts1 <- plot_axis_opts(
  xaxis_label = "Risk Ratio",
  xopts = list(labelsize = 8, ticksize = 8)
)
axis_opts <- append(axis_opts1, list(xpos = "top"))
ae_risk1_forest <- risk_stat(
  datain = ae_entry,
  a_subset = ae_pre_process$a_subset,
  summary_by = "Patients",
  eventvar = "AEDECOD",
  ctrlgrp = "Placebo",
  trtgrp = "Xanomeline High Dose~~Xanomeline Low Dose",
  statistics = "Risk Ratio",
  alpha = 0.05,
  cutoff_where = "PCT > 5",
  sort_opt = "Ascending",
  sort_var = "Count",
  g_sort_by_ht = "Y"
) |>
  plot_display_bign(ae_entry)
series_opts1 <- plot_aes_opts(ae_risk1_forest, series_color = "black~black~black")

forest_dat <- ae_risk_forest |>
  filter(!is.nan(.data[["RISK"]]), !is.infinite(.data[["RISK"]]))
test_that("Test Case 1: Standard error checks", {
  expect_error(
    ae_forest_plot(
      datain = ae_risk_forest[0, ],
      series_opts = list(
        color = g_seriescol(ae_risk_forest, c("black", "royalblue2"), "TRTVAR"),
        shape = g_seriessym(ae_risk_forest, NA, "TRTVAR"),
        size = rep(1, 2)
      ),
      axis_opts = plot_axis_opts(
        xaxis_label = "Risk Ratio",
        xopts = list(labelsize = 8)
      )
    ),
    "Input ae_forest_plot data is empty"
  )
  expect_error(
    ae_forest_plot(
      datain = ae_risk_forest,
      series_opts = list(
        color = g_seriescol(ae_risk_forest, c("black", "royalblue2"), "TRTVAR"),
        shape = g_seriessym(ae_risk_forest, NA, "TRTVAR"),
        size = rep(1, 2)
      ),
      rel_widths = c(0.38, 0.27),
      axis_opts = plot_axis_opts(
        xaxis_label = "Risk Ratio",
        xopts = list(labelsize = 8)
      ),
      text_size = 2.4,
      term_label = "Reported Term for the Adverse Event",
      risk_ref = 1,
      highlight_sig = "N"
    ),
    "rel_widths should be equal to the number of plot columns"
  )
})
test_that("Test Case 2: Standard Inputs 1", {
  forest1 <- ae_forest_plot(
    datain = ae_risk_forest,
    series_opts = series_opts,
    trtpair_color = c("#F8766D", "#00BFC4"),
    axis_opts = axis_opts,
    term_label = "Reported Term for the Adverse Event",
    highlight_sig = "Y",
    rel_widths = c(0.5, 0.35, 0.15),
    ht_dispyn = "N",
    pvalue_dispyn = "Y",
    terms_perpg = NULL
  )
  expect_type(forest1, "list")
  expect_length(forest1, 1)
  expect_true("ggplot" %in% class(forest1[[1]]))
  expect_length(forest1[[1]][["layers"]], 3)
  purrr::walk(forest1[[1]][["layers"]], \(x) {
    expect_snapshot(x[["geom_params"]][-1])
  })
})

test_that("Test Case 3: Standard Inputs 2", {
  forest2 <- ae_forest_plot(
    datain = ae_risk1_forest,
    series_opts = series_opts1,
    axis_opts = axis_opts1,
    term_label = "",
    risk_ref = 1,
    pairwise = "Y",
    highlight_sig = "N",
    rel_widths = c(0.25, 0.45, 0.3),
    ht_dispyn = "Y",
    pvalue_dispyn = "N",
    terms_perpg = NULL
  )
  expect_type(forest2, "list")
  expect_length(forest2, 2)
  expect_true("ggplot" %in% class(forest2[[1]]))
  expect_length(forest2[[2]][["layers"]], 3)
})
test_that("Test Case 4: Page splitting", {
  forest3 <- ae_forest_plot(
    datain = ae_risk_forest,
    series_opts = series_opts,
    trtpair_color = c("#F8766D", "#00BFC4"),
    axis_opts = axis_opts,
    term_label = "Reported Term for the Adverse Event",
    highlight_sig = "Y",
    rel_widths = c(0.5, 0.35, 0.15),
    ht_dispyn = "N",
    pvalue_dispyn = "Y",
    terms_perpg = 8
  )
  expect_true(length(forest3) > 1)
  purrr::walk(
    forest3,
    \(x) expect_true("ggplot" %in% class(x))
  )
  expect_length(forest3[[3]][["layers"]], 3)
})

# Create scatter plot to test sig points
splot <- forest_plot_scatter(
  datain = ae_risk_forest,
  xvar = "PCT",
  yvar = "DPTVAL",
  series_var = "TRTVAR",
  series_opts = series_opts,
  hovervar = "HOVER_PCT",
  xaxis_pos = "top",
  legend_opts = list(pos = "bottom", dir = "horizontal"),
  axis_opts = list(xsize = 8, xtsize = 6, xaxis_label = "Percentage")
)
test_that("Test Case: Significant Points", {
  actual <- ae_forest_hlt_sig(
    plotin = splot,
    datain = forest_dat,
    pvalue_sig = 0.05,
    pts_size = 1.5
  )
  expect_length(actual[["layers"]], 3)
  expect_true(length(actual[["layers"]]) > length(splot[["layers"]]))
  expect_equal(actual[["labels"]][["fill"]], "EFFECT")
})
