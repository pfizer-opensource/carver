ae_pre_process <- ae_pre_processor(
  datain = adae,
  obs_residual = 0
)

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

options(warn = -1)
ae_risk <- risk_stat(
  datain = ae_entry,
  a_subset = ae_pre_process$a_subset,
  summary_by = "Patients",
  eventvar = "AEDECOD",
  ctrlgrp = "Placebo",
  trtgrp = "Xanomeline High Dose",
  statistics = "Risk Ratio",
  alpha = 0.05,
  cutoff_where = "PCT > 2",
  sort_opt = "Ascending",
  sort_var = "Count",
  hoveryn = "Y"
)
fp <- forest_plot_base(
  ae_risk,
  xvar = "RISK",
  yvar = "DPTVAL",
  xminvar = "RISKCIL",
  xmaxvar = "RISKCIU",
  hovervar = "HOVER_RISK",
  series_var = "TRTPAIR",
  xrefline = 1,
  hline_y = "Y",
  axis_opts = plot_axis_opts(
    xaxis_label = "Risk Ratio",
    xopts = list(labelsize = 8)
  ),
  legend_opts = list(pos = "bottom", dir = "horizontal")
)
test_that("Test case 1: Forest Plot Base Works with standard inputs", {
  expect_true(is.ggplot(fp))
  expect_equal(fp[["data"]], ae_risk)
  purrr::walk(c("mapping", "layers", "theme"), \(x) expect_snapshot(fp[[x]]))
  expect_error(forest_plot_base(
    datain = data.frame(),
    xvar = "RISK",
    yvar = "DPTVAL",
    xminvar = "RISKCIL",
    xmaxvar = "RISKCIU",
    hovervar = "HOVER_RISK",
    series_var = "TRTPAIR",
    xrefline = 1,
    hline_y = "N",
    axis_opts = plot_axis_opts(
      xaxis_label = "Risk Ratio",
      xopts = list(labelsize = 8)
    )
  ))
})

# Forest Scatter plot
sp <-
  forest_plot_scatter(
    datain = ae_risk,
    xvar = "PCT",
    yvar = "DPTVAL",
    series_var = "TRTVAR",
    series_opts = list(
      color = g_seriescol(ae_risk, c("black", "goldenrod"), "TRTVAR"),
      shape = g_seriessym(ae_risk, NA, "TRTVAR"),
      size = rep(1, 2)
    ),
    hovervar = "HOVER_PCT",
    xaxis_pos = "top",
    legend_opts = list(pos = "bottom", dir = "horizontal"),
    hline_y = "Y",
    axis_opts = list(xsize = 8, xtsize = 4, xaxis_label = "Percentage")
  )
test_that("Test case 1: Forest Plot Scatter Works with standard inputs", {
  expect_true(is.ggplot(sp))
  expect_equal(sp[["data"]], ae_risk)
  purrr::walk(c("mapping", "layers", "theme"), \(x) expect_snapshot(sp[[x]]))
})

test_that("Test Case 2: Forest plot scatter errors resolve correctly", {
  expect_error(
    forest_plot_scatter(
      datain = data.frame(),
      xvar = "PCT",
      yvar = "DPTVAL",
      series_var = "TRTVAR",
      series_opts = list(
        color = g_seriescol(ae_risk, c("black", "goldenrod"), "TRTVAR"),
        shape = g_seriessym(ae_risk, NA, "TRTVAR"),
        size = rep(1, 2)
      ),
      hovervar = "HOVER_PCT",
      xaxis_pos = "top",
      hline_y = "N",
      axis_opts = list(xsize = 8, xtsize = 6, xaxis_label = "Percentage")
    )
  )
})

tt <- ae_risk |>
  mutate(XVAR = "HT") |>
  tbl_to_plot(
    yvar = "DPTVAL",
    labelvar = "DPTVAL",
    text_size = 2,
    axis_opts = list(xaxis_label = "", xsize = 8, xtsize = 0)
  ) +
  theme(axis.text.x = element_blank())
test_that("Test Case 1: forest_display static works correctly", {
  actual <- forest_display(
    plot_list = list(splot = sp, fplot = fp),
    rel_widths = c(0.6, 0.4),
    interactive = "N"
  )
  expect_type(actual, "list")
  expect_true("ggplot" %in% class(actual))
  expect_true("waiver" %in% class(actual$data))
  expect_length(actual$layers, 3)
  expect_error(
    forest_display(
      plot_list = list(sp = sp, fp = fp),
      rel_widths = c(0.6, 0.4),
      interactive = "N"
    )
  )
  expect_error(
    forest_display(
      plot_list = list(splot = sp, fplot = fp),
      rel_widths = c(0.6),
      interactive = "N"
    ),
    "rel_widths should be equal to the number of plot columns"
  )
})

test_that("Test Case 1: forest_display interactive works correctly", {
  actual <- forest_display(
    plot_list = list(termtable = tt, splot = sp, fplot = fp),
    rel_widths = c(0.25, 0.38, 0.27),
    interactive = "Y",
    plot_height = 800,
    xpos = "top"
  )
  expect_type(actual, "list")
  expect_true("plotly" %in% class(actual))
  expect_true(actual$x$subplot)
  expect_equal(actual$height, 800)
  expect_snapshot(actual$x$layout)
})
