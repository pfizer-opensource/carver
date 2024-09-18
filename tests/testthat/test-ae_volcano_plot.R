data("ae_risk")
vaxis_opts <- ae_volcano_opts(
  datain = ae_risk,
  statistic = "Risk Ratio",
  trt1_label = "Control",
  trt2_label = "Exposure",
  pvalue_trans = "-log10",
  xref_offset = 1
)
axis_opts <- plot_axis_opts(
  ylinearopts = vaxis_opts$ylinearopts,
  yaxis_scale = vaxis_opts$yaxis_scale,
  xaxis_label = vaxis_opts$xaxis_label,
  yaxis_label = vaxis_opts$yaxis_label
)

test_that("Test 1: Volcano plot Options works", {
  expect_type(vaxis_opts, "list")
  expect_named(
    vaxis_opts,
    c("xaxis_label", "yaxis_label", "ylinearopts", "yaxis_scale", "xref")
  )
  expected <- list(
    xaxis_label = "<--- Favors Control (N=69) ---- Favors Exposure (N=79) --->\nRisk Ratio",
    yaxis_label = "-log10 p-value",
    ylinearopts = list(breaks = as.numeric(paste0("1e-", 0:20)), labels = as.character(0:20)),
    yaxis_scale = reverselog_trans(10),
    xref = c(1, 0, 2)
  )
  expect_equal(vaxis_opts, expected)
  vaxis_opts2 <- ae_volcano_opts(
    datain = ae_risk,
    pvalue_trans = "none"
  )
  expected2 <- list(
    xaxis_label = "<--- Favors Control (N=69) ---- Favors Exposure (N=79) --->\nRisk Ratio",
    yaxis_label = "p-value",
    ylinearopts = list(
      breaks = c(0.05, 0, rep(1, 10) / 10^(9:0)),
      labels = as.character(c(0.05, 0, rep(1, 10) / 10^(9:0)))
    ),
    yaxis_scale = "identity",
    xref = c(1, 0, 2)
  )
  expect_equal(vaxis_opts2, expected2)
})

test_that("Test 1: Volcano plot with standard inputs", {
  volcano_test <- ae_volcano_plot(
    datain = ae_risk,
    axis_opts = axis_opts,
    legend_opts = list(label = "", pos = "bottom", dir = "horizontal"),
    xref = vaxis_opts$xref,
    pvalue_sig = 0.05
  )
  expect_type(volcano_test, "list")
  expect_true("ggplot" %in% class(volcano_test))
  expect_snapshot(volcano_test[["mapping"]])
  expect_snapshot(volcano_test[["labels"]])
  volcano_test2 <- ae_volcano_plot(
    datain = ae_risk,
    axis_opts = axis_opts,
    xref = vaxis_opts$xref,
    pvalue_sig = 0.004
  )
  purrr::walk(
    volcano_test2$layers,
    \(x) expect_snapshot(x$aes_params)
  )
})

test_that("Test 2: Volcano plot with interactive output", {
  volcano_test <- ae_volcano_plot(
    datain = ae_risk,
    axis_opts = axis_opts,
    legend_opts = list(label = "", pos = "bottom", dir = "horizontal"),
    xref = vaxis_opts$xref,
    pvalue_sig = 0.05,
    interactive = "Y"
  )
  expect_type(volcano_test, "list")
  expect_true("plotly" %in% class(volcano_test))
  expect_equal(volcano_test$height, 700)
  expect_equal(volcano_test$width, 800)
})
