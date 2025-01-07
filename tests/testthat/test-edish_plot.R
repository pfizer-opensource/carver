merged_data <- adsl_merge(
  adsl = adsl,
  dataset_add = adlb
) |>
  mentry(
    subset = "SAFFL == 'Y'",
    trtvar = "TRT01A",
    trtsort = "TRT01AN"
  )

pt_data <- process_edish_data(
  datain = merged_data,
  xvar = "both",
  alt_paramcd = "L00030S",
  ast_paramcd = "L00028S",
  bili_paramcd = "L00021S"
)

# dataset to test xvar for "ast/alt"
dt_xvar <- process_edish_data(
  datain = merged_data,
  xvar = "ast",
  alt_paramcd = "L00030S",
  ast_paramcd = "L00028S",
  bili_paramcd = "L00021S"
)
series_opts <- plot_aes_opts(pt_data,
  series_size = c(2, 2),
  series_shape = "circle~square"
)
e_plot <- edish_plot(
  datain = pt_data,
  axis_opts = plot_axis_opts(
    xlinearopts = list(
      breaks = c(0.1, 1, 2, 10),
      limits = c(0.1, 10),
      labels = c("0.1", "1", "2x ULN", "10")
    ),
    ylinearopts = list(
      breaks = c(0.1, 1, 3, 10),
      limits = c(0.1, 10),
      labels = c("0.1", "1", "3x ULN", "10")
    ),
    xaxis_label = "Peak ALT/AST (x ULN)",
    yaxis_label = "Peak Total Bilirubin (x ULN)"
  ),
  xrefline = c("2", "gray30", "dashed"),
  yrefline = c("3", "gray30", "dashed"),
  quad_labels =
    "Potential Hy's Law Cases~Temple's Corollary~Gilberts Syndrome or Cholestasis~Normal",
  legend_opts = list(
    label = "Treatment",
    pos = "bottom", dir = "horizontal"
  ),
  series_opts = series_opts,
  interactive = "N"
)

test_that("edish data Works with standard inputs", {
  actual_trt <- unique(pt_data$TRTVAR)

  expect_equal(levels(actual_trt), c("Drug1"))
  expect_equal(nrow(pt_data), 57)

  expect_error(process_edish_data(
    datain = merged_data,
    alt_paramcd = "L00030S",
    ast_paramcd = "L00028S",
    bili_paramcd = "wefewf"
  ), "Please provide valid PARAMCD")

  # test xvar for "ast/alt"
  expect_equal(dt_xvar$XVAR, dt_xvar$ast)
})

test_that("edish_plot works with expected output", {
  expect_type(e_plot, "list")
  expect_true(nrow(e_plot$data) > 0)
  expect_equal(
    e_plot$labels$x,
    "Peak ALT/AST (x ULN)"
  )
  expect_equal(
    e_plot$labels$y,
    "Peak Total Bilirubin (x ULN)"
  )
  expect_true(is.ggplot(e_plot))


  # plotly output comparison
  ptly <- edish_plot(
    datain = pt_data,
    series_opts = series_opts,
    interactive = "Y"
  )
  expect_equal(class(ptly), c("plotly", "htmlwidget"))
})

test_that("snapshot comparison", {
  purrr::walk(c("mapping", "theme", "labels"), function(x) {
    expect_snapshot(e_plot[[x]])
  })
})
