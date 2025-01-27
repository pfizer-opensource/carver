data(adsl)
adsl_entry <- mentry(
  datain = adsl,
  subset = "EFFFL=='Y'",
  byvar = "RACE",
  trtvar = "TRT01A",
  trtsort = "TRT01AN",
  pop_fil = NA
)

adsl_sum <- msumstat(
  datain = adsl_entry,
  dptvar = "AGE",
  statvar = "mean",
  figyn = "Y"
)[["gsum"]] |>
  mutate(
    XVAR = BYVAR1,
    YVAR = as.numeric(mean)
  )

test_that("Test Case 1: bar_plot works with expected inputs", {
  bar_out <- bar_plot(
    datain = adsl_sum,
    flip_plot = "N",
    series_opts = list(
      color = c("red", "gold", "cyan")
    ),
    axis_opts = plot_axis_opts(),
    legend_opts = list(
      label = "", pos = "bottom",
      dir = "horizontal"
    ),
    series_var = "TRTVAR",
    series_labelvar = "TRTVAR",
    bar_pos = "dodged",
    griddisplay = "N",
    plot_title = NULL
  )

  legendgroups <- unique(bar_out[["data"]][["TRTVAR"]])

  expect_type(bar_out, "list")
  expect_true(is.ggplot(bar_out))
  expect_equal(legendgroups, unique(bar_out[["data"]][["TRTVAR"]]))
  purrr::walk(
    c("mapping", "labels"),
    \(x) expect_snapshot(bar_out[[x]])
  )
})

test_that("Test Case 2: bar_plot works with modified inputs", {
  bar_out <- bar_plot(
    datain = adsl_sum,
    flip_plot = "Y",
    series_opts = list(
      color = c("red", "gold", "cyan"),
      contrast = c("black", "grey", "pink")
    ),
    axis_opts = plot_axis_opts(),
    legend_opts = list(
      label = "", pos = "bottom",
      dir = "horizontal"
    ),
    series_var = "TRTVAR",
    series_labelvar = "TRTVAR",
    bar_pos = "stacked",
    griddisplay = "N",
    plot_title = NULL
  )

  legendgroups <- unique(bar_out[["data"]][["TRTVAR"]])

  expect_type(bar_out, "list")
  expect_equal(legendgroups, unique(bar_out[["data"]][["TRTVAR"]]))
  expect_true(nrow(bar_out$data) > 0)
  expect_true(length(bar_out) > 0)
})
