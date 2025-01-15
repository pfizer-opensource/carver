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
  statvar = "mean"
)
adsl_sum$gsum <- adsl_sum$gsum |>
  mutate(
    XVAR = fct_reorder(.data[["BYVAR1"]], .data[["BYVAR1N"]]),
    YVAR = as.numeric(.data[["mean"]])
  )
fig <- line_plot(
  datain = adsl_sum$gsum,
  axis_opts = plot_axis_opts(xaxis_label = "Race", yaxis_label = "Mean Age"),
  legend_opts = list(label = "Treatment", pos = "bottom",
                     dir = "horizontal"),
  series_opts = plot_aes_opts(
    adsl_sum$gsum,
    "TRTVAR",
    series_color = "firebrick~forestgreen~dodgerblue",
    series_shape = "triangle~square~circle"
  ),
  griddisplay = "Y"
)

test_that("Standard line plot outputs", {
  expect_true(is.ggplot(fig))
  expect_equal(fig$data, adsl_sum$gsum)
  purrr::walk(c("mapping", "labels"), \(x) expect_snapshot(fig[[x]]))
})

test_that("Expect errors", {
  expect_error(
    line_plot(
      datain = adsl_sum$gsum |> select(-all_of("XVAR")),
      axis_opts = plot_axis_opts(xaxis_label = "Race", yaxis_label = "Mean Age"),
      legend_opts = list(
        label = "Treatment", pos = "bottom",
        dir = "horizontal"
      ),
      series_opts = list(
        color = c("red", "gold", "cyan")
      ),
      griddisplay = "Y"
    ),
    "XVAR, YVAR, series_var and series_labelvar should exist in data"
  )
})
