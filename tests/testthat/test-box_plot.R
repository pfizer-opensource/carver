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
  statvar = c(
    "mean", "median", "q25", "q75", "whiskerlow",
    "whiskerup", "outliers"
  )
)

adsl_sum$gsum$XVAR <- fct_reorder(adsl_sum$gsum$BYVAR1, adsl_sum$gsum$BYVAR1N)
fig_col <- box_plot(
  datain = adsl_sum$gsum,
  axis_opts = plot_axis_opts(xaxis_label = "Race", yaxis_label = "Age"),
  legend_opts = list(
    label = "Treatment", pos = "bottom",
    dir = "horizontal"
  ),
  series_opts = list(
    color = c("red", "gold", "cyan"), shape = c(16, 17, 15),
    size = c(1, 1, 1)
  )
)
fig_fill <- box_plot(
  datain = adsl_sum$gsum,
  axis_opts = plot_axis_opts(xaxis_label = "Race", yaxis_label = "Age"),
  legend_opts = list(
    label = "Treatment", pos = "bottom",
    dir = "horizontal"
  ),
  series_opts = list(
    color = c("red", "gold", "cyan"), shape = c(16, 17, 15),
    size = c(1, 1, 1)
  ),
  boxfill = "Y",
  griddisplay = "Y"
)

# Whiskers varied to min and max
adsl_sum2 <- msumstat(
  datain = adsl_entry,
  dptvar = "AGE",
  statvar = c(
    "mean", "median", "q25", "q75", "min",
    "max"
  )
)
adsl_sum2$gsum$XVAR <- fct_reorder(adsl_sum2$gsum$BYVAR1, adsl_sum2$gsum$BYVAR1N)
fig_fill2 <- box_plot(
  datain = adsl_sum2$gsum,
  axis_opts = plot_axis_opts(xaxis_label = "Race", yaxis_label = "Age"),
  legend_opts = list(
    label = "Treatment", pos = "bottom",
    dir = "horizontal"
  ),
  series_opts = list(
    color = c("red", "gold", "cyan"), shape = c(16, 17, 15),
    size = c(1, 1, 1)
  ),
  boxfill = "Y"
)

# Tests:
test_that("Standard box plot outputs", {
  expect_true(is.ggplot(fig_col))
  expectdata <- adsl_sum$gsum |>
    mutate(across(all_of(c(
      "mean", "median", "q25", "q75", "whiskerlow",
      "whiskerup"
    )), as.numeric))
  expect_equal(fig_col$data, expectdata)
  purrr::walk(
    list(fig_col, fig_fill, fig_fill2),
    \(p) purrr::walk(c("mapping", "labels"), \(x) expect_snapshot(p[[x]]))
  )
})

test_that("Errors resolved", {
  expect_error(
    box_plot(
      datain = adsl_sum$gsum |> select(-all_of(c("median", "q25"))),
      axis_opts = plot_axis_opts(xaxis_label = "Race", yaxis_label = "Age"),
      legend_opts = list(
        label = "Treatment", pos = "bottom",
        dir = "horizontal"
      ),
      series_opts = list(
        color = c("red", "gold", "cyan"), shape = c(16, 17, 15),
        size = c(1, 1, 1)
      )
    ), "Expected statistics not found"
  )
  expect_error(
    box_plot(
      datain = data.frame(),
      axis_opts = plot_axis_opts(xaxis_label = "Race", yaxis_label = "Age"),
      legend_opts = list(
        label = "Treatment", pos = "bottom",
        dir = "horizontal"
      ),
      series_opts = list(
        color = c("red", "gold", "cyan"), shape = c(16, 17, 15),
        size = c(1, 1, 1)
      )
    )
  )
})
