data("adsl")
# ggplot --> ggplotly object as input
stat_fig <- adsl |>
  slice(1:10) |>
  ggplot(aes(x = .data[["AGE"]], y = .data[["BMIBL"]], color = .data[["AGEGR1"]])) +
  geom_point() +
  theme(legend.position = "none")
test_fig <- stat_fig |>
  plotly::ggplotly()

test_that("Test Case 1: plotly_legend works", {
  fig1 <- plotly_legend(
    fig = test_fig,
    lg_pos = "bottom",
    lg_lab = "Age Group",
    dir = "h"
  )
  expect_type(fig1, "list")
  expect_true("plotly" %in% class(fig1))
  expect_true(length(fig1$x) > length(test_fig$x))
  expect_snapshot(fig1$x$layoutAttrs[[1]])
  expect_snapshot(fig1$x$layoutAttrs[[2]])
  fig2 <- plotly_legend(
    fig = test_fig,
    lg_pos = c(0.2, 0.1),
    lg_lab = "",
    dir = "vertical"
  )
  expect_equal(length(fig2$x$layoutAttrs), 1)
  expect_snapshot(fig2$x$layoutAttrs[[1]])
})

test_that("Test Case 2: plotly_legend resolves errors", {
  expect_error(
    plotly_legend(test_fig, lg_pos = "left", dir = "vert"),
    "dir can only be 'h' or 'v'"
  )
  expect_error(plotly_legend(stat_fig, "right"))
  expect_error(
    plotly_legend(test_fig, lg_pos = "upper"), "lg_pos must be top, bottom, left, right or vector"
  )
})

test_that("Test Case 3: as_plotly works as expected", {
  ptly_out <- as_plotly(
    plot = stat_fig,
    height = 500,
    width = 400,
    hover = "color"
  )
  expect_type(ptly_out, "list")
  expect_true("plotly" %in% class(ptly_out))
  expect_equal(ptly_out$height, 500)
  expect_equal(ptly_out$width, 400)
  expect_equal(ptly_out$x$source, "plot_output")
})
