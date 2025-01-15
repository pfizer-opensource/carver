library(carver)
library(dplyr)
data(adsl)

mentry_df <- adlb |>
  mentry(
    subset = "PARAMCD %in% c('ALT', 'BILI') & !is.na(ANRHI)",
    byvar = NA_character_,
    trtvar = "TRTA",
    trtsort = "TRTAN",
    subgrpvar = NA_character_,
    trttotalyn = "N",
    add_grpmiss = "N",
    pop_fil = "SAFFL"
  ) |>
  group_by(.data[["USUBJID"]], .data[["TRTVAR"]], .data[["PARAMCD"]]) |>
  summarise(AVAL_N = max(.data[["AVAL"]])) |>
  tidyr::pivot_wider(id_cols = c("USUBJID", "TRTVAR"), names_from = "PARAMCD", values_from = "AVAL_N") |>
  mutate(XVAR = .data[["ALT"]], YVAR = .data[["BILI"]])
fig <- mentry_df |>
  scatter_plot(
    axis_opts = plot_axis_opts(),
    series_var = "TRTVAR",
    series_labelvar = "TRTVAR",
    series_opts = list(
      shape = c(16, 17, 18),
      color = scales::hue_pal()(3),
      size = c(2, 2, 3)
    ),
    legend_opts = list(
      label = "Treatment",
      pos = "bottom",
      dir = "horizontal"
    ),
    plot_title = "Scatter Plot of maximum ALT vs BILI"
  )

test_that("scatter_plot works as expected", {
  expect_snapshot(print(tibble::as_tibble(fig[["data"]]), n = Inf))
  purrr::walk(c("mapping", "theme", "labels"), \(y) expect_snapshot(fig[[y]]))
})
