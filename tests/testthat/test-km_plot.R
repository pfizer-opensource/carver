data("survival")

km_df <- survival[["adsl"]] |>
  surv_pre_processor(
    dataset_analysis = survival[["adtte"]],
    analysis_subset = "PARAMCD == 'PFS_P'"
  )

test_that("km_plot works with default options", {
  p <- km_df |>
    km_plot(trt_colors = "#F8766D~#00BA38~#619CFF")
  surv_df <- purrr::list_modify(p$data, survfit = NULL)
  p$theme$legend.background <- NULL
  fit <- purrr::list_modify(p[["data"]][["survfit"]][[1]], call = NULL, .Environment = NULL)

  expect_snapshot(print(tibble::as_tibble(surv_df), n = Inf, width = Inf))
  expect_snapshot(p$theme)
  purrr::walk(p$labels, ~ expect_snapshot(.x))
  purrr::walk(fit, ~ expect_snapshot(.x))
  expect_true(p$mapping[[1]])
})

test_that("km_plot works with different options", {
  p <- km_df |>
    km_plot(
      disp_conf.int = "Y",
      risktab_stats = "n.risk~n.censor",
      risktab_height = 0.25,
      trt_colors = "#F8766D~#00BA38~#619CFF",
      axis_opts = plot_axis_opts(
        xlinearopts = list(breaks = 3),
        ylinearopts = list(breaks = 0.1),
        xaxis_label = "Progression-Free Survival Time (Months)",
        yaxis_label = "Probability of Progression Free Survival"
      )
    )
  surv_df <- purrr::list_modify(p$data, survfit = NULL)
  p$theme$legend.background <- NULL
  fit <- p[["data"]][["survfit"]][[1]]

  expect_snapshot(print(tibble::as_tibble(surv_df), n = Inf, width = Inf))
  expect_snapshot(p$theme)
  purrr::walk(p$labels, ~ expect_snapshot(.x))
  expect_snapshot(print(tibble::as_tibble(summary(fit)[["table"]]), n = Inf, width = Inf))
  expect_true(p$mapping[[1]])
})

test_that("km_plot returns empty plot when `datain` is empty", {
  actual <- km_df |>
    dplyr::filter(USUBJID == "xxxx") |>
    km_plot(
      disp_conf.int = "Y",
      risktab_stats = "n.risk~n.censor",
      risktab_height = 0.25,
      trt_colors = "#F8766D~#00BA38~#619CFF",
      axis_opts = plot_axis_opts(
        xlinearopts = list(breaks = 3),
        ylinearopts = list(breaks = 0.1),
        xaxis_label = "Progression-Free Survival Time (Months)",
        yaxis_label = "Probability of Progression Free Survival"
      )
    )

  expected <- empty_plot("No data available")[["plot"]]
  expect_identical(actual[["data"]], expected[["data"]])
  expect_identical(actual[["mapping"]], expected[["mapping"]])
  expect_identical(actual[["theme"]], expected[["theme"]])
})
