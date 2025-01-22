data(adae)
data(FMQ_Consolidated_List)

prep_ae <- adae |>
  ae_pre_processor(
    ae_filter = "ANY",
    subset = "AOCCPFL == 'Y'",
    obs_residual = 0,
    fmq_data = FMQ_Consolidated_List
  )
prep_entry <- prep_ae$data |>
    mentry(
      trtvar = "TRTA",
      trtsort = "TRTAN",
      trttotalyn = "N",
      byvar = "FMQ_NAM"
    )
prep_event_analysis <- prep_entry |>
  process_event_analysis(
    a_subset = prep_ae$a_subset,
    summary_by = "Events",
    hterm = "FMQ_NAM",
    ht_val = "ABDOMINAL PAIN",
    ht_scope = "Narrow",
    lterm = "AEDECOD",
    lt_val = "ABDOMINAL DISCOMFORT",
    lt_scope = "Narrow"
  )

test_that("Test Case 1: process_event_analysis works with expected inputs", {
  expect_named(prep_event_analysis, c("query_df", "pt_df"))
  expect_equal(unique(prep_event_analysis$pt_df$DPTVAL), "ABDOMINAL DISCOMFORT")
  expect_equal(unique(prep_event_analysis$pt_df$DPTVAR), "AEDECOD")
  expect_equal(
    unique(prep_event_analysis$query_df$DPTVAL),
    c("ABDOMINAL PAIN", "ABDOMINAL DISCOMFORT", "STOMACH DISCOMFORT")
  )
  expect_equal(unique(prep_event_analysis$query_df$DPTVAR), "AEDECOD")
  purrr::walk(
    prep_event_analysis,
    \(x) expect_snapshot(print(tibble::as_tibble(x), n = Inf, width = Inf))
  )
})

test_that("Test Case 2: event_analysis_plot works with expected inputs", {
  plot <- event_analysis_plot(
    datain = prep_event_analysis,
    ref_line = 1,
    x_tickangle = 15,
    disp.proportion = "4~6",
    pt_color = "royalblue3",
    interactive = "Y"
  )

  expect_snapshot(plot$x$data)
})
