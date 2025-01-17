ae_pre_process <- ae_pre_processor(
  datain = adae,
  ae_filter = "Any Event",
  obs_residual = 0,
  fmq_data = FMQ_Consolidated_List
)
ae_entry <- ae_pre_process[["data"]] |>
  mentry(
    subset = NA,
    byvar = "AEBODSYS",
    trtvar = "TRTA",
    trtsort = "TRTAN",
    trttotalyn = "N",
    add_grpmiss = "N",
    sgtotalyn = "N",
    pop_fil = "Overall Population"
  )

test_that("Test Case 1: adae_summary with standard inputs works", {
  ae_risk <- ae_entry |>
    adae_risk_summary(
      a_subset = ae_pre_process$a_subset,
      summary_by = "Patients",
      hterm = "AEBODSYS",
      lterm = "AEDECOD",
      ctrlgrp = "Placebo",
      trtgrp = "Xanomeline Low Dose",
      statistics = "Risk Difference",
      alpha = 0.05,
      cutoff_where = "PCT > 5",
      sort_opt = "Ascending",
      sort_var = "Count",
      risklabels = tbl_risk_labels("Risk Difference")
    )
  expect_equal(unique(pull(ae_risk, "TRTPAIR")), "Placebo -vs- Xanomeline Low Dose")
  expect_snapshot(ae_risk)
  output <- ae_risk |>
    tbl_processor(keepvars = "Risk Ratio (CI)") |>
    tbl_display()
  expect_snapshot(output)
})

test_that("Test Case 2: adae_summary with summary row", {
  ae_risk <- ae_entry |>
    adae_risk_summary(
      a_subset = ae_pre_process$a_subset,
      summary_by = "Patients",
      hterm = "AEBODSYS",
      lterm = "AEDECOD",
      ctrlgrp = "Placebo",
      trtgrp = "Xanomeline Low Dose",
      statistics = "Risk Ratio",
      alpha = 0.05,
      cutoff_where = "PCT > 5",
      sort_opt = "Alphabetical",
      sort_var = "Count",
      risklabels = tbl_risk_labels("Risk Ratio"),
      sum_row = "Y",
      sum_row_label = "Any AE"
    )
  expect_equal(unique(pull(ae_risk, "TRTPAIR")), "Placebo -vs- Xanomeline Low Dose")
  expect_snapshot(ae_risk)
  output <- ae_risk |>
    tbl_processor(keepvars = "Risk Ratio (CI)") |>
    tbl_display()
  expect_snapshot(output)
})

test_that("Test Case 3: Check empty and errors", {
  ae_risk <- data.frame() |>
    adae_risk_summary(
      a_subset = NA,
      summary_by = "Patients",
      hterm = "AEBODSYS",
      lterm = "AEDECOD",
      ctrlgrp = "Placebo",
      trtgrp = "Xanomeline Low Dose",
      statistics = "Risk Difference",
      alpha = 0.05,
      cutoff_where = "PCT > 5",
      sort_opt = "Alphabetical",
      sort_var = "Count",
      sum_row = "Y",
      sum_row_label = "Any AE"
    )
  expect_equal(nrow(ae_risk), 0)
  ae_risk1 <- ae_entry |>
    adae_risk_summary(
      a_subset = ae_pre_process$a_subset,
      summary_by = "Patients",
      hterm = "AEBODSYS",
      lterm = "AEDECOD",
      ctrlgrp = "Placebo",
      trtgrp = "Xanomeline Low Dose",
      statistics = "Risk Difference",
      alpha = 0.05,
      cutoff_where = "PCT > 50",
      sort_opt = "Alphabetical",
      sort_var = "Count",
      sum_row = "Y",
      sum_row_label = "Any AE"
    )
  expect_equal(nrow(ae_risk1), 0)
})
