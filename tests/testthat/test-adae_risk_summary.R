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

test_that("Standard Inputs work", {
  output <- ae_entry |>
    adae_risk_summary(
      a_subset = ae_pre_process[["a_subset"]],
      summary_by = "Patients",
      hterm = "AEBODSYS",
      lterm = "AEDECOD",
      ctrlgrp = "Placebo",
      trtgrp = "Xanomeline Low Dose",
      statistics = "Risk Ratio",
      alpha = 0.05,
      cutoff = 5,
      sort_opt = "Ascending",
      sort_var = "Count"
    )
  expect_s3_class(output, "data.frame")
  # Check risk calculated as expected;
  expect_true("Risk Ratio (CI)" %in% names(output))
  # CHeck only pair of treatments present in output:
  expect_length(unique(output$TRTPAIR), 1)
  expect_snapshot(output)
  out_table <- output |>
    tbl_processor(keepvars = c("Risk Ratio (CI)", "P-value"))
  expect_snapshot(out_table)
})

test_that("High cutoff as expected", {
  expect_message(
    adae_risk_summary(
      ae_entry,
      a_subset = ae_pre_process[["a_subset"]],
      summary_by = "Patients",
      hterm = "AEBODSYS",
      lterm = "AEDECOD",
      ctrlgrp = "Placebo",
      trtgrp = "Xanomeline Low Dose",
      statistics = "Risk Ratio",
      alpha = 0.05,
      cutoff = 20,
      sort_opt = "Ascending",
      sort_var = "Count"
    ))
})

test_that("Errors Resolved correctly", {
  expect_error(
    adae_risk_summary(
      data.frame(),
      a_subset = ae_pre_process[["a_subset"]],
      summary_by = "Patients",
      hterm = "AEBODSYS",
      lterm = "AEDECOD",
      ctrlgrp = "Placebo",
      trtgrp = "Xanomeline Low Dose",
      statistics = "Risk Ratio",
      alpha = 0.05,
      cutoff = 5,
      sort_opt = "Ascending",
      sort_var = "Count"
    ),
    "Input data is empty"
  )
  expect_error(
    adae_risk_summary(
      ae_entry,
      a_subset = ae_pre_process[["a_subset"]],
      summary_by = "Patients",
      hterm = "AEBODSYS",
      lterm = "AEDECOD",
      ctrlgrp = "Placebo",
      trtgrp = "Xanomeline Low Dose",
      statistics = "Relative Risk",
      alpha = 0.05,
      cutoff = 5,
      sort_opt = "Ascending",
      sort_var = "Count"
    ),
    "Invalid Risk Statistics; specify any one of `Risk Ratio` or `Risk Difference`"
  )
})