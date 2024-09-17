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

test_that("Standard inputs for occ_tier works", {
  output <- occ_tier_summary(
    ae_entry,
    a_subset = ae_pre_process[["a_subset"]],
    summary_by = "Patients",
    hterm = "AEBODSYS",
    lterm = "AEDECOD",
    pctdisp = "TRT",
    cutoff = 5,
    apply_hrow_cutoff = "N",
    sort_opt = "Ascending",
    sort_var = "Count"
  )
  expect_s3_class(output, "data.frame")
  # Check treatments are accounted:
  all(unique(output$TRTVAR) %in% unique(ae_entry$TRTVAR))
  expect_snapshot(output)
})

test_that("Standard inputs for occ_tier works", {
  output <- occ_tier_summary(
    ae_entry,
    a_subset = ae_pre_process[["a_subset"]],
    summary_by = "Patients",
    hterm = "AEBODSYS",
    lterm = "AEDECOD",
    pctdisp = "TRT",
    cutoff = 5,
    apply_hrow_cutoff = "N",
    sort_opt = "Ascending",
    sort_var = "Count"
  )
  expect_s3_class(output, "data.frame")
  # Check treatments are accounted:
  all(unique(output$TRTVAR) %in% unique(ae_entry$TRTVAR))
  expect_snapshot(output)
})

test_that("Cut off applied for occ_tier works", {
  output <- occ_tier_summary(
    ae_entry,
    a_subset = ae_pre_process[["a_subset"]],
    summary_by = "Patients",
    hterm = "AEBODSYS",
    lterm = "AEDECOD",
    pctdisp = "TRT",
    cutoff = 5,
    apply_hrow_cutoff = "Y",
    sort_opt = "Alphabetical",
    sort_var = "Count"
  )
  expect_s3_class(output, "data.frame")
  # Check treatments are accounted:
  all(unique(output$TRTVAR) %in% unique(ae_entry$TRTVAR))
  expect_snapshot(output)
})

test_that("Cut off too high", {
  output <- occ_tier_summary(
    ae_entry,
    a_subset = ae_pre_process[["a_subset"]],
    summary_by = "Patients",
    hterm = "AEBODSYS",
    lterm = "AEDECOD",
    pctdisp = "TRT",
    cutoff = 40,
    apply_hrow_cutoff = "Y",
    sort_opt = "Alphabetical",
    sort_var = "Count"
  )
  expect_s3_class(output, "data.frame")
  expect_equal(output, data.frame("Note" = "No low term data available under these conditions"))
})

test_that("Errors resolved correctly", {
  expect_error(
    occ_tier_summary(
      data.frame(),
      a_subset = ae_pre_process[["a_subset"]],
      summary_by = "Patients",
      hterm = "AEBODSYS",
      lterm = "AEDECOD",
      pctdisp = "TRT",
      cutoff = 0,
      apply_hrow_cutoff = "Y",
      sort_opt = "Alphabetical",
      sort_var = "Count"
    ), "Input data is empty"
  )
  expect_error(
    occ_tier_summary(
      ae_entry,
      a_subset = ae_pre_process[["a_subset"]],
      summary_by = "Patients",
      hterm = "RACE",
      lterm = "AEDECOD",
      pctdisp = "TRT",
      cutoff = 0,
      apply_hrow_cutoff = "Y",
      sort_opt = "Alphabetical",
      sort_var = "Count"
    ))
})
