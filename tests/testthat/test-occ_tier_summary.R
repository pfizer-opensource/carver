data("adae")
data("adsl")
data("ae_pre_process")
adae1 <- adae |>
  mutate(ASEVN = recode(.data[["AESEV"]], "MILD" = 1, "MODERATE" = 2, "SEVERE" = 3))
ae_pre <- ae_pre_processor(
  adae1,
  subset = "TRTEMFL == 'Y'",
  max_sevctc = "SEV",
  pt_total = "Y"
)
ae_entry_max <- ae_pre[["data"]] |>
  mentry(
    subset = NA,
    byvar = "AEBODSYS",
    trtvar = "TRTA",
    trtsort = "TRTAN",
    trttotalyn = "N",
    add_grpmiss = "N",
    subgrpvar = "AESEV",
    sgtotalyn = "N",
    pop_fil = "Overall Population"
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

test_that("occ_tier standard inputs works", {
  output <- occ_tier_summary(
    ae_entry,
    a_subset = ae_pre_process[["a_subset"]],
    summary_by = "Patients",
    hterm = "AEBODSYS",
    lterm = "AEDECOD",
    pctdisp = "TRT",
    cutoff_where = "PCT > 10 & FREQ > 20",
    apply_hrow_cutoff = "N",
    sort_opt = "Ascending",
    sort_var = "Count"
  )
  expect_s3_class(output, "data.frame")
  expect_true(unique(trimws(output$SUBGRPVARX)) == "n (%)")
  expect_snapshot(print(output, n = Inf, width = Inf))
})

test_that("occ_tier modified inputs works", {
  output <- occ_tier_summary(
    ae_entry,
    a_subset = ae_pre_process[["a_subset"]],
    summary_by = "Patients",
    hterm = "AEBODSYS",
    lterm = "AEDECOD",
    pctdisp = "TRT",
    cutoff_where = "PCT > 10 & FREQ > 20",
    apply_hrow_cutoff = "Y",
    nolwrtierdispyn = "Y",
    htermctyn = "N",
    sort_opt = "Alphabetical",
    sum_row = "Y",
    sum_row_label = "Any Adverse Event"
  )
  expect_s3_class(output, "data.frame")
  expect_true(unique(trimws(output$SUBGRPVARX)) == "n (%)")
  expect_snapshot(print(output, n = Inf, width = Inf))
})

test_that("Empty Outputs", {
  output <- occ_tier_summary(
    data.frame(),
    a_subset = ae_pre_process[["a_subset"]],
    summary_by = "Patients",
    hterm = "AEBODSYS",
    lterm = "AEDECOD",
    pctdisp = "TRT",
    apply_hrow_cutoff = "Y",
    nolwrtierdispyn = "N",
    htermctyn = "N",
    sort_opt = "Alphabetical"
  )
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 0)
  output1 <- occ_tier_summary(
    ae_entry,
    a_subset = ae_pre_process[["a_subset"]],
    summary_by = "Patients",
    hterm = "AEBODSYS",
    lterm = "AEDECOD",
    pctdisp = "TRT",
    cutoff_where = "PCT > 50",
    apply_hrow_cutoff = "Y",
    htermctyn = "N",
    sort_opt = "Alphabetical"
  )
  expect_equal(nrow(output1), 0)
})

test_that("occ_tier standard inputs works max severity", {
  output <- occ_tier_summary(
    ae_entry_max,
    a_subset = ae_pre[["a_subset"]],
    summary_by = "Patients",
    hterm = "AEBODSYS",
    lterm = "AEDECOD",
    cutoff_where = "FREQ > 5",
    pctdisp = "TRT",
    sum_row = "Y",
    sum_row_label = "Any Adverse Event",
    nolwrtierdispyn = "N",
    sort_opt = "Alphabetical",
    stathead = "n"
  )
  expect_s3_class(output, "data.frame")
  expect_true(unique(trimws(output$SUBGRPVARX)) == "n")
  expect_snapshot(print(output, n = Inf, width = Inf))
})
