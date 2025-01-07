data("ae_pre_process")
ae_entry <- mentry(
  datain = ae_pre_process$data,
  subset = NA,
  byvar = "AEBODSYS",
  trtvar = "TRTA",
  trtsort = "TRTAN",
  subgrpvar = NA,
  trttotalyn = "N",
  add_grpmiss = "N",
  sgtotalyn = "N",
  pop_fil = "SAFFL"
)
dsin1 <- ae_entry |>
  filter(AEDECOD %in% c("NAUSEA", "SINUS BRADYCARDIA"))

denom <- dsin1 |>
  filter(TRTVAR %in% c("Placebo", "Xanomeline High Dose")) |>
  group_by(TRTVAR) |>
  summarise(N = length(unique(USUBJID))) |>
  ungroup()

freq <- dsin1 |>
  filter(TRTVAR %in% c("Placebo", "Xanomeline High Dose") &
           eval(parse(text = ae_pre_process$a_subset))) |>
  group_by(TRTVAR, AEBODSYS, AEDECOD) |>
  summarise(n = length(unique(USUBJID))) |>
  ungroup()

exp <- left_join(denom, freq, by = "TRTVAR")

mat <- matrix(c(2, 7, 3, 6), nrow = 2)

# testcase 1

test_that("Test Case 1: Check if the function gives expected statistic values", {
  risk <- suppressWarnings(epitools::riskratio.wald(mat, conf.level = 1 - 0.05, correction = TRUE))
  
  risk_val <- round(risk$measure[2, 1], 3)
  pval <- round(risk$p.value[2, 3], 4)
  cil <- round(risk$measure[2, 2], 2)
  ciu <- round(risk$measure[2, 3], 2)
  
  expected <- exp |>
    filter(AEDECOD == "NAUSEA") |>
    mutate(
      RISK = risk_val,
      PVALUE = pval,
      RISKCIL = cil,
      RISKCIU = ciu,
      PCT = (n * 100) / N
    )
  
  risk_s <- risk_stat(
    datain = dsin1,
    a_subset = ae_pre_process$a_subset,
    summary_by = "Patients",
    eventvar = "AEDECOD",
    ctrlgrp = "Placebo",
    trtgrp = "Xanomeline High Dose",
    statistics = "Risk Ratio",
    alpha = 0.05,
    cutoff_where = "PCT > 2",
    sort_opt = "Ascending",
    sort_var = "Count"
  )
  
  actual <- risk_s |>
    rename(AEBODSYS = BYVAR1, AEDECOD = DPTVAL, N = TOTAL_N, n = FREQ) |>
    filter(AEDECOD == "NAUSEA") |>
    mutate(
      N = as.integer(N),
      n = as.integer(n)
    ) |>
    select(TRTVAR, N, AEBODSYS, AEDECOD, n, RISK, PVALUE, RISKCIL, RISKCIU, PCT)
  
  expect_equal(actual$RISK, expected$RISK)
  expect_equal(actual$PVALUE, expected$PVALUE)
  expect_equal(actual, expected, ignore_attr = TRUE)
})

# test case 2

test_that("Test Case 2: Check if the function works as expected for risk difference", {
  risk <- suppressWarnings(riskdiff_wald(mat, conf.level = 1 - 0.05))
  
  risk_val <- round(risk$measure[2, 1], 3)
  pval <- round(risk$p.value[2, 3], 4)
  ciu <- round(risk$measure[2, 2], 4)
  cil <- round(risk$measure[2, 3], 4)
  
  expected <- exp |>
    filter(AEDECOD == "NAUSEA") |>
    mutate(
      RISK = risk_val,
      PVALUE = pval
    ) |>
    arrange(AEDECOD)
  
  risk_s <- risk_stat(
    datain = dsin1,
    a_subset = ae_pre_process$a_subset,
    summary_by = "Patients",
    eventvar = "AEDECOD",
    ctrlgrp = "Placebo",
    trtgrp = "Xanomeline High Dose",
    statistics = "Risk Difference",
    alpha = 0.05,
    sort_opt = "Alphabetical"
  )
  
  actual <- risk_s |>
    rename(AEBODSYS = BYVAR1, AEDECOD = DPTVAL, N = TOTAL_N, n = FREQ) |>
    filter(AEDECOD == "NAUSEA") |>
    mutate(
      N = as.integer(N),
      n = as.integer(n)
    ) |>
    select(TRTVAR, N, AEBODSYS, AEDECOD, n, RISK, PVALUE)
  
  expected$RISK <- -1 * (expected$RISK)
  expect_equal(actual$RISK, expected$RISK)
  expect_equal(actual$PVALUE, expected$PVALUE)
  expect_equal(actual, expected, ignore_attr = TRUE)
})

# test case 3

test_that("riskdiff_wald: check if the function works as expected", {
  evts <- 4
  non_evts <- 6
  control_evts <- 3
  cne <- 8
  
  expected_output <- (control_evts / (control_evts + cne)) - (evts / (evts + non_evts))
  actual <- suppressWarnings(riskdiff_wald(matrix(c(evts, control_evts, non_evts, cne), nrow = 2)))
  actual_output <- actual$measure[2, 1]
  
  expect_equal(actual_output, expected_output, ignore_attr = TRUE)
})

# test case 4

test_that("risk_stat: returns empty data frame when cutoff is too high", {
  actual <- risk_stat(
    datain = ae_entry,
    a_subset = ae_pre_process$a_subset,
    summary_by = "Patients",
    eventvar = "AEDECOD",
    ctrlgrp = "Placebo",
    trtgrp = "Xanomeline High Dose",
    statistics = "Risk Ratio",
    alpha = 0.05,
    cutoff_where = "FREQ > 500",
    sort_opt = "Ascending",
    sort_var = "Count"
  )
  
  expected <- data.frame(NULL)
  expect_identical(actual, expected)
})
