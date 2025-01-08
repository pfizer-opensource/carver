data("survival")
sh_pre <- surv_pre_processor(
  dataset_adsl = survival$adsl,
  adsl_subset = "RANDFL=='Y'",
  dataset_analysis = survival$adtte,
  split_by = "SEX",
  analysis_subset = "PARAMCD=='OS' & FASFL=='Y'",
  trtsort = "TRT01PN",
  censor_var = "CNSR",
  censor_val = 1,
  trtvar = "TRT01P",
  time_var = "AVAL"
)

data_list <- split_data_by_var(
  datain = sh_pre,
  split_by_prefix = "SUBGRPVAR"
)

test_that("surv_pre_processor works as expected", {
  actual_trt <- unique(sh_pre$TRTVAR)
  actual_subgrp <- unique(data_list[[1]]$SUBGRPVAR1)

  expect_equal(actual_subgrp, unique(survival$adsl$SEX))
  expect_equal(levels(actual_trt), c("Drug1", "Drug2", "Drug3"))
  expect_equal(levels(actual_trt), sort(as.character(actual_trt)))
  expect_equal(nrow(sh_pre), 120)
  expect_equal(unique(sh_pre$TRTVAR), actual_trt)
  expect_error(
    surv_pre_processor(
      dataset_adsl = survival$adsl,
      adsl_subset = "RANDFL=='Y'",
      dataset_analysis = survival$adtte,
      split_by = NA_character_,
      analysis_subset = "PARAMCD=='OS' & FASFL=='Y'",
      trtsort = "TRT01PN",
      censor_var = "xxxx",
      censor_val = 1,
      trtvar = "TRT01P",
      time_var = "AVAL"
    ),
    "Please provide a valid Censoring variable"
  )

  expect_error(
    surv_pre_processor(
      dataset_adsl = survival$adsl,
      adsl_subset = "RANDFL=='Y'",
      dataset_analysis = survival$adtte,
      split_by = NA_character_,
      analysis_subset = "PARAMCD=='OS' & FASFL=='Y'",
      trtsort = "TRT01PN",
      censor_var = "CNSR",
      censor_val = 1,
      trtvar = "TRT01P",
      time_var = "xxxx"
    ),
    "Invalid Duration variable"
  )
})

test_that("pairwise_surv_stats returns survival statistics as text output", {
  actual <- sh_pre |>
    pairwise_surv_stats()
  expected <-
    "HR (Drug1 vs Drug2) = 1.60, 95% CI (0.127, 20.180), 2-sided p = 0.7152, 1-sided p = 0.6424\nHR (Drug1 vs Drug3) = 1.21, 95% CI (0.342, 4.297), 2-sided p = 0.7654, 1-sided p = 0.6173\nHR (Drug2 vs Drug3) = 0.94, 95% CI (0.098, 9.048), 2-sided p = 0.9579, 1-sided p = 0.4790" # nolint
  expect_equal(actual, expected)
})
