bar_pre <- process_vx_bar_plot(
  dataset_adsl = vx_bar_data$adsl,
  adsl_subset = "SAFFL=='Y'",
  dataset_analysis = vx_bar_data$adfacevd,
  analysis_subset = "ATPTN <= 14 & toupper(FAOBJ) == 'PAIN AT INJECTION SITE' &
 !(AVAL %in% c(0, 0.5)) & FATESTCD != 'OCCUR' & !is.na(AVAL)",
  denom_subset = "ATPTN <= 14 & toupper(FAOBJ) == 'PAIN AT INJECTION SITE' &
 !(AVAL %in% c(0, 0.5))",
  overall_subset = NA,
  split_by = "SEX",
  trtvar = "TRT01A",
  trtsort = "TRT01AN",
  xvar = "ATPTN",
  yvar = "PCT",
  pctdisp = "DPTVAR",
  legendbign = "Y"
)


test_that("process_vs_bar_data works as expected", {
  expect_s3_class(bar_pre, "data.frame")
  actual_trt <- unique(bar_pre$TRTVAR)
  actual_subgrp <- unique(bar_pre$SUBGRPVAR1)
  
  expect_equal(actual_subgrp, sort(unique(vx_bar_data$adsl$SEX)))
  expect_equal(levels(actual_trt), c("Drug1", "Drug2", "Drug3"))
  expect_snapshot(print(bar_pre, width = Inf, n = Inf))
})
