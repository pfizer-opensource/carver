adsl_entry <- mentry(
  datain = adsl,
  byvar = "ETHNIC",
  trtvar = "TRT01A",
  trtsort = "TRT01AN",
  subgrpvar = NA_character_,
  trttotalyn = "Y",
  add_grpmiss = "N",
  sgtotalyn = "Y",
  pop_fil = "SAFFL"
)
adsl_sum <- adsl_summary(
  datain = adsl_entry,
  vars = "AGEGR1/AGEGR1N~AGE-S~SEX/SEXN~RACE/RACEN",
  stat_vars = "n~mean(sd)"
)
tbl_data <- adsl_sum |>
  display_bign_head(
    adsl_entry,
    trtbignyn = "Y",
    subbignyn = "Y"
  ) |>
  tbl_processor(
    dptlabel = "Age (Years), n (%)~NONE~Gender, n (%)~Race, n (%)",
    statlabel = "n~Mean (SD)",
    addrowvars = "DPTVAR"
  )
tbl <- tbl_data |>
  tbl_display(
    bylabel = "Ethnicity"
  )

# Data with no treatment/subgrp and no dptcolumn
adsl_entry1 <- mentry(
  datain = adsl,
  byvar = "ETHNIC",
  add_grpmiss = "N",
  sgtotalyn = "Y",
  pop_fil = "SAFFL"
)
adsl_cat <- adsl_entry1 |>
  mcatstat(dptvar = "SAFFL")
tbl_data1 <- adsl_cat |>
  display_bign_head(
    mentry_data = adsl_entry1,
    notrthead = "Participants, n (%)"
  ) |>
  tbl_processor(
    disp_value_col = "N",
    addrowvars = NA
  )
tbl1 <- tbl_data1 |>
  tbl_display(bylabel = "Ethnicity", boldheadyn = "Y")

test_that("tbl_processor works standard", {
  expect_s3_class(tbl_data, "data.frame")
  expect_snapshot(print(tbl_data, n = Inf, width = Inf))
  expect_true(class(tbl) == "flextable")
})

test_that("tbl_processor works without trt/dpt", {
  expect_s3_class(tbl_data1, "data.frame")
  expect_snapshot(print(tbl_data1, n = Inf, width = Inf))
  expect_true(class(tbl1) == "flextable")
})

test_that("tbl_processor works with keepvars", {
  testdata <- adsl_cat |>
    mutate(NewCol = "Keepthis")
  tbl_data2 <- testdata |>
    display_bign_head(
      mentry_data = adsl_entry1,
      notrthead = "Participants, n (%)"
    ) |>
    tbl_processor(
      disp_value_col = "N",
      addrowvars = NA,
      keepvars = "NewCol"
    )
  expect_s3_class(tbl_data2, "data.frame")
  expect_true("NewCol" %in% colnames(tbl_data2))
  expect_true(unique(tbl_data2$NewCol) == "Keepthis")
})

test_that("Empty_tbl works", {
  tbl_empty <- empty_tbl()
  expect_snapshot(tbl_empty)
})
