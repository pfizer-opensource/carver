data(adsl)

mentry_df <- adsl |>
  mentry(
    subset = NA_character_,
    byvar = "SEX",
    trtvar = "TRT01A",
    trtsort = "TRT01AN",
    subgrpvar = NA_character_,
    trttotalyn = "N",
    add_grpmiss = "N",
    sgtotalyn = "N",
    pop_fil = "SAFFL"
  )

test_that("split_var_types works", {
  actual <- split_var_types(c("AGE-S", "RACE", "SEX"))
  expected <-
    list(num_vars = "AGE", cat_vars = c("RACE", "SEX"), all_vars = c("AGE", "RACE", "SEX"))
  
  actual_ <- split_var_types(c("AGE", "RACE", "SEX"))
  expected_ <-
    list(
      num_vars = character(0),
      cat_vars = c("AGE", "RACE", "SEX"),
      all_vars = c("AGE", "RACE", "SEX")
    )
  
  expect_identical(actual, expected)
  expect_identical(actual_, expected_)
})

test_that("adsl_summary works as expected", {
  adsl_sum <- adsl_summary(
    datain = mentry_df,
    vars = "AGEGR1~AGE-S~RACE"
  )
  
  dataf <- adsl_sum |>
    display_bign_head(mentry_df) |>
    tbl_processor(
      dptlabel = str_to_vec("Age Group~Age~Race"),
      statlabel = str_to_vec("N~Range~Mean (SD)~Median~Interquartile Range")
    )
  
  adsl_sum_ <- adsl_summary(
    datain = mentry_df,
    vars = "AGEGR1~AGE~RACE"
  )
  
  dataf_ <- adsl_sum_ |>
    display_bign_head(mentry_df) |>
    tbl_processor(
      statlabel = str_to_vec("N~Range~Meansd~Median~IQR"),
      dptlabel = str_to_vec("Age Group~Age~Race")
    )
  
  expect_false(identical(dataf, dataf_))
  expect_true(nrow(dataf) < nrow(dataf_))
  expect_snapshot(print(tibble::as_tibble(dataf), n = Inf, width = Inf))
  expect_snapshot(print(tibble::as_tibble(dataf_), n = Inf, width = Inf))
})

test_that("adsl_summary gives returns correct summary statistics", {
  adsl_sum <- adsl_summary(
    datain = mentry_df,
    vars = "AGEGR1~AGE-S~RACE"
  )
  
  actual <- adsl_sum |>
    display_bign_head(mentry_df) |>
    tbl_processor(
      statlabel = str_to_vec("N~Range~Meansd~Median~IQR"),
      dptlabel = str_to_vec("Age Group~Age~Race")
    )
  
  agegr1 <- adsl |>
    dplyr::filter(SAFFL == "Y") |>
    dplyr::select(dplyr::all_of(c("TRT01A", "SEX", "AGEGR1"))) |>
    dplyr::group_by(dplyr::across(dplyr::everything())) |>
    dplyr::summarise(N = n()) |>
    dplyr::filter(TRT01A == "Placebo") |>
    dplyr::pull(N)
  
  exp_agegr1 <- actual |>
    dplyr::filter(DPTVAR == "Age Group") |>
    dplyr::arrange(BYVAR1, DPTVAL) |>
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("Placebo"),
      \(x) as.integer(stringr::str_squish(stringr::str_sub(x, 1, 2)))
    )) |>
    dplyr::pull(dplyr::starts_with("Placebo"))
  
  age_stat <- adsl |>
    dplyr::filter(SAFFL == "Y") |>
    dplyr::select(dplyr::all_of(c("TRT01A", "SEX", "AGE"))) |>
    dplyr::group_by(dplyr::across(c("TRT01A", "SEX"))) |>
    dplyr::summarize(
      Mean = paste0(round_f(mean(.data[["AGE"]]), 2), " (", round_f(sd(.data[["AGE"]]), 2), ")")
    ) |>
    dplyr::arrange(.data[["SEX"]]) |>
    dplyr::pull(Mean)
  
  exp_age_stat <- actual |>
    dplyr::filter(DPTVAL == "Meansd") |>
    dplyr::relocate(`Xanomeline Low Dose (N=84)`, .after = `Xanomeline High Dose (N=84)`)
  
  expect_identical(unique(actual[["DPTVAR"]]), c("Age Group", "Age", "Race"))
  expect_identical(agegr1, exp_agegr1)
  expect_identical(age_stat, unname(unlist(c(exp_age_stat[1, 7:9], exp_age_stat[2, 7:9]))))
})

test_that("adsl_summary works with subsets", {
  adsl_sum <- mentry_df |>
    adsl_summary(
      vars = "AGEGR1~AGE-S~SEX~RACE",
      a_subset = "AGE<65~AGE>80~NA~NA"
    )
  
  actual <- adsl_sum |>
    display_bign_head(mentry_df) |>
    tbl_processor(
      statlabel = str_to_vec("N~Range~Meansd~Median~IQR"),
      dptlabel = str_to_vec("Age Group~Age~Sex~Race")
    )
  
  adsl_sum_ <- mentry_df |>
    adsl_summary(
      vars = "AGEGR1~AGE-S~SEX~RACE",
      denom_subset = "RACE=='WHITE'~NA~NA~NA"
    )
  
  actual_ <- adsl_sum_ |>
    display_bign_head(mentry_df) |>
    tbl_processor(
      statlabel = str_to_vec("N~Range~Meansd~Median~IQR"),
      dptlabel = str_to_vec("Age Group~Age~Sex~Race")
    )
  
  expect_snapshot(print(actual, n = Inf, width = Inf))
  expect_snapshot(print(tibble::as_tibble(actual_), n = Inf, width = Inf))
  
  expect_error(
    adsl_summary(
      datain = mentry_df,
      vars = "AGEGR1~AGE-S~SEX~RACE",
      a_subset = "AGE<65~AGE>80~NA"
    ),
    "Number of subsets should be 1 or equal to number of corresponding variables"
  )
  expect_error(
    adsl_summary(
      datain = mentry_df,
      vars = "AGEGR1~AGE-S~SEX~RACE",
      denom_subset = "AGE<65~NA"
    ),
    "Number of subsets should be 1 or equal to number of corresponding variables"
  )
})

test_that("Analysis variables not present", {
  adsl_sum1 <- mentry_df |>
    adsl_summary(
      vars = "AGEGR1/AGEGR1N~AGE-S~SEX/SEXN~RACE/RACEN",
      a_subset = "AGE<65~AGE>80~NA~NA"
    )
  adsl_sum2 <- mentry_df |>
    adsl_summary(
      vars = "AGEGR1/AGEGR1N~AGE-S~SEX/SEXN~RACE/RACEN~AGEGR5/AGEGR5N",
      a_subset = "AGE<65~AGE>80~NA~NA~NA"
    )
  expect_equal(adsl_sum1, adsl_sum2)
})
