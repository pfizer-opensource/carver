data("adsl")
adsl_entry <-
  adsl |> mentry(
    subset = "EFFFL=='Y'",
    byvar = "AGEGR1",
    subgrpvar = "SEX",
    trtvar = "TRT01A",
    trtsort = "TRT01AN",
    trttotalyn = "N",
    add_grpmiss = "N",
    pop_fil = NA
  )
expected_g <- adsl_entry |>
  group_by(across(all_of(c("BYVAR1", "BYVAR1N", "TRTVAR", "SUBGRPVAR1", "SUBGRPVAR1N")))) |>
  summarise(mean = round_f(mean(.data[["AGE"]], na.rm = TRUE), 2)) |>
  ungroup() |>
  mutate(DPTVAR = "AGE", CN = "N", DPTVARN = 1) |>
  arrange(across(all_of(c("BYVAR1N", "TRTVAR", "SUBGRPVAR1N"))))
expected_t <- expected_g |>
  pivot_longer(cols = "mean", names_to = "DPTVAL", values_to = "CVALUE") |>
  ungroup() |>
  mutate(DPTVALN = 1) |>
  arrange(across(all_of(c("BYVAR1N", "TRTVAR", "SUBGRPVAR1N"))))

test_that("Test 1: 'Check for expected inputs", {
  # Logic check
  # for a valid input data, the number of rows returned is >0
  actual <- msumstat(
    datain = adsl_entry,
    dptvar = "AGE",
    statvar = "mean",
    sigdec = 2
  )
  # Test
  expect_type(actual, "list")
  expect_named(actual, c("tsum", "gsum"))
  expect_equal(
    actual$gsum |> arrange(across(all_of(c("BYVAR1N", "TRTVAR", "SUBGRPVAR1N")))),
    expected_g
  )
  expect_equal(
    actual$tsum |> arrange(across(all_of(c("BYVAR1N", "TRTVAR", "SUBGRPVAR1N")))),
    expected_t
  )
})


# Test 2
test_that("Test 2: Check modified inputs", {
  # LOGIC: Check if the valid  Dependent Variable Exists and Type Conversion is performed
  test_adsl <- adsl_entry |>
    mutate(BMIBLCH = as.character(.data[["BMIBL"]]))
  actual <- msumstat(
    datain = test_adsl,
    dptvar = "BMIBLCH",
    statvar = "",
    sigdec = 2
  )
  expect_named(actual, c("tsum", "gsum"))
  expected <- test_adsl |>
    mutate(BMIBLCH = as.numeric(BMIBLCH)) |>
    filter(!is.na(BMIBLCH)) |>
    group_by(across(all_of(c("BYVAR1", "BYVAR1N", "TRTVAR", "SUBGRPVAR1", "SUBGRPVAR1N")))) |>
    summarise(
      n = as.character(n()),
      mean = round_f(mean(.data[["BMIBLCH"]], na.rm = TRUE), 2),
      min = round_f(min(.data[["BMIBLCH"]], na.rm = TRUE), 2),
      median = round_f(median(.data[["BMIBLCH"]], na.rm = TRUE), 2),
      max = round_f(max(.data[["BMIBLCH"]], na.rm = TRUE), 2),
      sd = round_f(sd(.data[["BMIBLCH"]], na.rm = TRUE), 2)
    ) |>
    ungroup() |>
    mutate(DPTVAR = "BMIBLCH", CN = "N", DPTVARN = 1)
  expect_equal(actual$gsum, expected)
  actual1 <- msumstat(
    datain = adsl_entry,
    dptvar = "AGE",
    statvar = "box",
    sigdec = 2
  )
  expect_true(all(
    c("median", "q25", "q75", "whiskerlow", "whiskerup", "outliers") %in% names(actual1$gsum)
  ))
  expect_true(all(
    unique(actual1$tsum$DPTVAL) == c("median", "q25", "q75", "whiskerlow", "whiskerup", "outliers")
  ))
})


test_that("Test 3: Check errors", {
  expect_equal(
    msumstat(
      datain = adsl_entry |> filter(TRTVAR == "A"),
      dptvar = "BMIBL",
      statvar = "",
      sigdec = 2
    ),
    adsl_entry |> filter(TRTVAR == "A")
  )
  expect_equal(
    msumstat(
      datain = adsl_entry,
      dptvar = "BMIBL",
      a_subset = "TRTVAR == 'A'",
      statvar = "",
      sigdec = 2
    ),
    adsl_entry |> filter(TRTVAR == "A")
  )
  expect_error(
    msumstat(
      datain = adsl_entry,
      dptvar = "YVAR",
      statvar = "",
      sigdec = 2
    ),
    "Dependent Variable does not Exist"
  )
  nostat <- msumstat(
    datain = adsl_entry,
    dptvar = "AGE",
    statvar = "pvalue",
    sigdec = 2
  )
  expect_equal(unique(nostat$gsum$pvalue), "_NO_STAT")
})

test_that("Test 4: Filter", {
  m_filt <- msumstat(
    datain = adsl_entry,
    a_subset = "SEX == 'F'",
    dptvar = "AGE",
    statvar = "mean",
    sigdec = 2
  )
  expect_true(unique(m_filt$gsum$SUBGRPVAR1) == "F")
})

test_that("Test 5: Sparse by value", {
  adsl_entry <- mentry(
    datain = adsl,
    subset = "EFFFL=='Y'",
    byvar = "SEX",
    trtvar = "TRT01A",
    trtsort = "TRT01AN",
    pop_fil = NA
  )
  
  adsl_sum <- adsl_entry |>
    msumstat(
      dptvar = "AGE",
      a_subset = "SEX == 'F'",
      statvar = "mean(sd)~median(minmaxc)~q3",
      sigdec = "3(2)~2(0)~1",
      sparsebyvalyn = "Y"
    )
  expect_equal(unique(adsl_sum$gsum$BYVAR1), c("F", "M"))
})
