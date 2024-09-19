data("adsl")
data("ae_pre_process")
ae_entry <-
  ae_pre_process$data |> mentry(
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
ad_entry <-
  adsl |> mentry(
    subset = "EFFFL=='Y'",
    trtvar = "TRT01A",
    trtsort = "TRT01AN",
    trttotalyn = "N",
    add_grpmiss = "N",
    sgtotalyn = "N",
    pop_fil = NA
  )

# Sample Output created from defaults
ad_sum <- ad_entry |>
  group_by(across(all_of(c("TRTVAR", "SEX")))) |>
  summarise(FREQ = length(unique(.data[["USUBJID"]]))) |>
  ungroup() |>
  group_by(across(all_of("TRTVAR"))) |>
  mutate(
    DPTVAL = as.character(.data[["SEX"]]), DENOMN = sum(.data[["FREQ"]]),
    PCT = round_f(100 * .data[["FREQ"]] / .data[["DENOMN"]], 2),
    CVALUE = paste0(.data[["FREQ"]], " (", .data[["PCT"]], "%)"), CN = "C",
    DPTVARN = 1, XVAR = .data[["DPTVAL"]],
    DPTVAR = "SEX", DPTVALN = as.numeric(factor(.data[["SEX"]]))
  ) |>
  ungroup() |>
  select(-all_of("SEX"))
# Sample Cumulative output
ad_cum <- ad_entry |>
  group_by(across(all_of(c("TRTVAR", "SEX")))) |>
  summarise(FREQ = length(unique(.data[["USUBJID"]]))) |>
  mutate(DPTVAL = as.character(.data[["SEX"]]), FREQ = cumsum(.data[["FREQ"]])) |>
  select(-all_of("SEX")) |>
  ungroup()


test_that("Case 1:mcatstat output with standard inputs and filters", {
  ad_mcat <- mcatstat(
    datain = ad_entry,
    uniqid = "USUBJID",
    dptvar = "SEX",
    pctdisp = "TRT"
  )
  # Check output returned is in expected format
  expect_type(ad_mcat, "list")
  expect_true(all(names(ad_mcat) %in% names(ad_sum)))
  # Check values are matched with expected:
  expect_equal(ad_mcat, ad_sum |> select(all_of(names(ad_mcat))), ignore_attr = TRUE)
})

test_that("Case 2: Empty input", {
  expect_error(
    mcatstat(
      datain = ad_entry |> filter(USUBJID == "A"),
      dptvar = "SEX",
      pctdisp = "TRT"
    ),
    "No data for mcatstat"
  )
  expect_error(
    mcatstat(
      datain = ad_entry,
      uniqid = "XYZVAR",
      dptvar = "SEX",
      pctdisp = "TRT"
    ),
    "uniqid should exist in data or be ALLCT"
  )
})


test_that("Case 3: Unique ID and sign variation", {
  m_subj <- mcatstat(
    datain = ae_entry,
    uniqid = "USUBJID",
    dptvar = "AEDECOD",
    pctdisp = "TRT",
    pctsyn = "N"
  )
  m_na <- mcatstat(
    datain = ae_entry,
    uniqid = "ALLCT",
    dptvar = "AEDECOD",
    pctdisp = "TRT",
    pctsyn = "N"
  )

  # All groups and order except actual count values should be equal
  expect_equal(
    m_subj |> select(-all_of(c("DENOMN", "FREQ", "PCT", "CVALUE"))),
    m_na |> select(-all_of(c("DENOMN", "FREQ", "PCT", "CVALUE")))
  )

  # Counts are different
  expect_false(setequal(unique(m_subj$DENOMN), unique(m_na$DENOMN)))

  # Check values (for denominator, should also apply to numerator)
  check_na <- ae_entry |>
    ungroup() |>
    group_by(TRTVAR) |>
    summarise(DENOMN = n()) |>
    ungroup()

  expect_equal(m_na |> distinct(across(all_of(c("TRTVAR", "DENOMN")))),
    check_na,
    ignore_attr = TRUE
  )
  expect_false(any(grep("%", m_subj$CVALUE)))
})

test_that("Case 4: Percentage denominator variation and denomyn", {
  # Invalid value for pctdisp:
  expect_error(mcatstat(
    datain = ad_entry,
    dptvar = "SEX",
    pctdisp = "ABC"
  ), "Invalid pctdisp")

  m_none <- mcatstat(
    datain = ad_entry,
    dptvar = "SEX",
    pctdisp = "NONE"
  )

  # No percentage columns:
  expect_false(any(c("PCT", "DENOMN") %in% names(m_none)))
  expect_equal(m_none$FREQ, m_none$CVALUE)

  # Variable total as denominator
  m_var <- mcatstat(
    datain = ad_entry,
    dptvar = "SEX",
    pctdisp = "VAR",
    denomyn = "Y"
  )
  # Entire matrix has same denominator - check value
  expect_length(unique(m_var$DENOMN), 1)
  expect_equal(unique(m_var$DENOMN), length(unique(ad_entry$USUBJID)))
  expect_true(all(str_detect(m_var$CVALUE, paste0("/", unique(m_var$DENOMN)))))
})

test_that("Case 5: Cumulative Frequency and Total", {
  m_cum <- mcatstat(
    datain = ad_entry,
    dptvar = "SEX",
    pctdisp = "NO",
    cum_ctyn = "Y"
  )
  expect_equal(m_cum[, c("TRTVAR", "FREQ", "DPTVAL")], ad_cum)

  m_total <- mcatstat(
    datain = ad_entry,
    dptvar = "SEX",
    pctdisp = "TRT",
    total_catyn = "Y",
    cum_ctyn = "N"
  )
  expect_true("Total" %in% unique(m_total$DPTVAL))
  # Test single group
  m_pl1 <- ad_sum |>
    filter(.data[["TRTVAR"]] == "Placebo") |>
    select(FREQ) |>
    sum()
  m_pl2 <- m_total |>
    filter(.data[["TRTVAR"]] == "Placebo", .data[["DPTVAL"]] == "Total") |>
    pull(FREQ)
  expect_equal(m_pl2, m_pl1)
})

test_that("Case 6: Filters check", {
  m_num <- mcatstat(
    datain = ad_entry,
    a_subset = "SEX == 'F'",
    uniqid = "USUBJID",
    dptvar = "SEX",
    pctdisp = "TRT"
  )
  m_denom <- mcatstat(
    datain = ad_entry,
    denom_subset = "SEX == 'F'",
    uniqid = "USUBJID",
    dptvar = "SEX",
    pctdisp = "VAR"
  )
  expect_equal(unique(m_num$DPTVAL), "F")
  denomvalue <- ad_entry |>
    filter(.data[["SEX"]] == "F") |>
    distinct(USUBJID) |>
    nrow()
  expect_equal(unique(m_denom$DENOMN), denomvalue)
  m_num1 <- mcatstat(
    datain = ad_entry,
    a_subset = "SEX == 'X'",
    uniqid = "USUBJID",
    dptvar = "SEX",
    pctdisp = "TRT"
  )
  expect_equal(nrow(m_num1), 0)
})
