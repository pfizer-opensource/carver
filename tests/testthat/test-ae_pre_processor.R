# testcase 1:
data(adae)
data(FMQ_Consolidated_List)
adae1 <- adae |>
  dplyr::mutate(ASEVN = dplyr::recode(.data[["AESEV"]], "MILD" = 1, "MODERATE" = 2, "SEVERE" = 3))
test_that("Test Case 1: With standard arguments", {
  adaetest <- adae
  adaetest$AEDECOD[1] <- NA
  actual <- ae_pre_processor(
    datain = adaetest,
    ae_filter = "Any Event",
    obs_residual = NA,
    fmq_data = NA
  )
  date_formats <- c("%d%b%Y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")
  expected <- adaetest |>
    mutate(
      ASTDT = as.Date(.data[["ASTDT"]], tryFormats = date_formats, optional = FALSE),
      AENDT = as.Date(.data[["AENDT"]], tryFormats = date_formats, optional = FALSE),
      TRTSDT = as.Date(.data[["TRTSDT"]], tryFormats = date_formats, optional = FALSE),
      TRTEDT = as.Date(.data[["TRTEDT"]], tryFormats = date_formats, optional = FALSE)
    ) |>
    tidyr::drop_na(all_of("TRTSDT")) |>
    mutate(AEDECOD = if_else(is.na(.data[["AEDECOD"]]) & !is.na(.data[["ASTDT"]]),
                             "Not Yet Coded", .data[["AEDECOD"]]
    ))
  expect_named(actual, c("data", "a_subset"))
  expect_equal(actual$data, expected)
  expect_true(is.na(actual$a_subset))
})


test_that("Test Case 2: Check filtering", {
  actual <- ae_pre_processor(
    datain = adae,
    ae_filter = "Serious",
    obs_residual = 5,
    fmq_data = NA
  )
  date_formats <- c("%d%b%Y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")
  expected <- adae |>
    mutate(
      ASTDT = as.Date(.data[["ASTDT"]], tryFormats = date_formats, optional = FALSE),
      AENDT = as.Date(.data[["AENDT"]], tryFormats = date_formats, optional = FALSE),
      TRTSDT = as.Date(.data[["TRTSDT"]], tryFormats = date_formats, optional = FALSE),
      TRTEDT = as.Date(.data[["TRTEDT"]], tryFormats = date_formats, optional = FALSE)
    ) |>
    tidyr::drop_na(all_of("TRTSDT")) |>
    filter(
      .data[["AESER"]] == "Y", .data[["ASTDT"]] > .data[["TRTSDT"]],
      .data[["ASTDT"]] < (.data[["TRTEDT"]] + 5)
    )
  expect_named(actual, c("data", "a_subset"))
  expect_equal(actual$data, expected)
  expect_equal(
    actual$a_subset,
    "AESER == 'Y' & (ASTDT > TRTSDT) & (ASTDT < (TRTEDT + 5))"
  )
})

# test case 3:
test_that("Test Case 3: FMQ created from Consolidated List", {
  actual <- ae_pre_processor(
    datain = adae,
    ae_filter = NA,
    obs_residual = NA,
    fmq_data = FMQ_Consolidated_List
  )
  # FMQ is given as BYVAR
  expect_true("FMQ_NAM" %in% names(actual$data))
  # Using PT = "Anxiety" as example:
  Fmq_Anxiety <- FMQ_Consolidated_List |>
    filter(PT == "Anxiety") |>
    mutate(FMQ_NAM = paste0(FMQ, "/", FMQCAT))
  expectedfmq <- paste(unique(Fmq_Anxiety$FMQ_NAM), collapse = "~~")
  actualfmq <- actual$data |>
    filter(AEDECOD == "ANXIETY") |>
    distinct(FMQ_NAM) |>
    pull()
  expect_equal(actualfmq, expectedfmq)
})

# test case 3:
test_that("Test Case 4: Filters executed correctly", {
  expect_error(
    ae_pre_processor(
      datain = adae,
      ae_filter = "GRADE 1",
      obs_residual = NA,
      fmq_data = FMQ_Consolidated_List
    ), "ATOXGR not found in data. Cannot apply ae_filter"
  )
  actual <- ae_pre_processor(
    datain = adae,
    ae_filter = "Serious",
    subset = "AGE > 80",
    fmq_data = NA
  )
  expect_named(actual, c("data", "a_subset"))
  expect_equal(nrow(actual$data), 0)
  expect_equal(actual$a_subset, "AESER == 'Y' & AGE > 80")
  actual1 <- ae_pre_processor(
    datain = data.frame()
  )
  expect_equal(actual1, list(data = data.frame(), a_subset = NA_character_))
})

# test case 5:
test_that("Test Case 5: Max Sev/Toxicity", {
  actual <- ae_pre_processor(
    datain = adae1,
    subset = "TRTEMFL == 'Y' & SITEID == '703'",
    max_sevctc = "SEV",
    sev_ctcvar = "ASEVN",
    hterm = "AEBODSYS",
    lterm = "AEDECOD",
    pt_total = "Y"
  )
  expected <- adae1 |>
    filter(TRTEMFL == "Y", SITEID == "703") |>
    mutate(
      ASTDT = as.Date(.data[["ASTDT"]], tryFormats = date_formats, optional = FALSE),
      AENDT = as.Date(.data[["AENDT"]], tryFormats = date_formats, optional = FALSE),
      TRTSDT = as.Date(.data[["TRTSDT"]], tryFormats = date_formats, optional = FALSE),
      TRTEDT = as.Date(.data[["TRTEDT"]], tryFormats = date_formats, optional = FALSE)
    ) |>
    tidyr::drop_na(all_of("TRTSDT")) |>
    group_by(across(all_of(c("TRTA", "USUBJID", "AEBODSYS", "AEDECOD")))) |>
    mutate(MAX_SEVCTC = ifelse(.data[["ASEVN"]] == max(.data[["ASEVN"]], na.rm = TRUE), 1, 0)) |>
    filter(.data[["MAX_SEVCTC"]] == 1) |>
    group_by(across(any_of(c("TRTA", "USUBJID")))) |>
    mutate(ANY = ifelse(.data[["ASEVN"]] == max(.data[["ASEVN"]], na.rm = TRUE), 1, 0)) |>
    group_by(across(any_of(c("TRTA", "USUBJID", "AEBODSYS")))) |>
    mutate(HT_FL = ifelse(.data[["ASEVN"]] == max(.data[["ASEVN"]], na.rm = TRUE), 1, 0)) |>
    group_by(across(any_of(c("TRTA", "AEDECOD", "USUBJID")))) |>
    mutate(PT_CNT = ifelse(.data[["ASEVN"]] == max(.data[["ASEVN"]], na.rm = TRUE), 1, 0)) |>
    ungroup()
  expect_equal(actual$data, expected)
  expect_equal(actual$a_subset, "TRTEMFL == 'Y' & SITEID == '703' & MAX_SEVCTC == 1")
})
