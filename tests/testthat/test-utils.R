# Test Utils functions

data("adae")
data("adsl")
data("ae_pre_process")

ae_pre <- mentry(
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
## data_attrib() testing:

test_that("Case 1: data_attrib works as expected", {
  # Labels for adae data:
  adae_lab <- data_attrib(adae)
  expect_true(class(adae_lab) == "data.frame")
  expect_equal(adae_lab$VAR_NAMES, toupper(colnames(adae)))
  expect_equal(
    adae_lab$VAR_LABEL,
    unlist(lapply(names(adae), function(x) attributes(adae[[x]])$label))
  )
})

#############################################################################
## var_start testing

test_that("Case 1: Pattern works with expected input", {
  age_start <- var_start(adsl, "AGE")
  expect_equal(age_start, c("AGE", "AGEGR1", "AGEU"))
})

test_that("Case 2: With or without 'N'", {
  # Labels for adae data:
  byno_n <- var_start(ae_pre, "BYVAR")
  expect_equal(byno_n, "BYVAR1")
  by_n <- var_start(ae_pre, "BYVARN")
  expect_equal(by_n, "BYVAR1N")
  agen_start <- var_start(adsl, "AGEGRN")
  expect_equal(agen_start, c("AGEGR1N"))
})

# Function: split_section_headers
# Test Data

adsl_entry <- mentry(
  datain = adsl,
  subset = "EFFFL=='Y'",
  byvar = NA,
  trtvar = "TRT01A",
  trtsort = "TRT01AN",
  subgrpvar = "SEX~AGEGR1"
)

# Test cases
test_that("Test 1: Check for splitting ", {
  actual <- split_section_headers(
    datain = adsl_entry,
    split_by = "SUBGRPVAR1",
    split_by_prefix = "",
    split_lab = "Group:",
    sep = "~"
  )
  # Check for level names count is equal
  expect_equal(actual, paste0("Group:", unique(adsl_entry$SEX)))
  actual1 <- split_section_headers(
    datain = adsl_entry,
    split_by = "",
    split_by_prefix = "SUBGRPVAR",
    split_lab = "Group:~Age Group:",
    sep = " & "
  )
  expected1 <- adsl_entry |>
    arrange(across(all_of(c("SUBGRPVAR1", "SUBGRPVAR2")))) |>
    distinct(across(all_of(c("SUBGRPVAR1", "SUBGRPVAR2")))) |>
    mutate(col = paste0("Group:", .data[["SUBGRPVAR1"]], " & Age Group:", .data[["SUBGRPVAR2"]])) |>
    pull(col)
  # Check for level names count is equal
  expect_equal(actual1, expected1)
})

test_that("Test 2: Check for exceptions", {
  actual <- split_section_headers(
    datain = adsl_entry,
    split_by = "",
    split_by_prefix = "",
    split_lab = "Group:",
    sep = "~"
  )
  # Check for level names count is equal
  expect_equal(actual, "")
  expect_error(
    split_section_headers(
      datain = adsl_entry,
      split_by = "",
      split_by_prefix = "TSTVAR",
      split_lab = "Group:",
      sep = " & "
    ), "No variables with split_by_prefix"
  )

  expect_error(
    split_section_headers(
      datain = adsl_entry,
      split_by = "TSTVAR",
      split_by_prefix = "",
      split_lab = "Group:",
      sep = " & "
    ), "split_by var should exist in data"
  )
})

#############################################################################
# split_data_by_variable

test_that("Test 1: Check for splitting data", {
  actual <- split_data_by_var(
    datain = adsl_entry,
    split_by = "SUBGRPVAR1",
    split_by_prefix = ""
  )
  expected <- adsl_entry |>
    group_by(across(all_of("SUBGRPVAR1"))) |>
    group_split()
  expect_equal(actual, expected)
  actual1 <- split_data_by_var(
    datain = adsl_entry,
    split_by = "",
    split_by_prefix = "SUBGRPVAR"
  )
  expected1 <- adsl_entry |>
    group_by(across(all_of(c("SUBGRPVAR1", "SUBGRPVAR2")))) |>
    group_split()
  # Check for level names count is equal
  expect_equal(actual1, expected1)
})

test_that("Test 2: Check for exceptions", {
  actual <- split_data_by_var(
    datain = adsl_entry,
    split_by = "",
    split_by_prefix = ""
  )
  # Check for level names count is equal
  expect_equal(actual, list(adsl_entry))
  expect_error(
    split_data_by_var(
      datain = adsl_entry,
      split_by = "",
      split_by_prefix = "TSTVAR"
    ), "No variables with split_by_prefix"
  )

  expect_error(
    split_data_by_var(
      datain = adsl_entry,
      split_by = "TSTVAR",
      split_by_prefix = ""
    ), "split_by var should exist in data"
  )
})

test_that("Test round_f() works", {
  expect_equal(round_f(13.4, 2), "13.40")
  expect_equal(round_f(12.243, 1), "12.2")
})

test_that("ord_summ_df works as expected", {
  actual <- iris |>
    ord_summ_df("Sepal.Length", "Descending")
  expected <- iris |>
    arrange(desc(.data[["Sepal.Length"]]))

  expect_identical(actual, expected)

  actual1 <- iris |>
    ord_summ_df("Petal.Width", "Alphabetical")
  expected1 <- iris |>
    arrange(.data[["Petal.Width"]])
  iristest <- iris |>
    mutate(BYVAR1 = .data[["Species"]])
  actual2 <- iristest |>
    ord_summ_df("Sepal.Length", "Ascending", "Y")
  expected2 <- iristest |>
    group_by(across(any_of("BYVAR1"))) |>
    arrange(.data[["Sepal.Length"]], .by_group = TRUE) |>
    ungroup()

  expect_identical(actual, expected)

  expect_identical(actual1, expected1)
  expect_identical(actual2, expected2)
})

test_that("get_sort_var works as expected", {
  actual <-
    map_chr(c("Count", "Percent", "RiskValue", "Alphabetical", "abc"), \(x) get_sort_var(x))
  expected <- c("CTRL_N", "CTRL_PCT", "RISK", "DPTVAL", "abc")

  expect_identical(actual, expected)
})

mcat_data <- mcatstat(adsl_entry,
  dptvar = "RACE"
)

test_that("add_bigN works as expected", {
  grpvar <- c("TRTVAR", "SUBGRPVAR1", "SUBGRPVAR2")
  actual <- add_bigN(
    data = mcat_data,
    dsin = adsl_entry,
    grpvar = grpvar,
    modvar = "TRTVAR"
  )
  bign <- adsl_entry |>
    group_by(across(all_of(grpvar))) |>
    summarise(BIGN = length(unique(.data[["USUBJID"]])))
  expected <- mcat_data |>
    left_join(bign, by = grpvar) |>
    mutate(TRTVAR_BIGN = paste0(.data[["TRTVAR"]], " (N=", .data[["BIGN"]], ")")) |>
    select(-all_of("BIGN"))
  expected[["TRTVAR_BIGN"]] <- factor(
    expected[["TRTVAR_BIGN"]],
    levels = unique(expected[["TRTVAR_BIGN"]][order(expected[["TRTVAR"]])])
  )
  expect_equal(actual, expected, ignore_attr = TRUE)
})

test_that("display_bign_head works as expected", {
  actual <- display_bign_head(
    datain = mcat_data,
    mentry_data = adsl_entry,
    trtbignyn = "Y",
    subbignyn = "N",
    colformat = " n (%)"
  )
  exp <- add_bigN(
    data = mcat_data,
    dsin = adsl_entry,
    grpvar = "TRTVAR",
    modvar = "TRTVAR"
  ) |>
    mutate(SUBGRPVAR2 = paste0(.data[["SUBGRPVAR2"]], " n (%)")) |>
    select(-all_of("TRTVAR")) |>
    rename("TRTVAR" = "TRTVAR_BIGN")
  expect_equal(actual, exp, ignore_attr = TRUE)

  # Only treatment no subgrp
  actual1 <- display_bign_head(
    datain = ae_pre,
    mentry_data = ae_pre,
    trtbignyn = "Y",
    subbignyn = "N",
    colformat = "trt"
  )
  exp1 <- add_bigN(
    data = ae_pre,
    dsin = ae_pre,
    grpvar = "TRTVAR",
    modvar = "TRTVAR"
  )
  expect_equal(levels(actual1$TRTVAR), paste0(levels(exp1$TRTVAR_BIGN), "trt"))
})

test_that("display_bign_head works as expected for subgrp n value", {
  actual <- display_bign_head(
    datain = mcat_data,
    mentry_data = adsl_entry,
    trtbignyn = "N",
    subbignyn = "Y",
    colformat = ""
  )
  exp <- add_bigN(
    data = mcat_data,
    dsin = adsl_entry,
    grpvar = c("TRTVAR", "SUBGRPVAR1"),
    modvar = "SUBGRPVAR1"
  ) |>
    add_bigN(
      dsin = adsl_entry,
      grpvar = c("TRTVAR", "SUBGRPVAR1", "SUBGRPVAR2"),
      modvar = "SUBGRPVAR2"
    ) |>
    select(-all_of(c("SUBGRPVAR1", "SUBGRPVAR2"))) |>
    rename("SUBGRVAR2" = "SUBGRPVAR2_BIGN", "SUBGRVAR1" = "SUBGRPVAR1_BIGN")
  expect_equal(actual, exp, ignore_attr = TRUE)
})

adsl_entry1 <- mentry(
  datain = adsl,
  subset = "EFFFL=='Y'",
  byvar = NA,
  trtvar = NA,
  trtsort = NA,
  subgrpvar = NA
)
mcat_data1 <- adsl_entry1 |>
  mcatstat(dptvar = "RACE")

test_that("display_bign_head works as expected without treatment/subgrp", {
  actual <- display_bign_head(
    datain = mcat_data1,
    mentry_data = adsl_entry1,
    notrthead = "Total n (%)"
  )
  head <- paste0("Total n(%) (N = ", length(unique(adsl_entry1$USUBJID)), ")")
  exp <- mcat_data1 |>
    mutate(!!head := .data[["CVALUE"]]) |>
    select(-all_of("CVALUE"))
  expect_equal(actual, exp, ignore_attr = TRUE)
})

test_that("sparse_vals works as expected", {
  data_entry <- mentry(
    adsl,
    byvar = "SEX",
    trtvar = "TRT01A",
    trtsort = "TRT01AN",
    subset = "SAFFL == 'Y'"
  ) |>
    mutate(DPTVAL = .data[["AGEGR1"]], DPTVALN = .data[["AGEGR1N"]])
  count <- data_entry |>
    dplyr::filter(.data[["SEX"]] == "F") |>
    group_by(across(all_of(c("BYVAR1", "TRTVAR", "DPTVAL")))) |>
    summarise(FREQ = length(unique(.data[["USUBJID"]])))
  actual <- sparse_vals(
    count,
    data_sparse = data_entry,
    sparseyn = "N",
    sparsebyvalyn = "Y",
    "BYVAR1",
    character(0),
    "BYVAR1N",
    character(0)
  )
  expect_s3_class(actual, "data.frame")
  expect_equal(setdiff(unique(actual$BYVAR1), unique(count$BYVAR1)), "M")
  expect_equal(unique(actual$FREQ[actual$BYVAR1 == "M"]), 0)
  count1 <- data_entry |>
    dplyr::filter(.data[["AGEGR1"]] != "<65") |>
    group_by(across(all_of(c("BYVAR1", "TRTVAR", "DPTVAL")))) |>
    summarise(FREQ = length(unique(.data[["USUBJID"]])))
  actual1 <- sparse_vals(
    count1,
    data_sparse = data_entry,
    sparseyn = "Y",
    sparsebyvalyn = "N",
    "BYVAR1",
    character(0),
    "BYVAR1N",
    character(0)
  )
  expect_s3_class(actual1, "data.frame")
  expect_equal(setdiff(unique(actual1$DPTVAL), unique(count1$DPTVAL)), "<65")
  actual2 <- sparse_vals(
    count1,
    data_sparse = data_entry,
    sparseyn = "N",
    sparsebyvalyn = "N",
    "BYVAR1",
    character(0),
    "BYVAR1N",
    character(0)
  )
  expect_s3_class(actual2, "data.frame")
  expect_equal(actual2, count1)
})

test_that("sparse_vals with summary stat", {
  data_entry <- mentry(
    adsl,
    byvar = "SEX",
    trtvar = "TRT01A",
    trtsort = "TRT01AN",
    subset = "SAFFL == 'Y'"
  )
  sums <- data_entry |>
    dplyr::filter(.data[["SEX"]] == "F") |>
    group_by(across(all_of(c("BYVAR1", "TRTVAR")))) |>
    summarise(mean = as.character(mean(.data[["AGE"]], na.rm = TRUE)))
  actual <- sparse_vals(
    sums,
    data_sparse = data_entry,
    sparseyn = "N",
    sparsebyvalyn = "Y",
    "BYVAR1",
    character(0),
    "BYVAR1N",
    character(0),
    fillvar = "mean",
    fill_with = "-"
  )
  expect_s3_class(actual, "data.frame")
  expect_equal(setdiff(unique(actual$BYVAR1), unique(sums$BYVAR1)), "M")
  expect_equal(unique(actual$mean[actual$BYVAR1 == "M"]), "-")
})

test_that("dataset_vignette works", {
  actual <- dataset_vignette(adsl, c("USUBJID", "TRT01A"), subset = "AGE >= 88")
  expdata <- filter(adsl, .data[["AGE"]] >= 88)
  expect_true("datatables" %in% class(actual))
  expect_s3_class(actual$x$data, "data.frame")
  expect_equal(nrow(actual$x$data), nrow(expdata))
  expect_equal(ncol(actual$x$data), ncol(expdata))
})
