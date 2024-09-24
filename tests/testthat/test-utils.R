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


#############################################################################
# summary_functions

test_that("Test 1: Summary functions return all functions", {
  ui_sigDec <- 2
  list_stats <- summary_functions(ui_sigDec)
  # Test
  expect_named(list_stats, c(
    "mean", "min", "max", "median", "iqr", "var", "sum", "sd", "q25",
    "q75", "p1", "p10", "p5", "p90", "p95", "p99", "meansd", "range",
    "q1q3", "medianrange", "whiskerlow", "whiskerup", "outliers",
    "geom_lowci", "geom_upci", "geommean", "n"
  ))
})

test_that("Test 2: Summary function returns expected values", {
  list_stats <- summary_functions(2)[c("meansd", "q1q3", "n", "outliers", "range")]
  actstats <- iris |>
    group_by(Species) |>
    summarise_at(.vars = "Sepal.Width", .funs = list_stats)
  fn_outlier <- function(x) {
    x <- x[!is.na(x)]
    paste(unique(x[x < min(
      x[(x >= (quantile(x, 0.25) - 1.5 * IQR(x))) & (x <= quantile(x, 0.25))]
    ) |
      x > max(
        x[(x <= (quantile(x, 0.75) + 1.5 * IQR(x))) & (x >= quantile(x, 0.75))]
      )]), collapse = "~")
  }
  expstats <- iris |>
    group_by(Species) |>
    summarise(
      meansd = paste0(
        round_f(mean(.data[["Sepal.Width"]]), 2),
        " (", round_f(sd(.data[["Sepal.Width"]]), 3), ")"
      ),
      q1q3 = paste0(
        "(", round_f(quantile(.data[["Sepal.Width"]], 0.25), 2), ", ",
        round_f(quantile(.data[["Sepal.Width"]], 0.75), 2), ")"
      ),
      n = as.character(n()),
      outliers = fn_outlier(.data[["Sepal.Width"]]),
      range = paste0(
        "(", round_f(min(.data[["Sepal.Width"]]), 2), ", ",
        round_f(max(.data[["Sepal.Width"]]), 2), ")"
      )
    )
  expect_equal(actstats, expstats)
})

test_that("Test 3: Summary function returns expected values", {
  set.seed(123)
  test_data <- rnorm(100, mean = 10, sd = 2)
  expected_geom_lowci <- exp(mean(log(test_data), na.rm = TRUE) -
    qt(0.975, df = length(test_data) - 1) *
      sd(log(test_data)) / sqrt(length(test_data)))
  expected_geom_upci <- exp(mean(log(test_data), na.rm = TRUE) +
    qt(0.975, df = length(test_data) - 1) *
      sd(log(test_data)) / sqrt(length(test_data)))
  expected_geommean <- exp(mean(log(test_data), na.rm = TRUE))

  expect_equal(summary_functions()$geom_lowci(test_data), paste(expected_geom_lowci))
  expect_equal(summary_functions()$geom_upci(test_data), paste(expected_geom_upci))
  expect_equal(summary_functions()$geommean(test_data), paste(expected_geommean))
})

#############################################################################

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
# Function: fmtrd
test_that("Test fmtrd function for calculation and precision", {
  # 1: Applying fmtrd function with mean
  # Test Data
  sample_data <- c(10.23, 20, NA, 40, 50, 75.567)
  mean_1 <- round_f(mean(sample_data, na.rm = TRUE), 2)
  max_1 <- round_f(max(sample_data, na.rm = TRUE), 2)
  expect_equal(fmtrd("mean")(sample_data), mean_1)
  expect_equal(fmtrd("max")(sample_data), max_1)
  # Significant decimal resolved correctly
  mean_2 <- round_f(mean(sample_data, na.rm = TRUE), 3)
  expect_equal(fmtrd("mean", d = 3)(sample_data), mean_2)
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
