data(adsl)
# For testing purposes
adsl[1, "SEX"] <- NA_character_
adsl[2, "SITEGR1"] <- NA_character_
test_that("Test Case:1 mentry works with the inputs given and returns the expected items", {
  data_out <- mentry(
    datain = adsl,
    subset = "EFFFL=='Y'",
    byvar = "SEX",
    subgrpvar = "SITEGR1",
    trtvar = "TRT01A",
    trtsort = "TRT01AN",
    trttotalyn = "N",
    sgtotalyn = "N",
    add_grpmiss = "Y",
    pop_fil = "SAFFL"
  )
  # it returns a dataframe
  expect_s3_class(data_out, "data.frame")
  
  # testing asubset filter
  expect_equal(unique(data_out$EFFFL), "Y")
  
  # testing population filter
  expect_equal(unique(data_out$SAFFL), "Y")
  # Treatment check
  expect_s3_class(data_out$TRTVAR, "factor")
  
  # testing missing logic in byvar and subgrpvar
  expect_equal(
    unique(data_out$BYVAR1),
    stringr::str_replace_na(unique(adsl$SEX), "Missing")
  )
  expect_equal(
    unique(data_out$SUBGRPVAR1),
    stringr::str_replace_na(unique(adsl$SITEGR1), "Missing")
  )
})

test_that("Test Case:2 byvar and byvarn check", {
  data_out <- mentry(
    datain = adsl,
    subset = "EFFFL=='Y'",
    byvar = "SEX~ETHNIC",
    subgrpvar = "RACE/RACEN",
    trtvar = "TRT01A",
    trtsort = "TRT01AN",
    trttotalyn = "N",
    sgtotalyn = "N",
    add_grpmiss = "N",
    pop_fil = "SAFFL"
  )
  byvars_check <- c(
    "BYVAR1", "BYVAR1N", "BYVAR2", "BYVAR2N", "SUBGRPVAR1",
    "SUBGRPVAR1N"
  )
  expect_true(isTRUE(all(byvars_check %in% names(data_out))))
  expect_identical(unique(adsl$RACEN[!is.na(adsl$RACEN)]), unique(data_out$SUBGRPVAR1N))
  expect_identical(unique(data_out$BYVAR1), unique(adsl$SEX[!is.na(adsl$SEX)]))
  expect_false("Missing" %in% unique(data_out$BYVAR1))
})

test_that("Total and Treatment Values", {
  data_out <- mentry(
    datain = adsl,
    subset = NA_character_,
    subgrpvar = "SITEGR1",
    trtvar = "TRT01A",
    trtsort = NA,
    trttotalyn = "Y",
    sgtotalyn = "Y",
    add_grpmiss = "Y",
    pop_fil = "SAFFL"
  )
  chdata <- data_out |>
    filter(.data[["TRTVAR"]] %in%
             c("NOT ASSIGNED", "SCREEN FAILURE", "SCRNFAIL", "NOTRT", "NOTASSGN"))
  expect_equal(nrow(chdata), 0)
  exp <- data_out |>
    mutate(
      TRTSORT = as.numeric(factor(.data$TRTVAR)),
      TRTSORT = ifelse(.data[["TRTVAR"]] == "Total", 999, .data[["TRTSORT"]])
    ) |>
    select(all_of(names(data_out)))
  expect_equal(data_out, exp)
  expect_true("Total" %in% data_out$TRTVAR)
  expect_true("Total" %in% data_out$SUBGRPVAR1)
})

test_that("Check Empty Input", {
  data_out <- mentry(
    datain = data.frame(),
    subset = NA_character_,
    subgrpvar = "SITEGR1",
    trtvar = "TRT01A",
    trtsort = NA,
    trttotalyn = "Y",
    sgtotalyn = "Y",
    add_grpmiss = "Y",
    pop_fil = "SAFFL"
  )
  expect_equal(data_out, data.frame())
})
