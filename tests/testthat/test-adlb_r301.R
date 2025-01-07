data("adlb")

lb_entry <- adlb |>
  mentry(
    subset = NA_character_,
    byvar = "PARCAT1~PARAM",
    subgrpvar = NA_character_,
    trtvar = "TRTA",
    trtsort = NA_character_,
    trttotalyn = "N",
    sgtotalyn = "N",
    add_grpmiss = "N",
    pop_fil = "SAFFL"
  )

lb_entry_subgrp <- adlb |>
  mentry(
    subset = NA_character_,
    byvar = "PARCAT1~PARAM",
    subgrpvar = "RACE",
    trtvar = "TRTA",
    trtsort = "TRTAN",
    trttotalyn = "Y",
    sgtotalyn = "Y",
    add_grpmiss = "Y",
    pop_fil = "SAFFL"
  )

lb_entry_subset <- adlb |>
  mentry(
    subset = NA_character_,
    byvar = "PARCAT1~PARAM",
    subgrpvar = "RACE",
    trtvar = "TRTA",
    trtsort = NA_character_,
    trttotalyn = "N",
    sgtotalyn = "N",
    add_grpmiss = "N",
    pop_fil = "SAFFL"
  )

test_that("lab_abnormality_summary works as expected with different options", {
  out <- lb_entry |>
    lab_abnormality_summary(
      crit_vars = "CRIT3~CRIT4",
      pctdisp = "SUBGRP",
      a_subset = NA_character_,
      denom_subset = NA_character_
    )

  out <- out |>
    display_bign_head(lb_entry) |>
    tbl_processor(addrowvars = NA_character_)

  out_by_subgrp <-
    lb_entry_subgrp |>
    lab_abnormality_summary(
      crit_vars = "CRIT3~CRIT4",
      pctdisp = "SUBGRP",
      a_subset = NA_character_,
      denom_subset = "APSBLFL == 'Y'"
    )

  out_by_subgrp <- out_by_subgrp |>
    display_bign_head(mentry_data = lb_entry_subgrp) |>
    tbl_processor(addrowvars = NA_character_)

  out_by_subset <-
    lb_entry_subset |>
    lab_abnormality_summary(
      crit_vars = "CRIT3~CRIT4",
      pctdisp = "SUBGRP",
      a_subset = "RACE == 'WHITE'"
    )

  out_by_subset <- out_by_subset |>
    display_bign_head(mentry_data = lb_entry_subset) |>
    tbl_processor(addrowvars = NA_character_)

  purrr::walk(out, \(x) expect_snapshot(x))
  purrr::walk(out_by_subgrp, \(x) expect_snapshot(x))
  purrr::walk(out_by_subset, \(x) expect_snapshot(x))
})

test_that("lab_abnormality_summary returns expected error messages", {
  df_ <- lb_entry |>
    dplyr::select(-APSBLFL)

  expect_error(
    lab_abnormality_summary(
      datain = df_,
      crit_vars = "CRIT3~CRIT4",
      pctdisp = "SUBGRP",
      a_subset = NA_character_,
      denom_subset = NA_character_
    ),
    "`APSBLFL` not present in `datain`, please provide a valid denominator subset condition"
  )

  expect_error(
    lab_abnormality_summary(
      datain = df_,
      crit_vars = "CRIT3~CRIT487900",
      pctdisp = "SUBGRP",
      a_subset = NA_character_,
      denom_subset = NA_character_
    )
  )
})
