
#' Incidence of Laboratory Test Abnormalities (Without Regard to Baseline Abnormality)
#'
#' @param datain Input dataset (`adlb`).
#' @param crit_vars Criteria variables
#' @param pctdisp Denominator to calculate percentages by.
#' Values: `"TRT", "VAR","COL", "SUBGRP", "SGRPN", "CAT", "NONE", "NO", "DPTVAR"`
#' @param a_subset Subset conditions for analysis of dependent variable.
#' @param denom_subset Subset conditions for denominator eg. `"APSBLFL == 'Y'"`
#'
#' @return `data.frame` with summary of laboratory abnormality incidence counts
#' @export
#'
#' @examples
#' data("lab_data")
#'
#' lb_entry <- lab_data$adlb |>
#'   mentry(
#'     subset = NA_character_,
#'     byvar = "PARCAT1~PARAM",
#'     subgrpvar = NA_character_,
#'     trtvar = "TRTA",
#'     trtsort = "TRTAN",
#'     trttotalyn = "N",
#'     sgtotalyn = "N",
#'     add_grpmiss = "N",
#'     pop_fil = "SAFFL"
#'   )
#'
#' out <-
#'   lb_entry |>
#'   lab_abnormality_summary(
#'     crit_vars = "CRIT3~CRIT4",
#'     pctdisp = "SUBGRP",
#'     a_subset = NA_character_,
#'     denom_subset = NA_character_
#'   ) |>
#'   display_bign_head(mentry_data = lb_entry) |>
#'   tbl_processor(
#'     dptlabel = ""
#'   )
#'
#' out
#'
#' # `flextable` output
#' out |>
#'   tbl_display(
#'     bylabel = "Parameter Category~Parameter",
#'     dpthead = "Primary Criteria"
#'   )
#'
lab_abnormality_summary <- function(datain,
                                    crit_vars = "CRIT3~CRIT4",
                                    pctdisp = "SUBGRP",
                                    a_subset = NA_character_,
                                    denom_subset = NA_character_) {
  # Data checks and error messages
  stopifnot(is.data.frame(datain) && nrow(datain) > 0)
  dptvars <- toupper(str_to_vec(crit_vars))
  dptvars_fl <- glue("{dptvars}FL")
  byvars <- var_start(datain, "BYVAR")
  byvarsN <- glue("{byvars}N")
  stopifnot("Criteria Variables/Flags not present in `datain`" = all(dptvars %in% names(datain)) ||
              all(dptvars_fl %in% names(datain)))
  # handle denom_subset when not specified
  if (is.na(denom_subset) || str_squish(denom_subset) == "") {
    if ("APSBLFL" %in% names(datain)) {
      message("`denom_subset` not specified, set to APSBLFL == 'Y'")
      dsubset <- c("APSBLFL == 'Y'")
    } else {
      stop(
        "`APSBLFL` not present in `datain`, please provide a valid denominator subset condition"
      )
    }
  } else {
    dsubset <- denom_subset
  }
  # Pre process adlb
  adlb <- datain |>
    filter(!str_sub(.data[["PARAMCD"]], start = -2L) %in% c("PL", "SL")) |>
    # Replace missing values numeric equivalent grouping variables with 0
    mutate(across(any_of(byvarsN), ~ replace_na(., 0)))
  # Calculate lab abnormalities by Criteria Flags
  seq_along(dptvars) |>
    map(\(dptval) {
      asubset <- glue("{dptvars_fl[dptval]} == 'Y'")
      if (!is.na(a_subset) &&
          str_squish(a_subset) != "") {
        asubset <- glue("{a_subset} & {asubset}")
      }
      ## add lab abnormality counts
      adlb |>
        count_abnormalities(
          asubset,
          dsubset,
          toupper(byvars),
          dptvars[[dptval]],
          pctdisp
        )
    }) |>
    # combine and display lab abnormality table
    bind_rows() |>
    mutate(across(c("DENOMN", "CVALUE"), as.character)) |>
    rename(N = DENOMN, n = CVALUE) |>
    pivot_longer(c("N", "n"), names_to = "SUBGRPVARX", values_to = "CVALUE") |>
    mutate(SUBGRPVARXN = 9999)
}

#' Count Lab Abnormalities
#'
#' @inheritParams lab_abnormality_summary
#'
#' @return List of data frames
#' @noRd
#'
count_abnormalities <-
  function(datain,
           a_subset,
           denom_subset,
           byvars,
           dptvars,
           pctdisp) {
    crit_df <-
      datain |>
      filter(.data[[dptvars]] != "") |>
      group_by(across(all_of(c(byvars, dptvars)))) |>
      distinct() |>
      ungroup() |>
      rename(DPTVAR = all_of(dptvars))
    ## summarize categorical variables on data filtered by criteria flags
    crit_df |>
      mcatstat(
        a_subset = a_subset,
        denom_subset = denom_subset,
        dptvar = "DPTVAR",
        pctdisp = pctdisp,
        pctsyn = "N"
      )
  }
