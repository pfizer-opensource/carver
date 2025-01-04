# Copyright 2024 Pfizer Inc
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#' Incidence of Laboratory Test Abnormalities (Without Regard to Baseline Abnormality)
#'
#' @param datain Input dataset (`adlb`).
#' @param crit_vars Criteria variables
#' @param stathead Column label to display `n` in the output. Default is `n (%)`
#' Values: `"TRT", "VAR","COL", "SUBGRP", "SGRPN", "CAT", "NONE", "NO", "DPTVAR"`
#' @inheritParams mcatstat
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
#'     denom_subset = NA_character_,
#'     sigdec = 1
#'   ) |>
#'   display_bign_head(mentry_data = lb_entry) |>
#'   tbl_processor()
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
                                    denom_subset = NA_character_,
                                    sigdec = 2,
                                    sparseyn = "Y",
                                    pctsyn = "N",
                                    stathead = "n (%)") {
  # Data checks and error messages
  if (nrow(datain) < 1) {
    return(datain)
  }
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
  out_data <- seq_along(dptvars) |>
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
          pctdisp,
          sigdec,
          sparseyn,
          pctsyn,
          dptval
        )
    }) |>
    # combine and display lab abnormality table
    bind_rows()
  # Data check:
  if (nrow(out_data) == 0) {
    return(data.frame())
  }
  out_data |>
    mutate(across(c("DENOMN", "CVALUE"), as.character)) |>
    rename(N = DENOMN, !!stathead := CVALUE) |>
    pivot_longer(c("N", stathead), names_to = "SUBGRPVARX", values_to = "CVALUE") |>
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
           pctdisp,
           sigdec,
           sparseyn,
           pctsyn,
           dptvarn) {
    if (nrow(datain) == 0) {
      return(data.frame())
    }
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
        dptvarn = dptvarn,
        pctdisp = pctdisp,
        pctsyn = pctsyn,
        sigdec = sigdec,
        return_zero = "Y",
        sparseyn = sparseyn
      )
  }
