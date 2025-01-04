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
#' Summary analysis for categorical variables
#'
#' \code{mcatstat()} returns a new dataframe containing counts and percentages
#' of a categorical analysis variable according to grouping and treatment
#' variables passed in \code{mentry()}
#'
#' @param datain Input data from `mentry()` output to get counts for each
#'  category
#' @param a_subset Analysis Subset condition specific to categorical analysis.
#' @param denom_subset Subset condition to be applied to data set for calculating denominator.
#' @param uniqid Variable(s) to calculate unique counts of. eg. `"USUBJID"`, `"SITEID"`,
#' `"ALLCT"`
#' @param dptvar Categorical Analysis variable and ordering variable if exists,
#' separated by /. eg: `"SEX"`, `"SEX/SEXN"`, `"AEDECOD"`, `"ISTPT/ISTPTN"`
#' @param pctdisp Method to calculate denominator (for %) by.
#' Possible values: `"TRT"`, `"VAR"`, `"COL"`, `"SUBGRP"`, `"CAT"`, `"NONE"`, `"NO"`, `"DPTVAR"`,
#' `"BYVARxyN"`
#' @param miss_catyn To include empty/blank values as `miss_catlabel` in categories of
#' `dptvar` variable or not. Values: `"Y"/"N"`
#' @param miss_catlabel Label for missing values
#' @param cum_ctyn To return cumulative frequency instead of individual
#' frequencies for each category. Values: `"Y"/"N"`
#' @param total_catyn To return a 'Total' row for the categories of `dptvar`
#' variable or not. Possible values: `"Y"/"N"`
#' @param total_catlabel Label for total category row. eg- "All"/"Total"
#' @param dptvarn Number to assign as `DPTVARN`, useful for block sorting when
#' multiple `mcatstat()` outputs are created to be combined.
#' @param pctsyn Display Percentage Sign in table or not. Values: `"Y"/"N"`
#' @param sigdec Number of decimal places for % displayed in output
#' @param denomyn Display denominator in output or not. Values: `"Y"/"N"`
#' @param sparseyn To sparse missing categories/treatments or not? `"Y"/"N"`
#' @param sparsebyvalyn Sparse missing categories within by groups. `"Y"/"N"`
#' @param return_zero Return rows with zero counts if analysis subset/ non-missing does not
#' exist in data. `"Y"/"N"`
#'
#' @details
#' \itemize{
#' \item Object passed to `datain` is the return element from `mentry()`
#' \item `a_subset` condition is applied to data to be analysed, and not applied for getting
#' denominator
#' \item `denom_subset` condition, if given, to apply to denominator data alone. Usually used
#' with `pctdisp` = "SUBGRP" or "DPTVAR"
#' \item `uniqid` is the variable name to get unique counts of. If given as
#' *"ALLCT"*, it sums all observations for the given category. If *"USUBJID"* then it
#' calculates the number of unique subjects per category.
#' \item `cum_ctyn` as `"Y"` to get output value as cumulative frequencies instead
#'  of individual frequencies. If `"Y"`, `total_catyn` will be reset to `"N"`
#' \item `pctdisp` has possible values for method to get denominator to calculate
#'  percentage:
#'  \itemize{
#'    \item **NONE/NO**: No percent calculation
#'    \item **TRT**: Treatment total counts acts as denominator
#'    \item **VAR**: Variable Total of all treatments/groups acts as denominator
#'    \item **COL**: Column wise denominator - percentage within each `Treatment-Subgroup(s)`
#'    combination
#'    \item **CAT**: Row-wise denominator - percentage within each `Bygroup(s)-dptvar` combination
#'    \item **SUBGRP**: Percentage within each `Treatment-By group(s)-Subgroup(s)` combination
#'    \item **DPTVAR**: Percentage within each `Treatment-By group(s)-Subgroup(s)-dptvar`
#'    combination.
#'    \item **BYVARxyN**: Percentage using `Treatment-Bygroup` combination as denominator. eg if
#'    `BYVAR12N` then uses `TRT-BYVAR1-BYVAR2` combination, if `BYVAR1N` then only `TRT-BYVAR1`
#'    \item **SGRPN**: Percentage using Subgroup total as denominator
#'  }
#' }
#'
#' @return a data.frame with counts and/or percentages, passed to
#'   passed to `tbl_processor()` or graph functions
#' @export
#'
#' @examples
#' data("adsl")
#'
#' df_mentry <-
#'   adsl |> mentry(
#'     subset = "EFFFL=='Y'",
#'     byvar = "AGEGR1",
#'     trtvar = "TRT01A",
#'     trtsort = "TRT01AN",
#'     subgrpvar = "SEX/SEXN",
#'     trttotalyn = "N",
#'     add_grpmiss = "N",
#'     sgtotalyn = "N",
#'     pop_fil = "Overall Population"
#'   )
#'
#' df_mentry |>
#'   mcatstat(
#'     a_subset = "SUBGRPVAR1 == 'F'",
#'     uniqid = "USUBJID",
#'     dptvar = "RACE/RACEN",
#'     pctdisp = "TRT"
#'   )
#'
mcatstat <- function(datain = NULL,
                     a_subset = NA_character_,
                     denom_subset = NA_character_,
                     uniqid = "USUBJID",
                     dptvar = NULL,
                     pctdisp = "TRT",
                     miss_catyn = "N",
                     miss_catlabel = "Missing",
                     cum_ctyn = "N",
                     total_catyn = "N",
                     total_catlabel = "Total",
                     dptvarn = 1,
                     pctsyn = "Y",
                     sigdec = 2,
                     denomyn = "N",
                     sparseyn = "N",
                     sparsebyvalyn = "N",
                     return_zero = "N") {
  if (nrow(datain) == 0) {
    return(datain)
  }
  stopifnot("uniqid should exist in data or be ALLCT" = all(uniqid %in% c(names(datain), "ALLCT")))
  # Identify by groups if exists
  BYVAR <- var_start(datain, "BYVAR")
  # Identify subgroups if exists
  SUBGRP <- var_start(datain, "SUBGRP")
  SUBGRPN <- var_start(datain, "SUBGRPN")
  BYVARN <- var_start(datain, "BYVARN")
  dptvars <- sep_var_order(dptvar)
  # Process unique ID variable (if passed as "ALLCT")
  # If unique ID is ALLCT, use all rows instead of unique subjects
  if (all(uniqid == "ALLCT")) {
    datain <- datain |>
      mutate(ALLCT = row_number())
  }
  # Process DPT variable
  data_pro <- datain |>
    create_grpvars(
      dptvars$vars,
      dptvars$order,
      "DPTVAL",
      ifelse(cum_ctyn == "Y", "N", total_catyn),
      totlabel = total_catlabel
    ) |>
    rename(all_of(c("DPTVAL" = "DPTVAL1", "DPTVALN" = "DPTVAL1N"))) |>
    mutate(
      DPTVAL = as.character(.data[["DPTVAL"]]),
      DPTVAL = ifelse(
        str_squish(.data[["DPTVAL"]]) == "" | is.na(.data[["DPTVAL"]]),
        miss_catlabel,
        .data[["DPTVAL"]]
      )
    )
  # Apply subsets to get num and denom data:
  dflist <- map(list(a_subset, denom_subset), \(s) {
    if (!is.na(s) && str_squish(s) != "") {
      filter(data_pro, !!!parse_exprs(s))
    } else {
      data_pro
    }
  })
  data_num <- dflist[[1]]
  data_denom <- dflist[[2]]
  # If missing categories to be included
  if (miss_catyn == "N") {
    data_num <- data_num |>
      filter(.data[["DPTVAL"]] != miss_catlabel)
  }
  # Set groups by Treatment, Sub,By if any to use for counts
  # Get N count as variable FREQ:
  countgrp <- c(
    var_start(data_num, "TRTVAR"), SUBGRP, SUBGRPN, BYVAR,
    BYVARN, "DPTVAL", "DPTVALN"
  )
  # If a_subset returns NONE and equired to return 0 count row:
  if (nrow(data_num) < 1 && return_zero == "Y") {
    counts <- data_pro |>
      group_by(across(any_of(countgrp))) |>
      summarise(FREQ = 0) |>
      ungroup()
  } else {
    # Else proceed to calculate count and percentage
    if (nrow(data_num) < 1 || nrow(data_denom) < 1) {
      return(data.frame())
    }
    # Get count dataset and sparse categories
    counts <- data_num |>
      group_by(across(any_of(countgrp))) |>
      summarise(FREQ = n_distinct(across(any_of(uniqid)))) |>
      ungroup()
    # Sparse categories (within by groups for sparseyn)
    # sparsebyvalyn will also impute for 'by' categories
    if (sparseyn == "Y" || (sparsebyvalyn == "Y" && length(BYVAR) > 0)) {
      data_sparse <- data_pro
    } else {
      data_sparse <- counts
    }
    counts <- counts |>
      sparse_vals(
        data_sparse = data_sparse,
        sparseyn = "Y",
        sparsebyvalyn = "N",
        BYVAR,
        SUBGRP,
        BYVARN,
        SUBGRPN
      ) |>
      sparse_vals(
        data_sparse = data_sparse,
        sparseyn = "N",
        sparsebyvalyn = sparsebyvalyn,
        BYVAR,
        SUBGRP,
        BYVARN,
        SUBGRPN
      )
    # If cumulative count is required then
    if (cum_ctyn == "Y") {
      counts <- counts |>
        group_by(across(any_of(c("TRTVAR", BYVAR, SUBGRP)))) |>
        arrange(.data[["DPTVALN"]], .by_group = TRUE) |>
        mutate(FREQ = cumsum(.data[["FREQ"]])) |>
        ungroup()
    }
  }
  # Calculate denominator/pct and add requisite variables for standard display processing:
  df <- counts |>
    calc_denom(
      data_denom,
      uniqid,
      pctdisp,
      pctsyn,
      denomyn,
      sigdec,
      BYVAR,
      SUBGRP
    )
  df <- df |>
    mutate(
      DPTVAR = dptvars$vars, XVAR = .data[["DPTVAL"]], DPTVARN = dptvarn, CN = "C"
    ) |>
    select(any_of(c(BYVAR, "TRTVAR", SUBGRP, "DPTVAR", "DPTVAL", "CVALUE")), everything())
  
  message("mcatstat success")
  
  df
}

#' Caclulate denominator and oercentage for mcatstat
#'
#' @param counts Dataframe containing counts (FREQ) by category
#' @param data_denom Dataframe subsetted to use as denominator data
#' @param uniqid Variable to calculate unique counts of.
#' Likely values: "USUBJID", "SITEID", "ALLCT"
#' @param pctdisp Method to calculate denominator (for %) by.
#' Possible values: "TRT","VAR","COL","SUBGRP","CAT","NONE","NO","DPTVAR", "BYVARxyN"
#' @param pctsyn Display Percentage Sign in table or not. "Values: "Y"/"N"
#' @param denomyn Display denominator in output column or not. "Values: "Y"/"N"
#' @param BYVAR  By group variables assigned. eg c("BYVAR1", "BYVAR2")
#' @param SUBGRP Subgroup variables assigned. eg c("SUBGRPVAR1", "SUBGRPVAR2")
#'
#' @return A dataframe containing denominator and/or percentage values
#'
#' @noRd
calc_denom <- function(counts,
                       data_denom,
                       uniqid = "USUBJID",
                       pctdisp = "TRT",
                       pctsyn = "Y",
                       denomyn = "N",
                       sigdec = 2,
                       BYVAR,
                       SUBGRP) {
  # Check Allowable pctdisp values
  stopifnot(
    "Invalid pctdisp" =
      str_remove(pctdisp, "[[:digit:]]+") %in%
      c("TRT", "VAR", "COL", "SUBGRP", "SGRPN", "CAT", "NONE", "NO", "DPTVAR", "BYVARN")
  )
  # Set denominator values for percentage
  if (pctdisp %in% c("NONE", "NO")) {
    df <- counts |> mutate(CVALUE = as.character(.data[["FREQ"]]))
    # No percentage if pctdisp is NO/NONE
  } else {
    # Identify which variables go towards creating Denominator
    if (pctdisp == "VAR") {
      # If pctdisp = VAR, total percent across all records
      df <- counts |> mutate(DENOMN = nrow(unique(data_denom[uniqid])))
    } else {
      percgrp <- switch(gsub("[[:digit:]]", "", pctdisp),
                        "TRT" = "TRTVAR",
                        "CAT" = c(BYVAR, "DPTVAL"),
                        "COL" = c("TRTVAR", SUBGRP),
                        "SUBGRP" = c("TRTVAR", SUBGRP, BYVAR),
                        "SGRPN" = SUBGRP,
                        "DPTVAR" = c("TRTVAR", SUBGRP, BYVAR, "DPTVAL"),
                        "BYVARN" = c("TRTVAR", paste0("BYVAR", str_to_vec(
                          str_extract(pctdisp, "[[:digit:]]+"), ""
                        )))
      ) |> intersect(names(data_denom))
      # Get denominator count per above variables
      df <- data_denom |>
        group_by(across(all_of(percgrp))) |>
        summarise(DENOMN = n_distinct(across(any_of(uniqid)))) |>
        inner_join(counts, by = percgrp, multiple = "all")
    }
    
    # Calculate percentage as PCT and concatenate as CVALUE
    p <- ifelse(pctsyn == "N", "", "%") # nolint
    df <- df |>
      mutate(
        PCT = (.data[["FREQ"]] * 100) / DENOMN,
        CPCT = round_f(.data[["PCT"]], sigdec)
      )
    if (denomyn == "Y") {
      cstat <- "{FREQ}/{DENOMN} ({CPCT}{p})"
    } else {
      cstat <- "{FREQ} ({CPCT}{p})"
    }
    df <- df |>
      mutate(CVALUE = ifelse(FREQ == 0, "0", glue(cstat)))
  }
  return(df |> ungroup())
}
