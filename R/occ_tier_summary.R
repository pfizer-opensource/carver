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
#' Generic Occurrence Summary Tiered Table
#'
#' @inheritParams risk_stat
#' @param datain Input dataset (generally the output from `mentry()`)
#' @param hterm High Level Event term variable, used for analysis
#' @param lterm Low Level Event term variable, used for analysis
#' @param pctdisp Method to calculate denominator (for %) by.
#' Possible values: `"TRT"`, `"VAR"`, `"COL"`, `"SUBGRP"`, `"CAT"`, `"NONE"`, `"NO"`, `"DPTVAR"`,
#' `"BYVARxyN"`
#' @param apply_hrow_cutoff To apply cutoff value to high terms in addition to low term.
#' If set to "Y" same cutoff is applied to remove both  high and low level terms that don't meet
#' the criteria.
#' If set to "N" (default), cutoff is applied only to Lower Level term. The terms that do not fit
#' the criteria are then excluded from the counts for High Level term. This does not happen in case
#' of "N" - all counts are included in high term which is displayed as long as it meets the criteria
#' as well.
#'
#' @return Summarized data frame for Adverse Events based on high and lower terms.
#' @export
#'
#' @examples
#' ae_entry <- ae_pre_process[["data"]] |>
#'   mentry(
#'     subset = NA,
#'     byvar = "AEBODSYS",
#'     trtvar = "TRTA",
#'     trtsort = "TRTAN",
#'     trttotalyn = "N",
#'     add_grpmiss = "N",
#'     sgtotalyn = "N",
#'     pop_fil = "Overall Population"
#'   )
#' output <- occ_tier_summary(
#'   ae_entry,
#'   a_subset = ae_pre_process[["a_subset"]],
#'   summary_by = "Patients",
#'   hterm = "AEBODSYS",
#'   lterm = "AEDECOD",
#'   pctdisp = "TRT",
#'   cutoff = 2,
#'   apply_hrow_cutoff = "N",
#'   sort_opt = "Ascending",
#'   sort_var = "Count"
#' )
#' output |>
#'   tbl_processor() |>
#'   tbl_display()
#'
occ_tier_summary <- function(datain,
                             a_subset = NA_character_,
                             summary_by = "Patients",
                             hterm = "AEBODSYS",
                             lterm = "AEDECOD",
                             pctdisp = "TRT",
                             cutoff = 2,
                             apply_hrow_cutoff = "N",
                             sort_opt = "Ascending",
                             sort_var = "Count") {
  stopifnot("Input data is empty" = nrow(datain) > 0)
  stopifnot("Invalid method to set denominator percentage" = pctdisp %in% c("TRT", "HT", "VAR"))
  stopifnot(
    "`byvar` in `mentry()` cannot be `NA` or ''" =
      identical(var_start(datain, "BYVAR"), "BYVAR1")
  )
  stopifnot(
    "`byvar` in `mentry()` should be identical to `hterm` in `occurrence_summary()" =
      identical(unique(datain[["BYVAR1"]]), unique(datain[[hterm]]))
  )
  rows <- c(var_start(datain, "BYVAR"), "DPTVAL")
  # Lowest Term Calculation
  lcat <- mcatstat(
    datain = datain,
    a_subset = a_subset,
    uniqid = ifelse(summary_by == "Events", "ALLCT", "USUBJID"),
    dptvar = lterm,
    pctdisp = pctdisp
  ) |>
    mutate(across(all_of(c("FREQ", "PCT")), \(x) as.double(x)))
  # Applying Cutoff to lower level term counts/pct
  if (!cutoff %in% c("", NA, 0)) {
    lcat_cut <- lcat |> filter(.data[["PCT"]] > as.numeric(cutoff))
    lcat <- lcat |>
      semi_join(lcat_cut, by = rows)
    if (nrow(lcat) < 1) {
      return(data.frame("Note" = "No low term data available under these conditions"))
    }
    # Rows under cutoff to be excluded from higher level term counts
    if (apply_hrow_cutoff != "Y") {
      datain <- datain |>
        semi_join(rename(lcat_cut, !!lterm := DPTVAL), by = gsub("DPTVAL", lterm, rows))
    }
  }

  # Higher Terms calculation:
  hcat <- map(hterm, \(h_term) {
    h <- mcatstat(
      datain = datain,
      a_subset = a_subset,
      uniqid = ifelse(summary_by == "Events", "ALLCT", "USUBJID"),
      dptvar = h_term,
      pctdisp = pctdisp
    ) |>
      mutate(across(all_of(c("FREQ", "PCT")), \(x) as.double(x)))
    # Apply CUTOFF based on parameter and lower term value
    if (!cutoff %in% c("", NA, 0) && apply_hrow_cutoff == "Y") {
      h <- h |>
        semi_join(h |> filter(.data[["PCT"]] > as.numeric(cutoff)), by = rows)
      if (nrow(h) < 1) {
        return(data.frame("Note" = "No high term data available under these conditions"))
      }
    }
    h
  })
  ### Combined Processing for lower and higher terms
  # Sort Values and Options
  if (sort_opt == "Alphabetical") {
    sort_var <- get_sort_var(sort_opt)
  } else {
    sort_var <- get_sort_var(sort_var)
  }
  ctrlgrp <- get_ctrlgrp(datain)
  comb <- append(hcat, list(lcat))
  # Mapping over each categorical dataset and applying post-processing function to concatenate
  # the dataframes into single output
  map(seq_along(comb), \(i) {
    xout <- comb[[i]] |>
      mutate(
        CTRL_N = ifelse(.data[["TRTVAR"]] == ctrlgrp, .data[["FREQ"]], NA),
        CTRL_PCT = ifelse(.data[["TRTVAR"]] == ctrlgrp, .data[["PCT"]], NA)
      )
    # Sort lowest terms
    if (i == length(comb) && sort_opt == "Alphabetical") {
      sort_var <- c(var_start(xout, "BYVAR"), sort_var)
    }
    ## Order Summary tables
    xout |>
      ord_summ_df(sort_var, sort_opt)
  }) |>
    post_occ_tier(ctrlgrp = ctrlgrp)
}

#' Prepare Occurrence summary for Tabular Display
#'
#' @param occ_summ Grouped Occurrence Summary
#' @inheritParams adae_risk_summary
#'
#' @return Flextable object
#' @noRd
post_occ_tier <-
  function(occ_summ, riskyn = "N", ctrlgrp, statistics = NULL) {
    occ_summ <- occ_summ |>
      setNames(c("hterm_summ", "lterm_summ"))
    final_cts <- occ_summ |>
      bind_rows() |>
      select(-any_of(c("DPTVARN", "DPTVALN"))) |>
      inner_join(ord_by_ht(occ_summ, ctrlgrp), by = c("BYVAR1", "DPTVAL")) |>
      select(-any_of(c("BYVAR1", "BYVAR1N"))) |>
      select(-starts_with("CTRL_")) |>
      mutate(
        DPTVAL = ifelse(.data[["DPTVALN"]] == 0,
          .data[["DPTVAL"]], paste0("\t\t\t", str_to_title(.data[["DPTVAL"]]))
        ),
        DPTVAR = "TIER",
        SUBGRPVARX = paste0("n (%)", strrep(" ", as.numeric(.data[["TRTVAR"]]))),
        SUBGRPVARXN = 1
      )

    if (riskyn == "Y") {
      # Rename variable containing RISK_CI based on type of statistic
      final_cts <- final_cts |>
        filter(!is.nan(.data[["RISK"]]), !is.infinite(.data[["RISK"]])) |>
        mutate(
          !!paste0(statistics, " (CI)") := .data[["RISK_CI"]],
          !!paste0("P-", "value") := .data[["PVALUE"]],
          CVALUE = paste0(.data[["FREQ"]], " (", .data[["PCT"]], "%)")
        )
    }
    final_cts
  }

#' Common sorting for both Risk/No risk cases
#'
#' @param df List of data frames summarized by events.
#' @param ctrlgrp Treatment Control value.
#'
#' @return Data frame
#' @noRd
ord_by_ht <- function(df, ctrlgrp) {
  uniqHT <-
    union(
      unique(pull(
        filter(df[["hterm_summ"]], .data[["TRTVAR"]] == ctrlgrp), "DPTVAL"
      )),
      unique(pull(
        filter(df[["hterm_summ"]], .data[["TRTVAR"]] != ctrlgrp), "DPTVAL"
      ))
    )

  map(names(df), \(x) {
    match_var <- recode(x, "hterm_summ" = "DPTVAL", "lterm_summ" = "BYVAR1")
    df_out <- df[[x]] |>
      select(any_of(c("BYVAR1", "DPTVAL"))) |>
      distinct() |>
      mutate(DPTVARN = match(.data[[match_var]], uniqHT)) |>
      filter(!is.na(.data[["DPTVARN"]]))

    if (x == "hterm_summ") {
      df_out <- df_out |>
        mutate(DPTVALN = 0)
    } else {
      df_out <- df_out |>
        group_by(.data[["DPTVARN"]]) |>
        mutate(DPTVALN = row_number()) |>
        ungroup()
    }
    df_out
  }) |>
    bind_rows()
}

#' Get Control Group
#'
#' @param df Data frame
#'
#' @return Name of control group
#' @noRd
get_ctrlgrp <- function(df) {
  levels(df[["TRTVAR"]])[1]
}
