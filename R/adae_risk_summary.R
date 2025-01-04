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
#' ADAE Summary with Risk Statistics
#'
#' @inheritParams risk_stat
#' @param hterm High Level Adverse Event term variable, used for analysis
#' @param lterm Low Level Adverse Event term variable, used for analysis
#' @param risklabels List containing labels for table with elements: risk, riskci, p, low, up, lowup
#' @param sum_row To show summary/any term row or not. 'Y'/'N'
#' @param sum_row_label Label for Summary Row to be displayed, if Y.
#' @param sigdec_cat Number of decimal places for % displayed in output
#'
#' @return Data frame to be displayed with risk/counts of higher and lower AE terms
#' @export
#'
#' @examples
#' data(adae)
#'
#' ae_pre_process <- ae_pre_processor(
#'   datain = adae,
#'   ae_filter = "Any Event",
#'   obs_residual = 0,
#'   fmq_data = FMQ_Consolidated_List
#' )
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
#'
#' ae_risk <- ae_entry |>
#'   adae_risk_summary(
#'     a_subset = ae_pre_process[["a_subset"]],
#'     summary_by = "Patients",
#'     hterm = "AEBODSYS",
#'     lterm = "AEDECOD",
#'     ctrlgrp = "Placebo",
#'     trtgrp = "Xanomeline Low Dose",
#'     statistics = "Risk Ratio",
#'     alpha = 0.05,
#'     cutoff_where = "PCT > 5",
#'     sort_opt = "Ascending",
#'     sort_var = "Count"
#'   )
#'
#' ae_risk |>
#'   tbl_processor(keepvars = c("Risk Ratio (CI)", "P-value")) |>
#'   tbl_display()
#'
adae_risk_summary <- function(datain,
                              a_subset = NA_character_,
                              summary_by = "Patients",
                              hterm = "AEBODSYS",
                              lterm = "AEDECOD",
                              ctrlgrp,
                              trtgrp,
                              statistics = "Risk Ratio",
                              riskdiff_pct = "N",
                              alpha = 0.05,
                              cutoff_where = NA,
                              sort_opt = "Ascending",
                              sort_var = "Count",
                              sum_row = "N",
                              sum_row_label = "Participants with Any AE",
                              risklabels = tbl_risk_labels(),
                              sigdec_cat = 1,
                              pctsyn = "Y") {
  if (nrow(datain) < 1) {
    return(datain)
  }
  stopifnot(length(ctrlgrp) == 1)
  stopifnot("Invalid Control Group" = ctrlgrp %in% unique(datain[["TRTVAR"]]))
  stopifnot(
    "Invalid Treatment Group" =
      all(str_to_vec(trtgrp, "~~") %in% unique(datain[["TRTVAR"]]))
  )
  stopifnot(
    "Invalid Risk Statistics; specify any one of `Risk Ratio` or `Risk Difference`" =
      tolower(statistics) %in% c("risk ratio", "risk difference")
  )
  stopifnot(
    "`byvar` in `mentry()` cannot be `NA` or ''" =
      "BYVAR1" %in% var_start(datain, "BYVAR")
  )
  # Get low term and apply cutoff
  ae_lsumm <- risk_stat(
    datain = datain,
    a_subset = a_subset,
    summary_by = summary_by,
    eventvar = lterm,
    ctrlgrp = ctrlgrp,
    trtgrp = trtgrp,
    statistics = statistics,
    alpha = alpha,
    cutoff_where = cutoff_where,
    sort_opt = sort_opt,
    sort_var = sort_var,
    riskdiff_pct = riskdiff_pct,
    hoveryn = "N",
    sigdec = sigdec_cat,
    pctsyn = pctsyn
  )
  if (nrow(ae_lsumm) == 0 || ncol(ae_lsumm) == 1) {
    return(ae_lsumm)
  } else {
    ae_lsumm <- ae_lsumm |>
      mutate(DPTVAR = lterm, CN = "C")
  }
  # Apply if CUTOFF exists
  if (!is.na(cutoff_where) && str_detect(cutoff_where, "PCT|FREQ")) {
    datain <- datain |>
      left_join(select(ae_lsumm, all_of(c("BYVAR1", "CUTFL")), {{ lterm }} := "DPTVAL"),
                by = c("BYVAR1", lterm)
      )
    a_subset <- paste(na.omit(c(a_subset, "CUTFL == 'Y'")), collapse = "&")
  }
  # If ANy AE row:
  if (sum_row == "Y") {
    h_terms <- c(hterm, "TIER")
  } else {
    h_terms <- hterm
  }
  ae_hsumm <- map(h_terms, \(term) {
    if (term == "TIER") {
      datain <- datain |>
        select(-starts_with("BYVAR")) |>
        mutate(TIER = sum_row_label)
    }
    risk_stat(
      datain = datain,
      a_subset = a_subset,
      summary_by = summary_by,
      eventvar = term,
      ctrlgrp = ctrlgrp,
      trtgrp = trtgrp,
      statistics = statistics,
      alpha = alpha,
      sort_opt = sort_opt,
      sort_var = sort_var,
      riskdiff_pct = riskdiff_pct,
      hoveryn = "N",
      sigdec = sigdec_cat,
      pctsyn = pctsyn
    ) |>
      mutate(DPTVAR = term, CN = "C")
  }) |>
    set_names(h_terms)
  
  ## retrun empty flextable
  if (nrow(ae_hsumm[[1]]) < 1) {
    return(data.frame())
  }
  list(ae_hsumm[[1]], ae_lsumm) |>
    post_occ_tier(
      riskyn = "Y",
      ctrlgrp = ctrlgrp,
      risklabels = risklabels,
      sum_row = ae_hsumm[["TIER"]]
    ) |>
    mutate(DPTVAR = "TIER")
}

#' Labels for AE risk table
#'
#' @return list of labels
#' @export
tbl_risk_labels <- function() {
  list(
    riskci = "Risk Ratio (CI)",
    p = "P-value",
    risk = "Risk Ratio",
    low = "Lower Limit",
    up = "Upper Limit",
    lowup = "(Lower-Upper)"
  )
}
