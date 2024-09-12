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
#' ADAE Summary with Risk Statistics
#'
#' @inheritParams risk_stat
#' @param hterm High Level Adverse Event term variable, used for analysis
#' @param lterm Low Level Adverse Event term variable, used for analysis
#'
#' @return List of summarized data frames for Adverse Events based on high and lower term.
#' @noRd
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
#'     cutoff = 5,
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
                              alpha = 0.05,
                              cutoff = 5,
                              sort_opt = "Ascending",
                              sort_var = "Count") {
  stopifnot("Input data is empty" = nrow(datain) > 0)
  stopifnot(length(ctrlgrp) == 1)
  stopifnot("Invalid Control Group" = ctrlgrp %in% unique(datain[["TRTVAR"]]))
  stopifnot(
    "Invalid Treatment Group" =
      all(str_to_vec(trtgrp, "~~") %in% unique(datain[["TRTVAR"]]))
  )
  stopifnot(
    "Invalid Risk Statistics; specify any one of `Risk Ratio` or `Risk Difference`" =
      statistics %in% c("Risk Ratio", "Risk Difference")
  )
  stopifnot(
    "`byvar` in `mentry()` cannot be `NA` or ''" =
      identical(var_start(datain, "BYVAR"), "BYVAR1")
  )
  stopifnot(
    "`byvar` in `mentry()` should be identical to `hterm` in `adae_summary()" =
      identical(unique(datain[["BYVAR1"]]), unique(datain[[hterm]]))
  )

  ae_summ <- map(c(hterm, lterm), \(term) {
    risk_stat(
      datain = datain,
      a_subset = a_subset,
      summary_by = summary_by,
      eventvar = term,
      ctrlgrp = ctrlgrp,
      trtgrp = trtgrp,
      statistics = statistics,
      alpha = alpha,
      cutoff = cutoff,
      sort_opt = sort_opt,
      sort_var = sort_var
    ) |>
      mutate(DPTVAR = term, CN = "C")
  }) |>
    set_names(c("hterm_summ", "lterm_summ"))

  ## retrun empty flextable
  if (nrow(ae_summ[["hterm_summ"]]) < 1) {
    return(data.frame("Note" = "No data available under these conditions"))
  }

  ae_summ |>
    post_occ_tier(
      riskyn = "Y",
      ctrlgrp = ctrlgrp,
      statistics = statistics
    )
}
