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
#' Pre-Process data for Bar Plot
#'
#' @param analysis_subset Subset conditions for analysis of dependent variable
#' Applicable only to numerator calculation for %
#' @param overall_subset Subset conditions for overall data.
#' @param denom_subset Subset condition to be applied to data set for
#' calculating denominator.
#' @param xvar Categorical Analysis variable for X axis
#' @param yvar Y axis variable/statistic. Possible Values: "FREQ"/"PCT"
#' @param pctdisp Method to calculate denominator (for %) by
#' Possible values: "TRT","VAR","COL","SUBGRP","CAT","NONE","NO","DPTVAR"
#' @inheritParams process_vx_scatter_data
#'
#' @details
#' \itemize{
#' \item Subset Processing
#'      Applying population subset selected
#'      Applying denominator/overall subset condition passed by the user
#'      Applying analysis/numerator subset condition passed by the user.
#' \item pctdisp has possible values for method to get denominator to calculate
#'  percentage, passed to `mcatstat()`. The commonly passed value for vaccine
#'  bar plot is:
#'  DPTVAR: Percentage within each Treatment-By group(s)-Subgroup(s)-dptvar
#'  combination.
#' }
#' @return mcatstat dataset as data frame.
#' @export
#'
#' @examples
#' data(vx_bar_data)
#'
#' process_vx_bar_plot(
#'   dataset_adsl = vx_bar_data$adsl,
#'   adsl_subset = "SAFFL=='Y'",
#'   dataset_analysis = vx_bar_data$adfacevd,
#'   analysis_subset = "ATPTN <= 14 & toupper(FAOBJ) == 'PAIN AT INJECTION SITE' &
#'  !(AVAL %in% c(0, 0.5)) & FATESTCD != 'OCCUR' & !is.na(AVAL)",
#'   denom_subset = "ATPTN <= 14 & toupper(FAOBJ) == 'PAIN AT INJECTION SITE' &
#'  !(AVAL %in% c(0, 0.5))",
#'   overall_subset = NA,
#'   split_by = "SEX",
#'   trtvar = "TRT01A",
#'   trtsort = "TRT01AN",
#'   xvar = "ATPTN",
#'   yvar = "PCT",
#'   pctdisp = "DPTVAR"
#' )
#'
process_vx_bar_plot <- function(dataset_adsl,
                                adsl_subset = "SAFFL=='Y'",
                                dataset_analysis,
                                analysis_subset = NA_character_,
                                overall_subset = NA_character_,
                                denom_subset = NA_character_,
                                split_by = NA_character_,
                                trtvar = "TRT01A",
                                trtsort = "TRT01AN",
                                xvar = "ATPTN",
                                yvar = "PCT",
                                pctdisp = "DPTVAR",
                                legendbign = "Y") {
  stopifnot(is.data.frame(dataset_adsl))
  stopifnot(is.data.frame(dataset_analysis))
  stopifnot(nrow(dataset_adsl) > 0)
  stopifnot(nrow(dataset_analysis) > 0)
  stopifnot(trtvar %in% toupper(names(dataset_adsl)))
  stopifnot("AVAL" %in% toupper(names(dataset_analysis)))
  
  if (!is.na(split_by) && str_squish(split_by) != "") {
    stopifnot(all(str_to_vec(split_by) %in% toupper(names(dataset_adsl))))
  }
  
  adsl_out <- adsl_merge(
    dataset_adsl,
    adsl_subset,
    dataset_analysis
  )
  
  mentry_out <- mentry(
    datain = adsl_out,
    subset = overall_subset,
    pop_fil = "Overall Population",
    subgrpvar = str_remove_all(split_by, " "),
    trtvar = trtvar,
    trtsort = trtsort,
    add_grpmiss = "N"
  )
  
  mcatstat_out <- mcatstat(
    datain = mentry_out,
    a_subset = analysis_subset,
    denom_subset = denom_subset,
    uniqid = "USUBJID",
    dptvar = xvar,
    pctdisp = pctdisp,
    sparseyn = "N"
  ) |>
    mutate(
      YVAR = as.numeric(.data[[yvar]]),
      XVAR = factor(XVAR,
                    levels = unique(XVAR[order(DPTVALN)])
      )
    )
  plotdata <- plot_title_nsubj(
    mentry_out,
    mcatstat_out,
    var_start(mcatstat_out, "SUBGRP")
  ) |>
    plot_display_bign(mentry_out, bignyn = legendbign)
  
  return(plotdata)
}
