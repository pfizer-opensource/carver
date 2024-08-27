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
#' Summary statistics for numeric data variable
#'
#' @param datain Input dataset from `mentry()`
#' @param dptvar Numerical variable for analysis. eg: `"AGE"`, `"HEIGHTBL"`
#' @param a_subset Analysis subset condition specific to this function.
#' @param statvar `Tilde` (`~`)-separated list of statistics to be computed. eg: `"mean~median"`
#' @param sigdec Number of base decimal places to retain in output
#' Applies to mean, min, max etc and `+ 1` for sd
#' @param dptvarn Number to assign as `'DPTVARN'`, used for block sorting when
#' multiple blocks are created to be combined.
#'
#' @details Current available statistics (values for `statvar`) :
#' n (count per group), mean, median, sd (standard deviation), min, max,
#' iqr (interquartile range), var (variance), sum, range ("min, max")
#' meansd ("mean (sd)"), medianrange ("median (range)"),
#' q25/q1 (25 % quantile), q75/q3 (75 % quantile) , p10 (10% quantile), p5, p1,
#' p90, p95, p99, q1q3 ("q25, q75"), whiskerlow, whiskerup (box lower/upper
#' whiskers), outliers (boxplot outliers, tilde-separated output),
#' box = median~q25~q75~whiskerlow~whiskerup~outliers (Tukey's method)
#'
#' @return a list containing 2 elements
#' \itemize{
#' \item `tsum` - Dataset with statistics as rows, for table output
#' \item `gsum` - Dataset with statistics as columns, for plot output
#' }
#' @export
#'
#' @examples
#' data(adsl)
#'
#' adsl_entry <- mentry(
#'   datain = adsl,
#'   subset = "EFFFL=='Y'",
#'   byvar = "SEX",
#'   trtvar = "TRT01A",
#'   trtsort = "TRT01AN",
#'   pop_fil = NA
#' )
#'
#' adsl_sum <- adsl_entry |>
#'   msumstat(
#'     dptvar = "AGE",
#'     a_subset = "BYVAR1 == 'M'",
#'     statvar = "mean",
#'     sigdec = 2
#'   )
#'
#' adsl_sum$tsum
#' adsl_sum$gsum
#'
msumstat <- function(datain = NULL,
                     a_subset = NA_character_,
                     dptvar = NULL,
                     statvar = "",
                     sigdec = 1,
                     dptvarn = 1) {
  # Check if data is present
  stopifnot("No data to analyze" = nrow(datain) != 0)
  # Check that dependent variable exists and convert it to numeric
  stopifnot("Dependent Variable does not Exist" = dptvar %in% names(datain))
  if (!is.numeric(datain[[dptvar]])) {
    datain <- datain |> mutate(across(all_of(dptvar), ~ as.numeric(.)))
  }
  datain <- datain |> filter(!is.na(.data[[dptvar]]))
  # IF analysis subset is given:
  if (!is.na(a_subset) && str_squish(a_subset) != "") {
    datain <- datain |> filter(eval(parse(text = a_subset)))
  }
  # Available and customized statistics
  if (is.null(statvar) || all(statvar == "")) {
    statinput <- c("n", "mean", "min", "median", "max", "sd")
  } else {
    # custom statistics given as input
    statinput <- statvar |>
      str_replace("box", "median~q25~q75~whiskerlow~whiskerup~outliers") |>
      str_to_vec()
  }
  # Define basic statistics to be printed in the output.
  # Creates summary stats functions:
  statinput <- recode(
    tolower(statinput),
    q1 = "q25", q3 = "q75"
  )
  # Get list of functions from summary_functions()
  list_stats <- summary_functions(sigdec)
  # Check that input stat functions exist
  stopifnot("Statistics not in summary_functions()" = all(statinput %in% names(list_stats)))
  list_stats <- list_stats[statinput]
  # Bring two variables for name and values of category and perform analyses
  data_wide <- datain |>
    select(any_of(c(dptvar, "TRTVAR")), starts_with(c("BYVAR", "SUBGRP"))) |>
    group_by(across(any_of(starts_with(c("BYVAR", "TRTVAR", "SUBGRP"))))) |>
    summarise(across(all_of(dptvar), list_stats, .names = "{.fn}")) |>
    mutate(across(where(is.character), ~ replace(., is.na(.), "-"))) |>
    mutate(DPTVAR = dptvar, CN = "N", DPTVARN = dptvarn) |>
    ungroup()

  # Tidy into long dataframe for use in tabular display
  data_long <- data_wide |>
    pivot_longer(all_of(statinput),
      names_to = "DPTVAL",
      values_to = "CVALUE"
    ) |>
    mutate(DPTVALN = as.numeric(fct_inorder(.data[["DPTVAL"]])))

  message("msum success")
  return(list(tsum = data_long, gsum = data_wide))
}
