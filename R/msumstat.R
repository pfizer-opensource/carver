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
#' Applies to mean, min, max, sd etc
#' @param dptvarn Number to assign as `'DPTVARN'`, used for block sorting when
#' multiple blocks are created to be combined.
#' @param sparsebyvalyn Sparse missing categories within by groups. `"Y"/"N"`
#' @param figyn Determine if output is for figure or not `"Y"/"N"`
#'
#' @details Current available statistics (values for `statvar`) :
#' n (count per group), mean, median, sd (standard deviation), min, max,
#' iqr (interquartile range), var (variance), sum, range ("min, max")
#' mean(sd), median(minmax), q25/q1 (25 % quantile), q75/q3 (75 % quantile) , p10 (10% quantile),
#' p5, p1, p90, p95, p99, q1q3 ("q25, q75"), whiskerlow, whiskerup (box lower/upper
#' whiskers), outliers (boxplot outliers, tilde-separated output), geometric mean/sd/CI
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
#'     a_subset = "SEX == 'F'",
#'     statvar = "mean(sd)~median(minmaxc)~q3",
#'     sigdec = "3(2)~2(0)~1",
#'     sparsebyvalyn = "N"
#'   )
#'
#' adsl_sum$tsum
#' adsl_sum$gsum
#'
msumstat <- function(datain = NULL,
                     a_subset = NA_character_,
                     dptvar = NULL,
                     statvar = "",
                     sigdec = "",
                     dptvarn = 1,
                     sparsebyvalyn = "N",
                     figyn = "N") {
  # Check if data is present
  if (nrow(datain) == 0) {
    return(datain)
  }
  # Check that dependent variable exists and convert it to numeric
  stopifnot("Dependent Variable does not Exist" = dptvar %in% names(datain))
  if (!is.numeric(datain[[dptvar]])) {
    datain <- datain |> mutate(across(all_of(dptvar), ~ as.numeric(.)))
  }
  # IF analysis subset is given:
  if (!is.na(a_subset) && str_squish(a_subset) != "") {
    datapro <- datain |> filter(eval(parse(text = a_subset)))
    # Check data exists after subset:
    if (nrow(datapro) == 0) {
      return(datapro)
    }
  } else {
    datapro <- datain
  }
  
  # Available and customized statistics
  if (is.null(statvar) || all(statvar == "")) {
    statinput <- c("n", "mean", "min", "median", "max", "sd")
  } else if (any(str_detect(statvar, "box"))) {
    # custom statistics given as input
    statinput <- statvar |>
      str_replace("box", "median~q25~q75~whiskerlow~whiskerup~outliers") |>
      str_to_vec()
  } else {
    statinput <- str_to_vec(statvar)
  }
  sigdec <- str_to_vec(sigdec)
  # Resolve concatenated statisitcs into simpler ones
  baselist <- parse_stats(tolower(statinput), sigdec)
  # Get list of basic functions from summary_functions()
  list_stats <- summary_functions(names(baselist), unname(baselist))
  # Stat grouping vars:
  countgrp <- c(
    grep("BYVAR", names(datapro), value = TRUE), "TRTVAR",
    grep("SUBGRPVAR", names(datapro), value = TRUE)
  )
  # Bring two variables for name and values of category and perform analyses
  data_wide <- datapro |>
    select(any_of(c(dptvar, countgrp))) |>
    group_by(across(any_of(countgrp))) |>
    summarise(across(all_of(dptvar), list_stats, .names = "{tolower(.fn)}")) |>
    ungroup() |>
    mutate(across(any_of(names(baselist)), ~ ifelse(.x %in% c("Inf", "-Inf"), NA, .x)))
  # If required, sparse empty by groups:
  # Note this only works if by variables exist and for tables:
  BYVAR <- var_start(data_wide, "BYVAR")
  BYVARN <- var_start(data_wide, "BYVARN")
  if (length(BYVAR) > 0 && figyn != "Y") {
    if (sparsebyvalyn == "Y") {
      data_sparse <- datain
    } else {
      data_sparse <- data_wide |> group_by(across(starts_with("BYVAR")))
      BYVAR <- character(0)
      BYVARN <- character(0)
    }
    data_wide <- data_wide |>
      sparse_vals(
        data_sparse = data_sparse,
        sparseyn = "N",
        sparsebyvalyn = "Y",
        BYVAR,
        var_start(data_wide, "SUBGRP"),
        BYVARN,
        var_start(data_wide, "SUBGRPN"),
        fillvar = colnames(data_wide)[!colnames(data_wide) %in% countgrp],
        fill_with = "-"
      )
  }
  data_wide <- data_wide |>
    derv_stats(statinput) |>
    select(any_of(c(countgrp, tolower(statinput)))) |>
    mutate(DPTVAR = dptvar, CN = "N", DPTVARN = dptvarn)
  # Post-sparsing, n/miss/obs should be 0 and not -
  if (any(c("n", "nmiss", "nobs") %in% names(data_wide))) {
    data_wide <- data_wide |>
      mutate(across(any_of(c("n", "nmiss", "nobs")), ~ gsub("^-$", "0", .x)))
  }
  # Tidy into long dataframe for use in tabular display
  data_long <- data_wide |>
    pivot_longer(all_of(tolower(statinput)),
                 names_to = "DPTVAL",
                 values_to = "CVALUE"
    ) |>
    mutate(DPTVALN = as.numeric(fct_inorder(.data[["DPTVAL"]])))
  
  message("msum success")
  return(list(tsum = data_long, gsum = data_wide))
}
