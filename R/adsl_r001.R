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
#' Demographic Characteristics Table
#'
#' @param datain Input data from `mentry()` output
#' @param vars Names of `adsl` variables to display (Add `"-S"` to numeric variables),
#' tilde-separated
#' @param stat_vars Statistics to display in table for numeric vars, tilde-separated.
#' @param a_subset Analysis Subset condition; tilde-separated for each variable in `vars`.
#' @param denom_subset Subset condition to be applied to dataset for calculating denominator,
#' tilde-separated for categorical variables within `vars`.
#' @param sigdec_stat Number of base decimal places to retain in output of summary statistic.
#' Applies to mean, min, max, sd etc
#' @param sigdec_cat Number of decimal places for % displayed in output
#' @inheritParams mcatstat
#'
#' @details
#' \itemize{
#' \item `vars` should contain all variables for analysis with `"-S"` added to numeric/summary stat
#' variables, the remaining being for categorical analysis.
#' eg. for `"AGEGR1/AGEGR1N~AGE-S~SEX/SEXN~BMIBL-S"`, `AGEGR1` and `SEX` will be analysed by
#' category and `AGE` and `BMIBL` as summary statistics.
#' \item Argument `stat_vars` should contain names of statistic to apply to all summary analysis
#' variables. `sigdec` applies only to statistical analysis of numeric variables (-S)
#' \item Arguments `pctdisp`, `total_catyn`, `miss_catyn`, `miss_catlabel` apply to all variables
#' under categorical analyses.
#' \item `a_subset` should tilde-separated subset conditions, corresponding to each variable in
#' `vars`. Length should be `1` or number of variables in `vars`.
#' If only `1` condition is given, it will apply to all variables.
#' \item `denom_subset`, commonly used only when `pctdisp` is `"SUBGRP"` or `"DPTVAR"` corresponds
#' to categorical variables within `vars`. eg. for `"AGEGR1~AGE-S~SEX~BMIBL-S"`, there should be `2`
#' tilde-separated conditions like `"!is.na(USUBJID)~!is.na(SITEID)"`.
#' If only `1` condition is given, it will apply to all categorical/discrete variables.
#' }
#'
#'
#' @return `data.frame` to be passed on to `tbl_processor` and `tbl_display`
#' @export
#'
#' @examples
#' data(adsl)
#'
#' mentry_df <- adsl |>
#'   mentry(
#'     subset = NA_character_,
#'     byvar = NA_character_,
#'     trtvar = "TRT01A",
#'     trtsort = "TRT01AN",
#'     subgrpvar = NA_character_,
#'     trttotalyn = "Y",
#'     add_grpmiss = "N",
#'     pop_fil = "SAFFL"
#'   )
#'
#' adsl_sum <- mentry_df |>
#'   adsl_summary(
#'     vars = "AGEGR1/AGEGR1N~AGE-S~SEX/SEXN~RACE/RACEN",
#'     a_subset = "AGE<65~AGE>80~SEX=='F'~NA"
#'   )
#'
#' adsl_sum |>
#'   display_bign_head(mentry_data = mentry_df) |>
#'   tbl_processor(
#'     statlabel = "N~Range~Meansd~Median~Q1Q3",
#'     dptlabel = "Age Group~_NONE_~Sex~Race",
#'     addrowvar = "DPTVAR"
#'   ) |>
#'   tbl_display() |>
#'   flextable::autofit()
#'
#' # Same variable with 2 unique subset conditions
#' adsl_sum <- mentry_df |>
#'   adsl_summary(
#'     vars = "AGEGR1~AGE-S~SEX~SEX~RACE",
#'     a_subset = "AGE<65~AGE>80~SEX=='F'~SEX=='M'~NA"
#'   )
#'
#' adsl_sum |>
#'   display_bign_head(mentry_data = mentry_df) |>
#'   tbl_processor(
#'     statlabel = "N~Range~Meansd~Median~IQR",
#'     dptlabel = "Age Group~_NONE_~Sex1~Sex2~Race",
#'     addrowvar = "DPTVAR"
#'   ) |>
#'   tbl_display() |>
#'   flextable::autofit()
#'
#' # participants in each data analysis set
#' mentry_df |>
#'   adsl_summary(
#'     vars = "ITTFL~SAFFL~DISCONFL",
#'     a_subset = "ITTFL=='Y'~SAFFL=='Y'~DISCONFL=='Y'"
#'   ) |>
#'   display_bign_head(mentry_df) |>
#'   tbl_processor(
#'     statlabel = "N",
#'     dptlabel = "Intent-to-treat (n%)~Safety (n%)~Discontinued (n%)",
#'     disp_value_col = "N"
#'   ) |>
#'   tbl_display() |>
#'   flextable::autofit()
#'
#' # with groups and subgroups
#' mentry_df_grouped <- adsl |>
#'   mentry(
#'     subset = NA_character_,
#'     byvar = "SEX",
#'     trtvar = "TRT01A",
#'     trtsort = "TRT01AN",
#'     subgrpvar = "ETHNIC",
#'     trttotalyn = "N",
#'     add_grpmiss = "N",
#'     sgtotalyn = "Y",
#'     pop_fil = "SAFFL"
#'   )
#'
#' adsl_sum_grouped <- mentry_df_grouped |>
#'   adsl_summary(
#'     vars = "AGEGR1~AGE-S~RACE"
#'   )
#'
#' adsl_sum_grouped |>
#'   display_bign_head(mentry_df_grouped) |>
#'   tbl_processor(
#'     statlabel = "N~Range~Meansd~Median~IQR",
#'     dptlabel = "Age Group~Age~Race",
#'     addrowvar = "DPTVAR"
#'   ) |>
#'   tbl_display(
#'     bylabel = "Sex",
#'   ) |>
#'   flextable::autofit()
#'
#' ## Demographics Characteristics - Full Analysis Set with `pharmaverseadam` datasets
#' \dontrun{
#' library(dplyr)
#' library(tidyr)
#'
#' advs <- pharmaverseadam::advs |>
#'   filter(
#'     .data$PARAMCD %in% c("HEIGHT", "WEIGHT", "BMI"),
#'     .data$ABLFL == "Y"
#'   ) |>
#'   select(all_of(c("USUBJID", "PARAMCD", "AVAL"))) |>
#'   pivot_wider(
#'     id_cols = "USUBJID",
#'     names_from = "PARAMCD",
#'     values_from = "AVAL"
#'   )
#'
#' adsl_vs <- pharmaverseadam::adsl |>
#'   adsl_merge(
#'     dataset_add = advs,
#'     adsl_subset = "toupper(TRT01A) != 'SCREEN FAILURE'"
#'   ) |>
#'   mentry(
#'     trtvar = "TRT01A",
#'     trttotalyn = "Y"
#'   )
#'
#' adsl_vs |>
#'   adsl_summary(
#'     vars = "SEX~AGE-S~AGEGR1~RACE~ETHNIC~HEIGHT-S~WEIGHT-S~BMI-S",
#'     stat_vars = "median(minmax)~mean(sd)"
#'   ) |>
#'   display_bign_head(adsl_vs) |>
#'   tbl_processor(
#'     dptlabel = "Sex, n(%)~Age (Years)~Age Category (Years), n(%)~Race, n(%)~Ethnicity,
#'     n(%)~Height (cm)~Weight (kg)~BMI (kg/m2)",
#'     statlabel = "Median (Min, Max)~Mean (SD)",
#'     addrowvars = "DPTVAR"
#'   ) |>
#'   tbl_display() |>
#'   flextable::autofit()
#' }
#'
adsl_summary <- function(datain,
                         vars,
                         stat_vars = "n~minmaxc~mean(sd)~median~q1q3",
                         pctdisp = "TRT",
                         total_catyn = "N",
                         total_catlabel = "Total",
                         miss_catyn = "N",
                         miss_catlabel = "Missing",
                         pctsyn = "Y",
                         sigdec_stat = 2,
                         sigdec_cat = 2,
                         a_subset = NA_character_,
                         denom_subset = NA_character_,
                         sparseyn = "N",
                         sparsebyvalyn = "N") {
  if (nrow(datain) == 0) {
    return(datain)
  }
  vars <- split_var_types(toupper(str_to_vec(vars)))
  stat_vars <- str_to_vec(stat_vars)
  # mapping `a_subset` to all variables, `denom_subset` to only categorical variables
  map_df <- map_var_subsets(
    vars[["all_vars"]],
    list(a_subset, denom_subset)
  )
  cat_df <- map_df |>
    filter(!.data$vars %in% .env$vars[["num_vars"]])
  num_df <- map_df |>
    filter(.data$vars %in% .env$vars[["num_vars"]])
  # analysis for categorical variables
  datacat <- map(seq_along(cat_df$vars), \(x) {
    if (!str_to_vec(cat_df$vars[x], "/")[1] %in% names(datain)) {
      cat_sums <- data.frame()
    } else {
      cat_sums <- mcatstat(
        datain = datain,
        a_subset = cat_df[["subset1"]][x],
        denom_subset = cat_df[["subset2"]][x],
        dptvar = cat_df[["vars"]][x],
        uniqid = "USUBJID",
        pctdisp = pctdisp,
        total_catyn = total_catyn,
        miss_catyn = miss_catyn,
        miss_catlabel = miss_catlabel,
        dptvarn = cat_df[["ord"]][x],
        sigdec = sigdec_cat,
        pctsyn = pctsyn,
        sparseyn = sparseyn,
        sparsebyvalyn = sparsebyvalyn,
        return_zero = "Y"
      )
    }
    cat_sums
  }) |>
    bind_rows() |>
    select(-any_of(c("XVAR", "FREQ", "PCT", "CPCT")))
  # analysis for numeric variables
  if (length(vars[["num_vars"]]) > 0) {
    datasums <-
      map(
        seq_along(num_df$vars),
        \(x) {
          if (!num_df[["vars"]][x] %in% names(datain)) {
            df <- data.frame()
          } else {
            df <- msumstat(
              datain = datain,
              a_subset = num_df[["subset1"]][x],
              dptvar = num_df[["vars"]][x],
              statvar = stat_vars,
              dptvarn = num_df[["ord"]][x],
              sigdec = sigdec_stat,
              sparsebyvalyn = sparsebyvalyn
            )[["tsum"]]
          }
          df
        }
      ) |>
      bind_rows()
  } else {
    datasums <- NULL
  }
  bind_rows(datacat, datasums)
}

#' Split variables based on type of analysis
#'
#' @param vars Vector of variable names
#'
#' @return list
#' @noRd
#'
split_var_types <- function(vars) {
  num_vars <- vars[stringr::str_which(vars, "-S")]
  
  list(
    num_vars = str_replace_all(num_vars, "-S", ""),
    cat_vars = vars[!vars %in% num_vars],
    all_vars = str_replace_all(vars, "-S", "")
  )
}

#' Map variables with their corresponding subsets
#'
#' @param varlist List of vector/list of variables
#' @param subsetlist List of vector/list of subsets corresponding to vars in `varlist` element.
#'
#' @return `data.frame` with variables and subsets
#' @noRd
#'
map_var_subsets <- function(varlist, subsetlist) {
  sub <- map(seq_along(subsetlist), \(i) {
    var_len <- length(varlist)
    subset <- dplyr::na_if(str_to_vec(subsetlist[[i]]), "NA")
    # repeat subset for all variables if only one subset is given
    if (length(subset) == 1) {
      subset <- rep(subset, var_len)
    }
    stopifnot(
      "Number of subsets should be 1 or equal to number of corresponding variables" =
        var_len == length(subset)
    )
    bind_cols(!!paste0("subset", i) := subset) |>
      mutate(ord = row_number())
  })
  bind_cols(vars = varlist, reduce(sub, left_join, by = "ord"))
}
