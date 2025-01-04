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
#' Merge Datasets
#'
#' @param ... Datasets to be merged.
#' @param byvars By variables required to perform merge.
#' @param subset Dataset specific subset conditions as `list`, default is `NULL`.
#' Has to be specified in the same order of datasets to be merged
#'
#' @return A `data.frame`
#' @export
#'
#' @examples
#' dataset_merge(
#'   lab_data$adsl,
#'   lab_data$adlb,
#'   byvars = "STUDYID~USUBJID~SUBJID",
#'   subset = list("SEX=='F'", "PARAMCD == 'L00021S'")
#' )
#'
#' dataset_merge(
#'   lab_data$adsl,
#'   lab_data$adlb,
#'   byvars = "STUDYID~USUBJID~SUBJID",
#'   subset = list("SEX=='F'", NA_character_)
#' )
#'
#' dataset_merge(
#'   lab_data$adsl,
#'   lab_data$adlb,
#'   byvars = "STUDYID~USUBJID~SUBJID",
#'   subset = list(NA_character_, "PARAMCD == 'L00021S'")
#' )
#'
#' dataset_merge(
#'   lab_data$adsl,
#'   lab_data$adlb,
#'   byvars = "STUDYID~USUBJID~SUBJID",
#'   subset = list("USUBJID == 'XYZ1 1003 10031009'", NA_character_)
#' )
#'
#' dataset_merge(
#'   waterfall_plot_data$adrs,
#'   waterfall_plot_data$adtr,
#'   byvars = "STUDYID~USUBJID~TRT01P",
#'   subset = list("PARAMCD == 'BOR_C'", NA_character_)
#' )
#'
#' ## more than 2 datasets
#'
#' dataset_merge(
#'   dplyr::filter(lab_data$adsl, USUBJID == "XYZ1 1003 10031009"),
#'   lab_data$adsl,
#'   lab_data$adlb,
#'   byvars = "STUDYID~USUBJID~SUBJID"
#' )
#'
dataset_merge <- function(..., byvars, subset = NULL) {
  dfs <- list2(...)
  stopifnot("At least two datasets required for merging" = length(dfs) >= 2)
  byvars <- str_to_vec(byvars)
  if (!every(dfs, \(x) all(byvars %in% names(x)))) stop("`byvars` not present")

  if (length(subset) > 0) {
    stopifnot("Length of subsets and datasets should be equal" = length(dfs) == length(subset))
    if (every(subset, is.na)) stop("All subsets cannot be `NA`, use `subset = NULL` instead")
    dfs <- map(seq_along(dfs), \(i) {
      df_sub <- dfs[[i]]
      if (!is.na(subset[[i]])) {
        df_sub <- df_sub |>
          filter(!!!parse_exprs(subset[[i]]))
      }
      df_sub
    })
  }

  df_list <- map(seq_along(dfs), \(x) {
    out <- dfs[[x]]
    if (x < length(dfs)) {
      out <- out |>
        select(all_of(union(byvars, setdiff(
          names(dfs[[x]]), names(dfs[[x + 1]])
        ))))
    }
    out
  })
  reduce(df_list, left_join, byvars)
}

#' Merge adsl dataset with the analysis dataset
#'
#' @param adsl adsl dataset
#' @param adsl_subset population variable subset condition
#' @param dataset_add analysis dataset
#'
#' @return merged dataset
#' @export
#'
#' @examples
#' data(lab_data)
#'
#' adsl_merge(
#'   adsl = lab_data$adsl,
#'   adsl_subset = "SAFFL=='Y'",
#'   dataset_add = lab_data$adlb
#' )
#'
adsl_merge <- function(adsl = NULL, adsl_subset = "", dataset_add = NULL) {
  stopifnot(length(adsl) > 0)
  stopifnot(nrow(adsl) > 0)
  stopifnot(length(dataset_add) > 0)

  if (adsl_subset != "" && !is.na(adsl_subset)) {
    adsl <- adsl |>
      filter(!!!parse_exprs(adsl_subset))
  }

  byvars <- grep("STUDYID|USUBJID|SUBJID", names(dataset_add), value = TRUE)

  adsl |>
    select(all_of(c(byvars, setdiff(names(adsl), names(dataset_add))))) |>
    left_join(dataset_add, by = byvars)
}
