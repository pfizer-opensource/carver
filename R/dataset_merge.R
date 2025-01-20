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
#' @param type Type of join to perform. Values: "left", "right", "inner", "full", "semi", "anti"
#'
#' @return A `data.frame`
#' @export
#'
#' @examples
#' dataset_merge(
#'   adsl,
#'   adlb,
#'   byvars = "STUDYID~USUBJID~SUBJID",
#'   subset = list("SEX=='F'", "PARAMCD == 'ALT'")
#' )
#'
#' dataset_merge(
#'   adsl,
#'   adlb,
#'   byvars = "STUDYID~USUBJID~SUBJID",
#'   subset = list("SEX=='F'", NA_character_)
#' )
#'
#' dataset_merge(
#'   adsl,
#'   adlb,
#'   byvars = "STUDYID~USUBJID~SUBJID",
#'   subset = list(NA_character_, "PARAMCD == 'ALT'")
#' )
#'
#' dataset_merge(
#'   adsl,
#'   adlb,
#'   byvars = "STUDYID~USUBJID~SUBJID",
#'   subset = list("USUBJID == '01-701-1015'", NA_character_)
#' )
#'
#' ## more than 2 datasets
#'
#' dataset_merge(
#'   dplyr::filter(adsl, USUBJID == "01-701-1015"),
#'   adsl,
#'   adlb,
#'   byvars = "STUDYID~USUBJID~SUBJID"
#' )
#'
dataset_merge <- function(..., byvars, subset = NULL, type = "left") {
  dfs <- rlang::list2(...)
  stopifnot("At least two datasets required for merging" = length(dfs) >= 2)
  stopifnot(
    "Type should be one of left, right, inner, full" =
      type %in% c("left", "right", "inner", "full")
  )

  if (type == "full") {
    warning("For full join, subsets will not work as expected. Consider using adsl_merge() instead")
  }
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
  type <- paste0(type, "_join")
  reduce(df_list, get(type), byvars)
}

#' Merge adsl dataset with the analysis dataset
#'
#' @param adsl adsl dataset
#' @param adsl_subset population variable subset condition
#' @param dataset_add analysis dataset
#' @param byvars Variables to merge the datasets by
#'
#' @return merged dataset
#' @export
#'
#' @examples
#' data("adae")
#' data("adsl")
#' adsl_merge(
#'   adsl = adsl,
#'   adsl_subset = "SAFFL=='Y'",
#'   dataset_add = adae
#' )
#'
adsl_merge <- function(adsl = NULL, adsl_subset = "", dataset_add = NULL, byvars = NULL) {
  stopifnot("Pass an ADSL dataset" = length(adsl) > 0)
  stopifnot("Pass an Analysis Dataset" = length(dataset_add) > 0)
  if (nrow(adsl) == 0 || nrow(dataset_add) == 0) {
    return(data.frame())
  }
  if (is.null(byvars)) {
    byvars <- intersect(colnames(adsl), colnames(dataset_add))
  }

  outdata <- full_join(adsl, dataset_add, by = byvars)
  if (adsl_subset != "" && !is.na(adsl_subset)) {
    outdata <- filter(outdata, !!!parse_exprs(adsl_subset))
  }
  outdata
}
