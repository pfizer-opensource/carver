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
#' Process data for Survival Analysis
#'
#' @inheritParams process_vx_scatter_data
#' @param censor_var Censoring Variable in the input dataset to be used in
#' ph reg model.Default: `"CNSR"`
#' @param censor_val Value within `CNSR` variable to be considered as Censor
#' value.Default: `1`
#' @param time_var Duration variable in the input dataset to be used in
#' proportional hazard regression model. Default: `"AVAL"`
#'
#' @return Data frame with added variables for survival analysis
#' @export
#'
#' @examples
#' data("survival")
#'
#' survival$adsl |>
#'   surv_pre_processor(
#'     dataset_analysis = survival$adtte,
#'     adsl_subset = "RANDFL=='Y'",
#'     analysis_subset = "PARAMCD=='OS' & FASFL=='Y'",
#'     split_by = NA_character_,
#'     trtvar = "TRT01P",
#'     trtsort = "TRT01PN",
#'     censor_var = "CNSR",
#'     censor_val = 1,
#'     time_var = "AVAL"
#'   )
#'
surv_pre_processor <- function(dataset_adsl,
                               dataset_analysis,
                               adsl_subset = "RANDFL=='Y'",
                               analysis_subset = NA_character_,
                               split_by = NA_character_,
                               trtvar = "TRT01P",
                               trtsort = "TRT01PN",
                               censor_var = "CNSR",
                               censor_val = 1,
                               time_var = "AVAL") {
  stopifnot(is.data.frame(dataset_adsl) || is.data.frame(dataset_analysis))
  stopifnot(nrow(dataset_adsl) > 1 || nrow(dataset_analysis) > 1)
  stopifnot(trtvar %in% names(dataset_adsl))
  stopifnot(
    "Invalid Duration variable" = time_var %in% toupper(names(dataset_analysis))
  )
  stopifnot(
    "Please provide a valid Censoring variable" = censor_var %in% toupper(names(dataset_analysis))
  )

  if (!is.na(split_by) && str_squish(split_by) != "") {
    stopifnot(all(str_to_vec(split_by) %in% toupper(names(dataset_adsl))))
  }
  mentry_out <- dataset_adsl |>
    adsl_merge(
      dataset_add = dataset_analysis |>
        mutate(
          timevar = .data[[time_var]],
          cnsrvar = ifelse(.data[[censor_var]] == censor_val, 0, 1)
        ),
      adsl_subset = adsl_subset
    ) |>
    mentry(
      subset = analysis_subset,
      trtvar = trtvar,
      trtsort = trtsort,
      subgrpvar = str_remove_all(split_by, " ")
    )
  plot_display_bign(mentry_out,
    mentry_data = mentry_out,
    bignyn = "N"
  )
}

#' Pairwise Survival Statistics
#'
#' @param datain Input `data.frame`
#'
#' @return Annotation vector
#' @noRd
#'
pairwise_surv_stats <- function(datain) {
  pairs <- combn(sort(unique(datain[["TRTSORT"]])), 2)

  pair_stat <- map_chr(seq_len(ncol(pairs)), \(i) {
    trt_index <- pairs[, i]
    pair_data <- datain |>
      filter(.data[["TRTSORT"]] %in% trt_index)
    trt_pair <- levels(pair_data[["TRTVAR"]])[trt_index] # nolint
    # tidy coxph fit
    summ <-
      survival::coxph(survival::Surv(timevar, cnsrvar) ~ TRTVAR, data = pair_data) |>
      broom::tidy(conf.int = TRUE, exponentiate = TRUE) |>
      filter(row_number() == 1)
    # extract coxph statistics
    pval_2s <- pull(summ, "p.value")
    HR <- round_f(pull(summ, "estimate"), 2)
    cil <- round_f(pull(summ, "conf.low"), 3) # nolint
    ciu <- round_f(pull(summ, "conf.high"), 3) # nolint
    if (HR < 1) {
      pval_1s <- round_f(pval_2s / 2, 4) # nolint
    } else {
      pval_1s <- round_f(1 - (pval_2s / 2), 4) # nolint
    }
    # create legend
    glue(
      "HR ({trt_pair[1]} vs {trt_pair[2]}) = {HR}, 95% CI ({cil}, {ciu}), 2-sided p = {round_f(pval_2s, 4)}, 1-sided p = {pval_1s}" # nolint
    )
  })

  paste0(pair_stat, collapse = "\n")
}
