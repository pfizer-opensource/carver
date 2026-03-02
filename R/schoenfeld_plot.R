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
#' Schoenfeld Plot
#'
#' @param datain input dataset derived from `surv_pre_processor()`
#' @param disp_clmband Display CLM band in graph (`Y/N`) Default: `Y`
#' @param ties_method Method of handling ties in ph reg model statement.
#' Default: `"efron"`. Values: `"efron", "breslow", "exact"` (`"exact"` is not
#' appropriate when the time variable is continuous or in presence of tied
#' events, see more here \link[survival]{coxph}).
#' @param time_unit Time unit to be displayed in the X axis label.
#' Default: "YEARS"
#' @param markersize Size of the marker to be displayed in the plot.
#' Default:3
#' @param marker_symbol Symbol for residual points from the utility
#' `shape_to_sym()`.Default: `"circle"`. Values: `"circle"/"circlefilled"`
#' @param axis_opts A `list` of axis specific options retrieved from
#' `plot_axis_opts()`
#' @param df Degrees of Freedom for the fitted natural spline.
#' @param pvalue_decimal Number of decimals needed for P-value.
#' Default: 4
#' @param pair_id Select the number of pairs required.User can give input as
#' only one set of pairs.For multiple pairs user have to do other subsetting.
#'
#' @return List containing Schoenfeld plots and Cox Regression statistics
#' \itemize{
#' \item pval: p-value
#' \item pair: Treatment pairs
#' \item s_plot: Schoenfeld Plot object
#' }
#' @export
#'
#' @examples
#'
#'
#' data("survival")
#'
#' sh_pre <- surv_pre_processor(
#'   dataset_adsl = survival$adsl,
#'   adsl_subset = "SAFFL=='Y'",
#'   dataset_analysis = survival$adtte,
#'   split_by = NA_character_,
#'   analysis_subset = "PARAMCD=='OS' & FASFL=='Y'",
#'   trtsort = "TRT01PN",
#'   censor_var = "CNSR",
#'   censor_val = 1,
#'   trtvar = "TRT01P",
#'   time_var = "AVAL"
#' )
#'
#' sh_plot <- schoenfeld_plot(
#'   datain = sh_pre,
#'   disp_clmband = "Y",
#'   ties_method = "efron",
#'   time_unit = "Months",
#'   markersize = 1.5,
#'   marker_symbol = shape_to_sym("circle"),
#'   axis_opts = plot_axis_opts(),
#'   pvalue_decimal = 4,
#'   pair_id = "1~2"
#' )
#' sh_plot
#'
schoenfeld_plot <- function(datain,
                            disp_clmband = "Y",
                            ties_method = "efron",
                            time_unit = "Months",
                            markersize = 3,
                            marker_symbol = shape_to_sym("circle"),
                            axis_opts = plot_axis_opts(),
                            df = 2,
                            pvalue_decimal = 4,
                            pair_id = NA_character_) {
  if (nrow(datain) < 2) {
    return(list(list(pval = NULL, pair = NULL, s_plot = empty_plot("No data available")$plot))) # nolint
  }
  # Applying pairwise logic generate treatment pairs & Apply cox model
  if (all(is.na(pair_id))) {
    pairs <- combn(sort(unique(datain[["TRTSORT"]])), 2)
  } else {
    distinct_pair_ids <- sort(as.numeric(unique(str_to_vec((pair_id)))))
    stopifnot("pair_id mismatch with values of TRTSORT from datain" = all(
      distinct_pair_ids %in% sort(unique(datain[["TRTSORT"]]))
    ))
    pairs <- combn(distinct_pair_ids, 2)
  }

  cox_reg_cols <- c("TRTVAR", "TRTSORT", "timevar", "cnsrvar")

  plt_list <- map(seq_len(ncol(pairs)), function(i) {
    trt_index <- pairs[, i]
    trt_pair <- levels(datain[["TRTVAR"]])[trt_index]

    pair_data <- datain |>
      select(all_of(cox_reg_cols)) |>
      filter(.data[["TRTSORT"]] %in% trt_index) |>
      mutate(trt = ifelse(.data[["TRTSORT"]] == pairs[1, i], 0, 1)) |>
      arrange(trt)

    cus_cx_ph <- pair_data |>
      custom_cox_ph(ties_method, df, pvalue_decimal)

    pair_lab <- paste0(trt_pair[1], " vs ", trt_pair[2])

    if (is.null(cus_cx_ph$out)) {
      return(list(
        pval = cus_cx_ph$pval,
        pair = pair_lab,
        s_plot = empty_plot(cus_cx_ph$err)$plot
      ))
    }

    cx_ph <- cus_cx_ph$out |>
      mutate(pair = pair_lab)

    # creating residual plot
    sfp <- ggplot(data = data.frame(cx_ph)) +
      geom_line(
        aes(x = pred.x, y = yhat), # nolint
        color = "black", linetype = "longdash"
      ) +
      labs(
        x = paste0(axis_opts$xaxis_label, " (", str_to_sentence(time_unit), ")"), # nolint
        y = axis_opts$yaxis_label
      ) +
      scale_x_continuous(
        breaks = axis_opts$Xbrks,
        limits = axis_opts$Xlims
      ) +
      geom_point(
        data = cx_ph, aes(x = xx, y = zy, col = zy),
        size = markersize, shape = marker_symbol
      ) +
      scale_color_gradient2(low = "blue", high = "red") +
      theme(
        panel.background = element_rect(fill = "white", colour = "black"),
        legend.position = "none",
        axis.title.x =
          element_text(size = axis_opts$xsize, face = axis_opts$xface),
        axis.title.y =
          element_text(size = axis_opts$ysize, face = axis_opts$yface),
        axis.text.x =
          element_text(size = axis_opts$xtsize, face = axis_opts$xtface),
        axis.text.y =
          element_text(size = axis_opts$ytsize, face = axis_opts$ytface)
      )
    # Display clmband or Not
    if (disp_clmband == "Y") {
      sfp <- sfp +
        geom_ribbon(aes(x = pred.x, ymin = ylow, ymax = yup),
          fill = "lightcyan4", alpha = 0.1
        )
    }

    list(pval = cus_cx_ph$pval, pair = cx_ph$pair, s_plot = sfp)
  })
  # return(plt_list)
}
