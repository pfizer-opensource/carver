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
#' Kaplan Meir Plot
#'
#' @param datain Pre-processed input data from `surv_pre_processor()`.
#' @param disp_conf.int Display confidence interval. Must be one of `"Y"` or `"N"`.
#' Default is `"N"`.
#' @param disp_risk.table Display risk table. Must be one of `"Y"` or `"N"`. Default is `"Y"`.
#' @param risktab_stats String of statistics to show in the risk table. Must be one or
#' more of `"n.risk"`, `"cum.event"`, `"cum.censor"`, `"n.event"` and `"n.censor"`. Default is
#' `"n.risk"`. See what each option means here \link[ggsurvfit]{add_risktable}).
#' @param risktab_height A numeric value between `0` and `1` to indicate the proportion of final
#' plot and risk table. Default is `NULL`.
#' @param disp_pair.stat Display paired prop-hazard statistics.  Must be one of `"Y", "N"`.
#' Default is `"Y"`.
#' @param trt_colors String of `hex` colors. Must be of the same length of unique `TRTVAR`.
#' @param axis_opts A `list` of axis specific options retrieved from `plot_axis_opts()`
#' @param legend_opts A `list` of legend specific options. Default is
#' `list(pos = "bottom", dir = "vertical")`.
#' @param time_unit Unit of `timevar` in `datain`. Default is `"Months"`.
#'
#' @return Kaplan-Meir Plot
#' @export
#'
#' @examples
#'
#' ## with `{pharmaverseadam}` test data sets
#' ## run `install.packages("pharmaverseadam")` prior running this example
#' \dontrun{
#' km_df <- pharmaverseadam::adsl |>
#'   surv_pre_processor(
#'     adsl_subset = "SAFFL == 'Y'",
#'     dataset_analysis = pharmaverseadam::adtte_onco,
#'     analysis_subset = "PARAMCD == 'OS'",
#'     trtvar = "TRT01P",
#'     trtsort = NA_character_,
#'     censor_val = 0
#'   )
#'
#' km_df |>
#'   km_plot(
#'     trt_colors = "#F8766D~#00BA38~#619CFF",
#'     disp_conf.int = "Y",
#'     time_unit = "Days",
#'     axis_opts = plot_axis_opts(
#'       xlinearopts = list(breaks = 25),
#'       ylinearopts = list(breaks = 0.1),
#'       xaxis_label = "Overall Survival Time (Days)",
#'       yaxis_label = "Probability of Overall Survival"
#'     )
#'   )
#' }
#'
km_plot <-
  function(datain,
           disp_conf.int = "N",
           disp_risk.table = "Y",
           risktab_stats = "n.risk",
           risktab_height = NULL,
           disp_pair.stat = "Y",
           trt_colors,
           axis_opts = plot_axis_opts(
             xlinearopts = list(breaks = 5),
             ylinearopts = list(breaks = 0.1),
             xaxis_label = "Progression-Free Survival Time (Months)",
             yaxis_label = "Probability of Progression Free Survival"
           ),
           legend_opts = list(
             pos = "bottom",
             dir = "vertical"
           ),
           time_unit = "Months") {
    stopifnot(is.data.frame(datain))
    stopifnot(
      "Variables required for survival analysis not present. Please run `surv_pre_processor()`" =
        all(c("timevar", "cnsrvar") %in% names(datain))
    )
    if (nrow(datain) < 1) {
      return(empty_plot("No data available")[["plot"]])
    }
    plot_data <- datain |>
      select(all_of(c(
        "TRTVAR", "TRTSORT", "timevar", "cnsrvar"
      )))
    ## create the `survfit` object
    survfit_km <-
      ggsurvfit::survfit2(
        Surv(timevar, cnsrvar) ~ TRTVAR,
        data = plot_data,
        conf.type = "log-log",
        type = "kaplan-meier"
      )
    ## get legend labels to display
    km_legend <- survfit_km |>
      ggsurvfit::km_legend_txt(time_unit)
    ## get pairwise prop hazard stats using `survival::coxph()`
    pair_stat <- NULL
    if (disp_pair.stat == "Y") {
      pair_stat <- plot_data |>
        pairwise_surv_stats()
    }
    ## Kaplan-Meir plot
    km <- survfit_km |>
      ggsurvfit::ggsurvfit(
        linewidth = 0.8,
        theme = list_modify(
          theme_std(axis_opts, legend_opts),
          legend.background = element_rect(color = "white"),
          legend.key = element_rect(fill = "white"),
          legend.key.size = unit(0.8, "line")
        )
      ) +
      guides(color = guide_legend(override.aes = list(fill = "white"))) +
      add_censor_mark(size = 5, shape = 124)
    ## conditionally display confidence interval
    if (disp_conf.int == "Y") {
      km <- km + add_confidence_interval()
    }
    ## conditionally display risk table
    if (disp_risk.table == "Y") {
      km <- km + add_risktable(
        risktable_stats = str_to_vec(risktab_stats),
        risktable_height = risktab_height,
        theme = theme_risktable_boxed()
      )
    }
    ## prepare km plot for display
    ### plot specific options
    km +
      ggsurvfit::scale_ggsurvfit(
        x_scales =
          list(
            breaks =
              seq(0, max(plot_data[["timevar"]], na.rm = TRUE), by = axis_opts[["Xbrks"]])
          ),
        y_scales =
          list(
            label = \(x) as.character(x),
            breaks = seq(0, 1, by = axis_opts[["Ybrks"]])
          )
      ) +
      scale_color_manual(values = str_to_vec(trt_colors), labels = km_legend) +
      scale_fill_manual(values = str_to_vec(trt_colors), labels = km_legend) +
      annotate(
        "text",
        x = Inf,
        y = Inf,
        hjust = 1.02,
        vjust = 1.2,
        label = pair_stat,
        size = 3
      ) +
      labs(x = axis_opts[["xaxis_label"]], y = axis_opts[["yaxis_label"]])
  }

#' KM Plot Legends
#'
#' @param survfit_km `survfit2()` object
#' @param time_unit unit of `timevar`
#'
#' @return Vector of labels
#' @noRd
#'
km_legend_txt <- function(survfit_km, time_unit) {
  tibble::as_tibble(summary(survfit_km)[["table"]], rownames = "strata") |>
    mutate(across("strata", \(x) str_remove_all(x, "TRTVAR="))) |>
    semi_join(ggsurvfit::tidy_survfit(survfit_km), by = "strata") |>
    mutate(
      txt = glue(
        "{strata} (N={records}, Events={events}, Median={round_f(median, 1)} {time_unit}, 95%CI ({round_f(`0.95LCL`, 1)}, {round_f(`0.95UCL`, 1)}))" # nolint
      ),
      .keep = "none"
    ) |>
    pull()
}
