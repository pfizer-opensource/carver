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
#' Pre-Process data for Line Plot
#'
#' @inheritParams process_vx_scatter_data
#'
#' @return Dataframe containing analysis values for requisite vaccine line plot statistics
#' @export
#'
#' @examples
#' data("vx_line_data")
#'
#' process_line_plot_data(
#'   dataset_adsl = vx_line_data$adsl,
#'   dataset_analysis = vx_line_data$adva,
#'   adsl_subset = "SAFFL == 'Y'",
#'   analysis_subset = 'PARAMN==2 & TRTARN!=""& (AVISITN %in% c(1,3,4,5,6,7))',
#'   split_by = "SEX",
#'   trtvar = "TRTP",
#'   trtsort = "TRTPN",
#'   xvar = "AVISIT",
#'   yvar = "AVAL"
#' )
#'
process_line_plot_data <-
  function(dataset_adsl,
           dataset_analysis,
           adsl_subset,
           analysis_subset,
           split_by = NA_character_,
           trtvar,
           trtsort,
           xvar = "AVISIT",
           yvar = "AVAL",
           legendbign = "Y") {
    # Checking if datasets are empty or not
    stopifnot("ADSL data is empty" = nrow(dataset_adsl) != 0)
    stopifnot("Analysis data is empty" = nrow(dataset_analysis) != 0)

    if (!is.na(split_by) && str_squish(split_by) != "") {
      stopifnot(all(str_to_vec(split_by) %in% toupper(names(dataset_adsl))))
    }

    # Merge with adsl
    adsl_merged <- adsl_merge(
      dataset_adsl,
      adsl_subset,
      dataset_analysis
    ) |>
      mentry(
        subset = analysis_subset,
        trtvar = trtvar,
        trtsort = trtsort,
        byvar = xvar,
        subgrpvar = str_remove_all(split_by, " "),
        trttotalyn = "N",
        sgtotalyn = "N",
        add_grpmiss = "N",
        pop_fil = NA
      )
    # Creating the final dataset
    final_data <- adsl_merged |>
      msumstat(
        dptvar = yvar,
        statvar = "geommean~geom_lowci~geom_upci",
        sigdec = 2,
        dptvarn = 1
      )
    # N count for graph output and prep for graph
    plot_data <- plot_title_nsubj(
      adsl_merged,
      final_data[["gsum"]],
      var_start(final_data[["gsum"]], "SUBGRP")
    ) |>
      mutate(
        across(all_of(c("geommean", "geom_lowci", "geom_upci")), as.numeric),
        XVAR = fct_reorder(str_to_title(.data[["BYVAR1"]]), .data[["BYVAR1N"]]),
        YVAR = .data[["geommean"]]
      ) |>
      plot_display_bign(mentry_data = adsl_merged, bignyn = legendbign)
    return(plot_data)
  }

#' Utility for Vaccine Line Plot - (Line Plot with error limits).
#' @inheritParams line_plot
#'
#' @return plot_vx -> Vaccine Line Plot
#' @export
#'
#' @examples
#' data("vx_line_data")
#'
#' lineplot_df <- process_line_plot_data(
#'   dataset_adsl = vx_line_data$adsl,
#'   dataset_analysis = vx_line_data$adva,
#'   adsl_subset = "SAFFL == 'Y'",
#'   analysis_subset = 'PARAMN==2 & TRTARN!=""& (AVISITN %in% c(1,3,4,5,6,7))',
#'   trtvar = "TRTP",
#'   trtsort = "TRTPN",
#'   xvar = "AVISIT",
#'   yvar = "AVAL"
#' )
#'
#' vx_plot <- vx_line_plot(
#'   datain = lineplot_df,
#'   series_opts = list(color = g_seriescol(lineplot_df, NA, "TRTVAR")),
#'   axis_opts = plot_axis_opts(
#'     ylinearopts = list(
#'       breaks = c(100, 1000, 10000, 100000),
#'       limits = c(100, 100000)
#'     ),
#'     xaxis_label = "Visit",
#'     yaxis_label = "Geometric Mean Titer",
#'     yaxis_scale = "log10"
#'   ),
#'   legend_opts = list(
#'     label = "",
#'     pos = "bottom",
#'     dir = "horizontal"
#'   ),
#'   griddisplay = "N"
#' )
#'
#' vx_plot
#'
vx_line_plot <- function(datain,
                         axis_opts = plot_axis_opts(),
                         series_opts,
                         legend_opts = list(
                           lab = "",
                           pos = "bottom",
                           dir = "horizontal"
                         ),
                         griddisplay = "N") {
  plot_vx <- line_plot(
    datain = datain,
    legend_opts = legend_opts,
    axis_opts = axis_opts,
    series_opts = series_opts,
    series_var = "TRTVAR",
    series_labelvar = ifelse("TRTTXT" %in% names(datain), "TRTTXT", "TRTVAR"),
    griddisplay = griddisplay,
    plot_title = glue("Number of Participants N = {unique(datain$splitN)}")
  ) +
    geom_errorbar(aes(x = XVAR, ymin = geom_lowci, ymax = geom_upci),
      width = .5,
      position = position_dodge(0.03)
    )
  message("VX Line Plot Generated")
  return(plot_vx)
}
