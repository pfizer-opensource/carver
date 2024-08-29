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
#' Forest Plot of Adverse Events
#'
#' `ae_forest_plot()` takes arguments for displaying Risk and counts data (from `risk_stat()`)
#' in the form of a forest plot with elements - list of AE terms corresponding to scatter plot,
#' forest of risk statistics and p-values according to treatment pair.
#'
#' @param datain Input data from `risk_stat()` output, to be used for plotting
#' @param series_opts Series Variable styling options, a `list` containing
#' `shape`, `color` and `size`; applicable to percent scatter plot with TRTVAR as series variable
#' @param trtpair_color Series colors for treatment pair statistic
#' (i.e. error bars in risk plot and p-value table)
#' @param axis_opts A `list` of axis specific options retrieved from `plot_axis_opts()`;
#' applied primarily to plot of risk statistics
#' @param term_label X axis label to display above list of AE terms.
#' @param risk_ref X intercept Value to draw vertical reference line in risk plot
#' @param ht_dispyn Display column for Event High Term corresponding to events. Values: "Y"/"N"
#' @param pvalue_dispyn Display column(s) for p-value corresponding to term. Values: "Y"/"N"
#' @param text_size Size of text for the AE terms as well as p-values
#' Used only if `ht_dispyn`and/or `pvalue_dispyn` = Y.
#' @param highlight_sig To highlight points in scatter plot having significant differences reflected
#' as percentage per treatment/event? Values: "Y"/"N"
#' @param pvalue_sig p value cutoff to determine and display significant differences
#' from control treatment, if `highlight_sig` = "Y"
#' @param pairwise Split plot as 1 treatment pair per page? Values: "Y"/"N". If N, all pairs are
#' displayed together.
#' @param terms_perpg Number of AE terms to display per page. If NULL, all terms are shown.
#' @param rel_widths Relative Widths of each sub-plot in the order:
#' High Term (optional)~Percent Scatter Plot~Risk Plot~p value table (optional). Number of values
#' should change per number of subplots, but total should be equal to 1.
#' eg; c(0.25, 0.38, 0.27, 0.10)/"0.25~0.38~0.27~0.1", if `ht_dispyn` and `pvalues_dispyn` = Y.
#' eg. If only `ht_dispyn` =  Y; "0.2~0.45~0.35" or if only `pvalue_dispyn` = Y, c(0.45, 0.35, 0.2)
#' @param interactive Return interactive plot (ggplotly)? Values: "Y"/"N"
#'
#' @details
#' \itemize{
#' \item Treatments with Risk statistic of a statistically significant p-value (below `pvalue_sig`)
#' are highlighted as 'Significantly Higher' or 'Significantly Lower' in the legend,
#' relative to the incidence rate of each term
#' on the control treatment.
#' \item Color, shape and size for each treatment in scatter plot passed in `series_opts` and
#' "TRTTXT" variable for legend labels are recommended to be created from `plot_aes_opts()`
#' \item Recommended values for risk_ref are 0 for Risk Difference and 1 for Risk Ratio plots.
#' \item `axis_opts` applies primarily to risk plot - with only X axis related values being used.
#' Elements `xsize` and `xtsize` alone are applicable to both percent plot and risk plot.
#' }
#'
#'
#' @return : a list of plots/plotly objects.
#' @export
#'
#' @examples
#' data("adae")
#' ae_pre <- ae_pre_processor(
#'   datain = adae,
#'   obs_residual = 0,
#'   fmq_data = NA
#' )
#' ae_entry <- mentry(
#'   datain = ae_pre$data,
#'   subset = NA,
#'   byvar = "AEBODSYS",
#'   trtvar = "TRTA",
#'   trtsort = "TRTAN",
#'   subgrpvar = NA,
#'   trttotalyn = "N",
#'   add_grpmiss = "N",
#'   sgtotalyn = "N",
#'   pop_fil = "SAFFL"
#' )
#' ae_risk_forest <- risk_stat(
#'   datain = ae_entry,
#'   a_subset = ae_pre$a_subset,
#'   summary_by = "Patients",
#'   eventvar = "AEDECOD",
#'   ctrlgrp = "Placebo",
#'   trtgrp = "Xanomeline High Dose",
#'   statistics = "Risk Ratio",
#'   alpha = 0.05,
#'   cutoff = 5,
#'   sort_opt = "Ascending",
#'   sort_var = "Count"
#' ) |>
#'   plot_display_bign(
#'     ae_entry,
#'     bignyn = "Y"
#'   )
#'
#' ae_forest_plot(
#'   datain = ae_risk_forest,
#'   series_opts = plot_aes_opts(
#'     datain = ae_risk_forest,
#'     series_color = c("black", "royalblue2"),
#'     series_size = rep(1, 5)
#'   ),
#'   axis_opts = plot_axis_opts(
#'     xaxis_label = "Risk Ratio",
#'     xopts = list(labelsize = 8)
#'   ),
#'   text_size = 2.4,
#'   term_label = "Reported Term for the Adverse Event",
#'   risk_ref = 1,
#'   highlight_sig = "N"
#' )[[1]]
#'
ae_forest_plot <-
  function(datain,
           series_opts,
           trtpair_color = rep("black", 10),
           axis_opts,
           term_label = "",
           risk_ref = 1,
           ht_dispyn = "N",
           pvalue_dispyn = "Y",
           text_size = 2.4,
           highlight_sig = "N",
           pvalue_sig = 0.05,
           pairwise = "N",
           terms_perpg = NULL,
           rel_widths = c(0.55, 0.35, 0.1),
           interactive = "N") {
    # Common processing for all plots:
    datain <- datain |>
      filter(!is.nan(.data[["RISK"]]), !is.infinite(.data[["RISK"]]))
    # Check risk data exists:
    stopifnot("Input ae_forest_plot data is empty" = nrow(datain) != 0)
    # If axis position not added, default it:
    axis_opts[["xpos"]] <- ifelse(is.null(axis_opts[["xpos"]]), "bottom", axis_opts[["xpos"]])
    # If single pair to be displayed per page
    if (pairwise != "N") {
      pair_list <- split_data_by_var(datain, "TRTPAIR")
    } else {
      pair_list <- list(datain)
    }
    nested_plots <- pair_list |>
      map(\(df) {
        # If num of terms per page specified:
        events <- unique(df$DPTVAL)
        if (!is.null(terms_perpg) && terms_perpg != 0) {
          per_page <- split(
            events,
            rep(seq_along(events),
              each = terms_perpg,
              length.out = length(events)
            )
          )
        } else {
          per_page <- list(events)
        }
        # Create list, running through per page:
        map(
          per_page,
          \(x) {
            # Filter and Convert Terms to factor to retain sorting order in plot
            dat_out <- df |>
              filter(.data[["DPTVAL"]] %in% x) |>
              arrange(desc(row_number())) |>
              mutate(
                DPTVAL = fct_inorder(.data[["DPTVAL"]])
              )
            plot_list <- list()
            ### Table to display Terms:
            if (ht_dispyn == "Y") {
              # Adjacent HT if duplicates, should be blank
              dat_out <- dat_out |>
                mutate(
                  YCAT = ifelse(.data[["BYVAR1"]] == lead(.data[["BYVAR1"]], default = "last"),
                    "", .data[["BYVAR1"]]
                  ),
                  XVAR = "HT"
                )
              plot_list[["termtable"]] <- dat_out |>
                tbl_to_plot(
                  yvar = "DPTVAL",
                  labelvar = "YCAT",
                  text_size = text_size,
                  axis_opts = list(xaxis_label = term_label, xsize = axis_opts$xsize, xtsize = 0)
                ) +
                theme(axis.text.x = element_blank())
            }
            ### ScatterPlot of treatment percentages
            plot_list[["splot"]] <- forest_plot_scatter(
              datain = dat_out,
              series_labelvar = ifelse("TRTTXT" %in% names(dat_out), "TRTTXT", "TRTVAR"),
              series_opts = series_opts,
              axis_opts =
                append(within(axis_opts, rm("xaxis_label")), list(xaxis_label = "Percent")),
              xaxis_pos = axis_opts$xpos
            )
            #### Highlight significantly high or low percentages
            if (highlight_sig == "Y") {
              plot_list[["splot"]] <- plot_list[["splot"]] |>
                ae_forest_hlt_sig(
                  datain = dat_out,
                  pvalue_sig = pvalue_sig,
                  pts_size = unname(series_opts$size[1]) * 2.5
                )
            }
            ### Line/forest plot of risk statistic:
            plot_list[["fplot"]] <- forest_plot_base(
              datain = dat_out,
              series_color = str_to_vec(trtpair_color),
              xrefline = risk_ref,
              axis_opts = axis_opts,
              xaxis_pos = axis_opts$xpos
            )
            ### p-value Table
            if (pvalue_dispyn == "Y") {
              plot_list[["ptable"]] <- dat_out |>
                tbl_to_plot(
                  xvar = "TRTPAIR",
                  yvar = "DPTVAL",
                  labelvar = "PVALUE",
                  colors = str_to_vec(trtpair_color),
                  text_size = text_size,
                  axis_opts =
                    list(xaxis_label = "p-value", xsize = axis_opts$xsize, xtsize = 0)
                ) +
                theme(axis.text.x = element_blank())
            }
            ### Combine plot for display
            plot_list |>
              forest_display(
                rel_widths = as.numeric(str_to_vec(rel_widths)),
                plot_height = max(length(unique(dat_out$DPTVAL)) * 20, 600),
                interactive = interactive,
                xpos = axis_opts$xpos
              )
          }
        )
      })
    # Return single level list
    return(flatten(nested_plots))
  }

#' Highlight Significant p-value points in forest plot
#'
#' @param plotin Input forest scatterplot
#' @param datain Input data containing pct, ctrl_pct and pvalue columns.
#' @param pvalue_sig p value cutoff.
#' @param pts_size Size of points plotted
#'
#' @return plot object with significant points highlighted
#' @noRd
ae_forest_hlt_sig <- function(plotin,
                              datain,
                              pvalue_sig = 0.05,
                              pts_size = 1.5) {
  labels <- c("Significantly Higher", "Significantly Lower")
  ## Color green or red for + and - effects
  hltfill <- setNames(c("red", "green"), labels)
  ## Identifying significant points higher/lower than control:
  hltpts <- datain |>
    filter(.data[["PVALUE"]] < pvalue_sig) |>
    mutate(EFFECT = case_when(
      .data[["PCT"]] > .data[["CTRL_PCT"]] ~ labels[1],
      .data[["PCT"]] < .data[["CTRL_PCT"]] ~ labels[2],
      TRUE ~ NA_character_
    )) |>
    filter(!is.na(.data[["EFFECT"]]))

  plotin +
    geom_point(
      data = hltpts,
      aes(
        x = .data[["PCT"]],
        y = .data[["DPTVAL"]],
        fill = .data[["EFFECT"]]
      ),
      inherit.aes = FALSE,
      shape = 23,
      size = pts_size,
      stroke = 0.2
    ) +
    scale_fill_manual(name = "", values = hltfill)
}
