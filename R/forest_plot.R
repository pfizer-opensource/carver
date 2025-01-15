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
#' Forest Plot - base with errorbars
#'
#' @param datain Input dataset, pre-processed.
#' @param xvar X-axis variable (continuous)
#' @param yvar Y-axis variable (discrete)
#' @param xminvar Min. x value for errorbar (usually lower limit of Confidence Interval)
#' @param xmaxvar Max. x value for errorbar (usually upper limit of Confidence Interval)
#' @param hovervar Variable containing tooltip info (for interactive plot)
#' @param series_var Series Variable (To stratify by).
#' @param series_color Vector of colors for series variable
#' @param xrefline X-intercept value for vertical reference line
#' @param xaxis_pos Position of X axis and title. Values: "top"/"bottom"
#' @param legend_opts Legend styling option, a `list` containing `pos`(position) and
#' `dir` (direction).
#' @param hline_y Horizontal line marking each Y variable to be plotted or not? "Y"/"N"
#' @param axis_opts A `list` of axis specific options, usually retrieved from `plot_axis_opts()`.
#' Following elements - xaxis_label, xaxis_scale, Xlims, Xbrks, xsize, xtsize are utilised and
#' required from list.
#'
#' @return a ggplot of statistic with interval bars.
#' @export
#'
#' @examples
#' data("adae")
#' ae_pre_process <- ae_pre_processor(
#'   datain = adae,
#'   obs_residual = 0
#' )
#' 
#' ae_entry <- mentry(
#'   datain = ae_pre_process$data,
#'   byvar = "AEBODSYS",
#'   trtvar = "TRTA",
#'   trtsort = "TRTAN",
#'   pop_fil = "SAFFL"
#' )
#' 
#' ae_risk <- risk_stat(
#'   datain = ae_entry,
#'   a_subset = ae_pre_process$a_subset,
#'   summary_by = "Patients",
#'   eventvar = "AEDECOD",
#'   ctrlgrp = "Placebo",
#'   trtgrp = "Xanomeline High Dose",
#'   statistics = "Risk Ratio",
#'   alpha = 0.05,
#'   cutoff_where = "PCT > 2",
#'   sort_opt = "Ascending",
#'   sort_var = "Count",
#'   hoveryn = "Y"
#' )
#' forest_plot_base(
#'   ae_risk,
#'   xvar = "RISK",
#'   yvar = "DPTVAL",
#'   xminvar = "RISKCIL",
#'   xmaxvar = "RISKCIU",
#'   hovervar = "HOVER_TEXT",
#'   series_var = "TRTPAIR",
#'   xrefline = 1,
#'   axis_opts = plot_axis_opts(
#'     xaxis_label = "Risk Ratio",
#'     xopts = list(labelsize = 8)
#'   )
#' )
forest_plot_base <- function(datain,
                             xvar = "RISK",
                             yvar = "DPTVAL",
                             xminvar = "RISKCIL",
                             xmaxvar = "RISKCIU",
                             hovervar = "HOVER_RISK",
                             series_var = "TRTPAIR",
                             series_color = c("black", "black"),
                             xrefline = 1,
                             xaxis_pos = "top",
                             legend_opts = list(pos = "bottom", dir = "horizontal"),
                             hline_y = "Y",
                             axis_opts) {
  stopifnot(nrow(datain) != 0)
  gplot <- ggplot(
    datain,
    aes(
      y = .data[[yvar]],
      x = .data[[xvar]],
      xmin = .data[[xminvar]],
      xmax = .data[[xmaxvar]],
      text = .data[[hovervar]],
      group = .data[[series_var]], color = .data[[series_var]]
    )
  ) +
    ggstance::geom_errorbarh(
      height = 0.1,
      position = ggstance::position_dodgev(height = 0.6), linewidth = 0.5
    ) +
    geom_point(
      shape = 22,
      size = 0.6,
      position = ggstance::position_dodgev(height = 0.6)
    ) +
    geom_vline(xintercept = xrefline, linetype = 3) +
    labs(color = NULL) +
    scale_y_discrete() +
    scale_x_continuous(
      name = axis_opts$xaxis_label,
      position = xaxis_pos, trans = axis_opts$xaxis_scale,
      limits = axis_opts$Xlims, breaks = axis_opts$Xbrks
    ) +
    scale_color_manual(values = series_color) +
    theme_cleany(
      xsize = axis_opts$xsize,
      xtsize = axis_opts$xtsize,
      legend_opts = legend_opts
    )
  if (hline_y == "Y") {
    gplot <- gplot +
      geom_hline(
        yintercept = seq_along(unique(datain[[yvar]])),
        linetype = "dotted",
        color = "black",
        linewidth = 0.2,
        alpha = 0.5
      )
  }
  return(gplot)
}

#' Scatter plot to be included within forest plot
#'
#' @inheritParams forest_plot_base
#' @param axis_opts A `list` of axis specific options.
#' Only following elements - xaxis_label, xsize, xtsize are utilised and
#' required from list. eg. list(xaxis_label = "Percentage", xsize = 8, xtsize = 6)
#' @param series_labelvar Series (Treatment) Variable labels for legend, if
#' different from `series_var`. If this is needed, then `series_labelvar`
#' should be a factor variable with levels corresponding to `series_var`, also a factor.
#' @param series_opts Series Variable styling options, a `list` containing
#' `shape`, `color` and `size`.
#'
#' @return a ggplot scatterplot, fit for combining with forest plot.
#' @export
#'
#' @examples
#' data("adae")
#' ae_pre_process <- ae_pre_processor(
#'   datain = adae,
#'   obs_residual = 0
#' )
#' 
#' ae_entry <- mentry(
#'   datain = ae_pre_process$data,
#'   byvar = "AEBODSYS",
#'   trtvar = "TRTA",
#'   trtsort = "TRTAN",
#'   pop_fil = "SAFFL"
#' )
#' 
#' ae_risk <- risk_stat(
#'   datain = ae_entry,
#'   a_subset = ae_pre_process$a_subset,
#'   summary_by = "Patients",
#'   eventvar = "AEDECOD",
#'   ctrlgrp = "Placebo",
#'   trtgrp = "Xanomeline High Dose",
#'   statistics = "Risk Ratio",
#'   alpha = 0.05,
#'   cutoff_where = "PCT > 2",
#'   sort_opt = "Ascending",
#'   sort_var = "Count",
#'   hoveryn = "Y"
#' )
#' forest_plot_scatter(
#'   datain = ae_risk,
#'   xvar = "PCT",
#'   yvar = "DPTVAL",
#'   series_var = "TRTVAR",
#'   series_opts = list(
#'     color = g_seriescol(ae_risk, c("black", "forestgreen"), "TRTVAR"),
#'     shape = g_seriessym(ae_risk, NA, "TRTVAR"),
#'     size = rep(1, 2)
#'   ),
#'   hovervar = "HOVER_TEXT",
#'   xaxis_pos = "top"
#' )
#'
forest_plot_scatter <- function(datain,
                                xvar = "PCT",
                                yvar = "DPTVAL",
                                series_var = "TRTVAR",
                                series_labelvar = series_var,
                                series_opts,
                                hovervar = "HOVER_PCT",
                                xaxis_pos = "top",
                                legend_opts = list(pos = "bottom", dir = "horizontal"),
                                hline_y = "Y",
                                axis_opts = list(
                                  xsize = 8,
                                  xtsize = 6,
                                  xaxis_label = "Percentage"
                                )) {
  stopifnot(nrow(datain) != 0)
  # Legend Labels if based on other variable:
  series_labels <- series_leg_lab(datain, series_var, series_labelvar)
  gplot <- ggplot(
    datain,
    aes(
      x = .data[[xvar]],
      y = .data[[yvar]],
      color = .data[[series_var]],
      shape = .data[[series_var]],
      size = .data[[series_var]],
      text = .data[[hovervar]]
    )
  ) +
    geom_point() +
    scale_color_manual(
      name = "",
      values = series_opts$color,
      labels = series_labels
    ) +
    scale_shape_manual(
      name = "",
      values = series_opts$shape,
      labels = series_labels
    ) +
    scale_size_manual(
      name = "",
      values = series_opts$size,
      labels = series_labels
    ) +
    scale_y_discrete(name = NULL) +
    scale_x_continuous(name = axis_opts$xaxis_label, position = xaxis_pos) +
    theme_std(axis_opts = axis_opts, legend_opts = legend_opts)
  if (hline_y == "Y") {
    gplot <- gplot +
      geom_hline(
        yintercept = seq_along(unique(datain[[yvar]])),
        linetype = "dotted",
        color = "black",
        linewidth = 0.2,
        alpha = 0.5
      )
  }
  return(gplot)
}

#' Display combined Forest Plot
#'
#' @param plot_list Named list of ggplot objects to be combined. Must contain the names
#' "splot" (scatter plot), "fplot" (risk/forest plot) and optionally may include
#' "ptable" (p-values/other statistic) and/or "termtable" (terms/categories)
#' @param rel_widths Relative widths of each subplot - applied to both static and interactive plot
#' @param interactive Return interactive subplot object? Alternatively, grid plot returned.
#' Values: "Y"/"N"
#' @param plot_height Height of plotly output, if specifically required
#' @param xpos Where should X xaxis for `splot` and `fplot` be displayed in interactive plot?
#' Values: "top"/"bottom". Value for static output is decided prior to passing in this function.
#' @param legend_opts Legend styling option, a `list` containing `pos`(position) and
#' `dir` (direction).
#' @return plot_grid object or plotly forest plot object
#' @export
#'
#' @examples
#' data(ae_risk)
#' splot <- forest_plot_scatter(
#'   datain = ae_risk,
#'   xvar = "PCT",
#'   yvar = "DPTVAL",
#'   series_var = "TRTVAR",
#'   series_opts = list(
#'     color = g_seriescol(ae_risk, c("black", "forestgreen"), "TRTVAR"),
#'     shape = g_seriessym(ae_risk, NA, "TRTVAR"),
#'     size = rep(1, 2)
#'   ),
#'   hovervar = "HOVER_TEXT",
#'   xaxis_pos = "top",
#'   legend_opts = list(pos = "bottom", dir = "horizontal"),
#'   axis_opts = list(xsize = 8, xtsize = 6, xaxis_label = "Percentage")
#' )
#' fplot <- forest_plot_base(
#'   ae_risk,
#'   xvar = "RISK",
#'   yvar = "DPTVAL",
#'   xminvar = "RISKCIL",
#'   xmaxvar = "RISKCIU",
#'   hovervar = "HOVER_TEXT",
#'   series_var = "TRTPAIR",
#'   xrefline = 1,
#'   axis_opts = plot_axis_opts(
#'     xaxis_label = "Risk Ratio",
#'     xopts = list(labelsize = 8)
#'   )
#' )
#' forest_display(list(splot = splot, fplot = fplot),
#'   rel_widths = c(0.6, 0.4)
#' )
#' forest_display(list(splot = splot, fplot = fplot),
#'   rel_widths = c(0.6, 0.4),
#'   interactive = "Y"
#' )
#'
forest_display <- function(plot_list,
                           rel_widths = c(0.25, 0.38, 0.27, 0.10),
                           interactive = "N",
                           plot_height = NULL,
                           xpos = "top",
                           legend_opts = list(pos = "bottom", dir = "horizontal")) {
  stopifnot(all(c("splot", "fplot") %in% names(plot_list)))
  stopifnot(
    "rel_widths should be equal to the number of plot columns" =
      !(length(rel_widths) < length(plot_list))
  )
  if (interactive == "Y") {
    combine_plot <- names(plot_list) |>
      map(\(i) {
        ptly <- plot_list[[i]] |>
          as_plotly(
            height = plot_height,
            axis_opts = list(xopts = list(side = xpos), yopts = NULL)
          )
        if (i %in% c("termtable", "ptable")) {
          ptly <- ptly |>
            plotly::style(hoverinfo = "none", textposition = "right", showlegend = FALSE)
        }
        # Fixing legend with parantheses:
        if (i %in% "splot") {
          ptly <- ptly |>
            plotly_rm_par()
        }
        ptly
      })
    combine_plot <- combine_plot |>
      plotly::subplot(
        nrows = 1,
        widths = rel_widths,
        titleX = TRUE,
        margin = 0.003
      ) |>
      plotly_legend(lg_pos = c(0.5, -0.2), dir = "h")
    combine_plot$x$source <- "plot_output"
  } else {
    legpattern <- paste0("guide-box-", trimws(legend_opts$pos))
    legend1 <- cowplot::get_plot_component(plot_list[["splot"]], pattern = legpattern)
    legend2 <- cowplot::get_plot_component(plot_list[["fplot"]], pattern = legpattern)
    plot_list[["splot"]] <- plot_list[["splot"]] + theme(legend.position = "none")
    plot_list[["fplot"]] <- plot_list[["fplot"]] + theme(legend.position = "none")
    # Combine for grid ggplot output
    row1 <- cowplot::plot_grid(
      plotlist = plot_list,
      align = "h",
      nrow = 1,
      rel_widths = rel_widths
    )
    # Separate legend from plots and combine:
    combine_plot <- cowplot::plot_grid(
      row1,
      legend1,
      legend2,
      ncol = 1,
      rel_heights = c(0.85, 0.075, 0.075)
    )
  }
  return(combine_plot)
}
