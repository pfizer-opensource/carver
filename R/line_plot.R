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
#' Utility for Line Plot
#'
#' @param datain Input dataset from process_line_plot_data() output.
#' @inheritParams box_plot
#' @param dodge_width Width to dodge points/lines by, IF required.
#'
#' @return plot - Line plot.
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
#' plot <- line_plot(
#'   datain = lineplot_df,
#'   series_var = "TRTVAR",
#'   series_labelvar = "TRTVAR",
#'   series_opts = list(
#'     color = g_seriescol(lineplot_df, NA, "TRTVAR")
#'   ),
#'   axis_opts = plot_axis_opts(
#'     ylinearopts = list(
#'       breaks = c(100, 1000, 10000, 100000),
#'       limits = c(100, 100000)
#'     ),
#'     xaxis_label = "Visit",
#'     yaxis_label = "Geometric Mean Titer"
#'   ),
#'   legend_opts = list(
#'     label = "",
#'     pos = "bottom",
#'     dir = "horizontal"
#'   ),
#'   griddisplay = "N",
#'   plot_title = "Line Plot"
#' )
#'
#' plot
#'
line_plot <- function(datain,
                      series_var = "TRTVAR",
                      series_labelvar = series_var,
                      series_opts,
                      axis_opts = plot_axis_opts(),
                      legend_opts = list(
                        label = "",
                        pos = "bottom",
                        dir = "horizontal"
                      ),
                      griddisplay = "N",
                      plot_title = NULL,
                      dodge_width = NULL) {
  stopifnot(nrow(datain) != 0)
  stopifnot(
    "XVAR, YVAR, series_var and series_labelvar should exist in data" =
      all(c("XVAR", "YVAR", series_var, series_labelvar) %in% names(datain))
  )
  series_labels <- series_leg_lab(datain, series_var, series_labelvar)
  # Creating dataset for plot
  plot <- datain |>
    ggplot(aes(
      x = .data[["XVAR"]],
      y = .data[["YVAR"]],
      group = .data[[series_var]]
    ))
  dodge_width <- as.numeric(dodge_width)
  if (length(dodge_width) > 0 && !is.na(dodge_width)) {
    plot <- plot +
      geom_line(aes(color = .data[[series_var]]), position = position_dodge(dodge_width)) +
      geom_point(aes(
        color = .data[[series_var]], shape = .data[[series_var]],
        size = .data[[series_var]]
      ), position = position_dodge(dodge_width))
  } else {
    plot <- plot +
      geom_line(aes(color = .data[[series_var]])) +
      geom_point(aes(
        color = .data[[series_var]], shape = .data[[series_var]],
        size = .data[[series_var]]
      ))
  }
  plot <- plot +
    labs(
      title = plot_title,
      x = axis_opts$xaxis_label,
      y = axis_opts$yaxis_label,
      fill = legend_opts$label
    ) +
    scale_y_continuous(
      trans = axis_opts$yaxis_scale,
      breaks = axis_opts$Ybrks,
      limits = axis_opts$Ylims
    ) +
    scale_x_discrete(
      breaks = axis_opts$Xbrks,
      limits = axis_opts$Xlims,
      labels = axis_opts$Xticks
    ) +
    theme_std(axis_opts, legend_opts, griddisplay) +
    scale_color_manual(
      name = legend_opts$label,
      values = series_opts$color,
      labels = series_labels
    ) +
    scale_shape_manual(
      name = legend_opts$label,
      values = series_opts$shape,
      labels = series_labels
    ) +
    scale_size_manual(
      name = legend_opts$label,
      values = series_opts$size,
      labels = series_labels
    )
  message("Line Plot Generated")
  plot
}
