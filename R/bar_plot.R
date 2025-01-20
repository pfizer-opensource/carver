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
#'  Utility for Bar plot.
#'
#' @param datain output dataset from mcat/msumstat data; processed in graphics
#' @param series_opts Series Variable styling options, a `list` containing
#' `color` and `contrast`.
#' @param flip_plot To flip plot axes or not. Values: "Y"/"N"
#' @param bar_pos Position of bars within group. Values: "dodged"/"stacked"
#' @param bar_width Width of individual bars *numeric*
#' @inheritParams scatter_plot
#' @return a ggplot object
#'
#' @details
#' `bar_pos` determines position of bars when there is a grouping variable. If dodged is selected,
#' bars are placed adjacent to each other.
#' `series_opts` should be a list containing the element `color` (to fill bars), and optionally the
#' element `contrast` to outline the bars, per the `series_var` levels.
#'
#' @export
#'
#' @examples
#' data(adsl)
#'
#' adsl_entry <- mentry(
#'   datain = adsl,
#'   subset = "EFFFL=='Y'",
#'   byvar = "RACE",
#'   trtvar = "TRT01A",
#'   trtsort = "TRT01AN",
#'   pop_fil = NA
#' )
#'
#' adsl_sum <- msumstat(
#'   datain = adsl_entry,
#'   dptvar = "AGE",
#'   statvar = "mean",
#'   figyn = "Y"
#' )[["gsum"]] |>
#'   plot_display_bign(adsl_entry) |>
#'   dplyr::mutate(
#'     XVAR = BYVAR1,
#'     YVAR = as.numeric(mean)
#'   )
#' bar_plot(
#'   datain = adsl_sum,
#'   flip_plot = "N",
#'   series_opts = list(
#'     color = c("red", "gold", "cyan")
#'   ),
#'   axis_opts = plot_axis_opts(),
#'   legend_opts = list(
#'     label = "", pos = "bottom",
#'     dir = "horizontal"
#'   ),
#'   series_var = "TRTVAR",
#'   series_labelvar = "TRTTXT",
#'   bar_pos = "dodged",
#'   griddisplay = "N",
#'   plot_title = NULL
#' )
#'
bar_plot <- function(datain,
                     series_var = "TRTVAR",
                     series_labelvar = series_var,
                     series_opts,
                     axis_opts = plot_axis_opts(),
                     legend_opts = list(
                       label = "",
                       pos = "bottom",
                       dir = "horizontal"
                     ),
                     bar_pos = "dodged",
                     bar_width = 0.7,
                     flip_plot = "N",
                     griddisplay = "N",
                     plot_title = NULL) {
  stopifnot(nrow(datain) > 0)
  stopifnot(
    "XVAR and YVAR not in data" =
      all(c("XVAR", "YVAR") %in% names(datain))
  )
  # Remove empty rows
  datain <- datain |>
    mutate(YVAR = as.numeric(.data[["YVAR"]]))

  # Bar plot:
  # Legend Labels if based on other variable:
  series_labels <- series_leg_lab(datain, series_var, series_labelvar)
  g_plot <- datain |>
    ggplot(aes(
      x = .data[["XVAR"]],
      y = .data[["YVAR"]],
      fill = .data[[series_var]],
      group = .data[[series_var]]
    )) +
    scale_fill_manual(
      name = legend_opts$label,
      values = series_opts$color,
      labels = series_labels
    )

  if (bar_pos == "stacked") {
    g_plot <- g_plot +
      geom_bar(stat = "identity", width = bar_width)
  } else {
    g_plot <- g_plot +
      geom_bar(stat = "identity", width = bar_width, position = position_dodge())
  }
  # Bar contrast color, if given:
  if ("contrast" %in% names(series_opts)) {
    g_plot <- g_plot +
      aes(color = .data[[series_var]]) +
      scale_color_manual(
        name = legend_opts$label,
        values = series_opts$contrast,
        labels = series_labels
      )
  }
  # Axes labels and tick labels and theme:
  g_plot <- g_plot +
    scale_x_discrete(
      breaks = axis_opts$Xbrks,
      limits = axis_opts$Xlims
    ) +
    labs(
      title = plot_title,
      x = axis_opts$xaxis_label,
      y = axis_opts$yaxis_label
    ) +
    theme_std(axis_opts, legend_opts, griddisplay)

  # Rotate plot if needed:
  if (flip_plot == "Y") {
    g_plot <- g_plot + coord_flip()
  }
  message("Bar Plot Success")
  return(g_plot)
}
