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
#' Create box plot - static and interactive output
#'
#' @param datain Input dataset from `msumstat()` output element `$gsum`
#' @param ystat Additional statistic to be plotted as markers. Default: *mean*
#' @param boxfill Whether box has fill color or not. Uses `series_opts$color` as
#' box fill colors if "Y" or as box outline colors if "N". ("Y"/"N")
#' @param box_opts Vector containing:
#' 1. Width of individual boxes in plot and
#' 2. Width of the interval between box-groups of X axis. eg. c(0.9, 0.9)
#' @inheritParams scatter_plot
#'
#' @details This function utilises data from processing steps to create boxplot.
#' 1. Statistics required: In `msumstat()` function prior, statvar should be
#' c("median", "q25", "q75", "sd", "whiskerlow", "whiskerup", ystat) or "box" with
#' ystat representing additional statistic to be calculated/plotted as points.
#' 2. `series_labvar` can be, for eg "TRTTXT" if that variable contains
#' modified labels for series_var (if "TRTVAR") to be shown in legend only
#' 3. `color` , `shape`, `size` elements of `series_opts` should be of the same
#' length as or not less than number of levels/unique values in the variable
#' `series_var` - recommended to get from `plot_aes_opts()`
#' @return a ggplot object of boxplot with given parameters
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
#'   statvar = c(
#'     "mean", "median", "q25", "q75", "whiskerlow",
#'     "whiskerup"
#'   )
#' )[["gsum"]] |>
#'   plot_display_bign(adsl_entry) |>
#'   dplyr::mutate(XVAR = BYVAR1)
#'
#' series_opts <- adsl_sum |>
#'   plot_aes_opts(
#'     series_color = c("red", "gold", "cyan"),
#'     series_shape = c(16, 17, 15),
#'     series_size = c(1, 1, 1)
#'   )
#'
#' box_plot(
#'   datain = adsl_sum,
#'   axis_opts = plot_axis_opts(),
#'   legend_opts = list(
#'     label = "Treatment", pos = "bottom",
#'     dir = "horizontal"
#'   ),
#'   series_opts = series_opts,
#'   plot_title = "Age distribution over Race groups"
#' )
box_plot <- function(datain,
                     series_var = "TRTVAR",
                     series_labelvar = series_var,
                     series_opts,
                     axis_opts,
                     legend_opts = list(
                       label = "", pos = "bottom",
                       dir = "horizontal"
                     ),
                     ystat = "mean",
                     boxfill = "N",
                     box_opts = c(0.7, 0.9),
                     griddisplay = "N",
                     plot_title = NULL) {
  stopifnot(nrow(datain) > 0)
  if (all(c("whiskerlow", "whiskerup") %in% names(datain))) {
    whiskers <- c("whiskerlow", "whiskerup")
  } else {
    whiskers <- c("min", "max")
  }
  # All statistics should be present:
  stopifnot("Expected statistics not found" = all(c(
    ystat, "median", "q25", "q75", whiskers
  ) %in% names(datain)))
  datain <- datain |>
    mutate(across(all_of(c(
      ystat, "median", "q25", "q75", whiskers
    )), as.numeric))
  # Legend Labels if based on other variable:
  series_labels <- series_leg_lab(datain, series_var, series_labelvar)
  g_plot <- ggplot(datain, aes(
    x = .data[["XVAR"]], ymin = .data[[whiskers[1]]], ymax = .data[[whiskers[2]]],
    lower = .data[["q25"]], upper = .data[["q75"]], middle = .data[["median"]]
  ))
  # Ia color filled or outline color:
  if (boxfill == "Y") {
    g_plot <- g_plot +
      aes(fill = .data[[series_var]]) +
      scale_fill_manual(
        name = legend_opts$label,
        values = series_opts$color,
        labels = series_labels
      )
  } else {
    g_plot <- g_plot +
      aes(color = .data[[series_var]]) +
      scale_color_manual(
        name = legend_opts$label,
        values = series_opts$color,
        labels = series_labels
      )
  }
  g_plot <- g_plot +
    geom_boxplot(
      stat = "identity",
      width = box_opts[1],
      position = position_dodge(box_opts[2])
    ) +
    geom_point(
      aes(
        x = XVAR, y = .data[[ystat]],
        group = .data[[series_var]],
        shape = .data[[series_var]],
        size = .data[[series_var]]
      ),
      position = position_dodge(box_opts[2])
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
    ) +
    scale_x_discrete(
      breaks = axis_opts$Xbrks,
      limits = axis_opts$Xlims
    ) +
    scale_y_continuous(
      trans = axis_opts$yaxis_scale,
      breaks = axis_opts$Ybrks,
      limits = axis_opts$Ylims
    ) +
    labs(
      title = plot_title,
      x = axis_opts$xaxis_label, y = axis_opts$yaxis_label
    ) +
    theme_std(axis_opts, legend_opts, griddisplay)
  # Check for existence of outliers in data
  if ("outliers" %in% names(datain)) {
    stat_outliers <- datain |>
      tidyr::separate_rows(outliers, sep = "~") |>
      mutate(outliers = as.numeric(.data[["outliers"]]))
    g_plot <- g_plot +
      geom_point(
        data = stat_outliers,
        mapping = aes(
          x = .data[["XVAR"]], y = .data[["outliers"]],
          group = .data[[series_var]]
        ),
        shape = 21,
        position = position_dodge(box_opts[2])
      )
  }
  message("Box Plot Success")
  return(g_plot)
}
