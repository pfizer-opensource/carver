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
#' Create Scatter Plot
#'
#' @param datain Input `data.frame`.
#' @param axis_opts A `list` of axis specific options retrieved from `plot_axis_opts()`.
#' @param series_var Series (Treatment) Variable.
#' @param series_labelvar Series (Treatment) Variable labels for legend, if
#' different from `series_var`. If this is needed, then `series_labelvar`
#' should be a factor variable with levels corresponding to `series_var`, also a factor.
#' @param series_opts Series Variable styling options, a `list` containing
#' `shape`, `color` and `size`.
#' @param legend_opts Legend styling option, a `list` containing `label`, `pos`(position) and
#' `dir` (direction).
#' @param plot_title Text to use as plot title, if required
#' @param griddisplay Display Grid `(Y/N)`.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Example 1
#'
#' data(adsl)
#'
#' mentry_df <- adsl |>
#'   mentry(
#'     subset = "AGE < 60",
#'     byvar = NA_character_,
#'     trtvar = "TRT01A",
#'     trtsort = "TRT01AN",
#'     subgrpvar = NA_character_,
#'     trttotalyn = "N",
#'     add_grpmiss = "N",
#'     pop_fil = "SAFFL"
#'   ) |>
#'   dplyr::mutate(XVAR = as.integer(factor(USUBJID)), YVAR = AGE)
#' mentry_df |>
#'   scatter_plot(
#'     axis_opts = plot_axis_opts(
#'       xlinearopts = list(
#'         breaks = sort(unique(mentry_df$XVAR)),
#'         labels = sort(unique(mentry_df$USUBJID))
#'       ),
#'       xopts = list(angle = 15)
#'     ),
#'     series_var = "TRTVAR",
#'     series_labelvar = "TRTVAR",
#'     series_opts = list(
#'       shape = c(16, 17, 18),
#'       color = scales::hue_pal()(3),
#'       size = c(2, 2, 3)
#'     ),
#'     legend_opts = list(
#'       label = "Treatment",
#'       pos = "bottom",
#'       dir = "horizontal"
#'     ),
#'     plot_title = "Scatter Plot of Subject vs Age"
#'   )
#'
#' # Example 2
#'
#' iris |>
#'   ### this step is required for `scatter_plot()` to work as it requires `XVAR` and `YVAR` present
#'   ### in the data
#'   mutate(XVAR = Sepal.Length, YVAR = Sepal.Width) |>
#'   scatter_plot(
#'     axis_opts = plot_axis_opts(),
#'     series_var = "Species",
#'     series_labelvar = "Species",
#'     series_opts = list(
#'       shape = c(16, 17, 18),
#'       color = scales::hue_pal()(3),
#'       size = c(2, 2, 3)
#'     ),
#'     legend_opts = list(
#'       label = "Flower Species",
#'       pos = "bottom",
#'       dir = "horizontal"
#'     ),
#'     plot_title = "Generic Scatter plot with Iris data"
#'   )
#'
scatter_plot <-
  function(datain,
           axis_opts = plot_axis_opts(),
           series_var,
           series_labelvar,
           series_opts,
           legend_opts,
           plot_title = "",
           griddisplay = "N") {
    stopifnot(is.data.frame(datain))
    stopifnot(nrow(datain) > 0)
    stopifnot(
      "`XVAR`/`YVAR` not present in data" = "XVAR" %in% names(datain) & "YVAR" %in% names(datain)
    )
    stopifnot(series_var %in% names(datain))
    stopifnot(length(series_opts$shape) == length(series_opts$color))
    stopifnot(length(series_opts$size) == length(series_opts$color))

    legend_label <- legend_opts$label
    series_labels <- series_leg_lab(datain, series_var, series_labelvar)

    g <- ggplot(
      datain,
      aes(
        x = XVAR,
        y = YVAR,
        shape = .data[[series_var]],
        color = .data[[series_var]],
        size = .data[[series_var]]
      )
    ) +
      geom_point(na.rm = TRUE) +
      scale_x_continuous(
        trans = axis_opts$xaxis_scale,
        breaks = axis_opts$Xbrks,
        limits = axis_opts$Xlims,
        labels = axis_opts$Xticks
      ) +
      scale_y_continuous(
        trans = axis_opts$yaxis_scale,
        breaks = axis_opts$Ybrks,
        limits = axis_opts$Ylims,
        labels = axis_opts$Yticks
      ) +
      scale_shape_manual(
        name = legend_label,
        values = series_opts$shape,
        labels = series_labels
      ) +
      scale_color_manual(
        name = legend_label,
        values = series_opts$color,
        labels = series_labels
      ) +
      scale_size_manual(
        name = legend_label,
        values = series_opts$size,
        labels = series_labels
      ) +
      labs(
        title = plot_title,
        x = axis_opts$xaxis_label,
        y = axis_opts$yaxis_label
      ) +
      theme_std(axis_opts, legend_opts, griddisplay)
    g
  }
