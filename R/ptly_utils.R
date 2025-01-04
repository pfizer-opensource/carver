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
#' Legend Positioning and Labeling for an interactive Plot
#'
#' By default, Plotly only creates legend on the right - use to modify position and label
#' accordingly
#'
#' @param fig Plotly or subplot object as input (with ggplot legend position none)
#' @param lg_lab Legend Title Text; if any.
#' @param lg_pos Legend Position. Values: "right","left","top","bottom", c(1,2), c(5,7) etc
#' @param dir Direction of legend. Values: "h", "v", "horizontal", "vertical"
#'
#' @return Plotly object with legend positioned and titled according to input
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' g <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
#'   geom_point() +
#'   theme(legend.position = "none")
#' plotly_legend(plotly::ggplotly(g), "bottom", "Species:")
plotly_legend <- function(fig,
                          lg_pos = "bottom",
                          lg_lab = "",
                          dir = "h") {
  # Check input objects
  stopifnot("plotly" %in% class(fig))
  dir <- recode(dir, "horizontal" = "h", "vertical" = "v")
  stopifnot("dir can only be 'h' or 'v'" = dir %in% c("h", "v"))
  # Assign options by position
  if (length(lg_pos) > 1) {
    opts <- list(lg_pos[1], lg_pos[2], "center", "top", "center", "bottom")
  } else {
    stopifnot(
      "lg_pos must be top, bottom, left, right or vector" =
        lg_pos %in% c("top", "bottom", "right", "left")
    )
    opts <- switch(lg_pos,
                   "bottom" = list(0.3, -0.2, "left", "top", "right", "top"),
                   "top" = list(0.3, 1.1, "left", "bottom", "right", "bottom"),
                   "left" = list(-0.2, 0.8, "right", "top", "right", "bottom"),
                   "right" = list(1, 0.8, "left", "top", "left", "bottom")
    )
  }
  # Legend title as annotation if exists
  if (lg_lab != "") {
    fig <- fig |>
      add_annotations(
        text = lg_lab, xref = "paper", yref = "paper",
        x = opts[[1]], xanchor = opts[[5]],
        y = opts[[2]], yanchor = opts[[6]],
        legendtitle = TRUE, showarrow = FALSE, font = list(size = 10)
      )
  }
  # Legend position and options
  fig <- fig |>
    layout(
      showlegend = TRUE,
      legend = list(
        orientation = dir, x = opts[[1]], y = opts[[2]],
        size = 8, xanchor = opts[[3]], yanchor = opts[[4]], font = list(size = 8)
      )
    )
  return(fig)
}

#' Standard convert ggplot to plotly object
#'
#' @param plot Input ggplot object
#' @param height Height of interactive plot
#' @param width Width of interactive plot
#' @param legend_opts Legend styling option, a `list` containing `label`, `pos`(position) and
#' `dir` (direction).
#' @param axis_opts Axis styling option. Default is a named list
#' `list(xopts = list(showticklabels = TRUE, showline = TRUE, mirror = TRUE), yopts = list(showticklabels = TRUE, showline = TRUE, mirror = TRUE))` # nolint
#' @param hover Tooltip aesthetic
#'
#' @return plotly widget
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' gplot <- adsl[1:100, ] |>
#'   ggplot(aes(x = .data[["RACE"]], y = .data[["AGE"]], text = .data[["USUBJID"]])) +
#'   geom_point()
#' as_plotly(
#'   plot = gplot,
#'   height = 400,
#'   width = 400
#' )
as_plotly <- function(plot,
                      height = NULL,
                      width = NULL,
                      legend_opts = list(label = "", pos = "bottom", dir = "h"),
                      axis_opts =
                        list(
                          xopts = list(
                            showticklabels = TRUE,
                            showline = TRUE,
                            mirror = TRUE
                          ),
                          yopts = list(
                            showticklabels = TRUE,
                            showline = TRUE,
                            mirror = TRUE
                          )
                        ),
                      hover = "text") {
  # Create plotly object
  ggplotly(
    plot + theme(legend.position = "none"),
    tooltip = hover,
    source = "plot_output",
    height = height,
    width = width
  ) |> # Retain X and Y axes with ticks and breaks
    layout(
      xaxis = axis_opts$xopts,
      yaxis = axis_opts$yopts
    ) |> # Legend position and options
    plotly_legend(
      lg_pos = legend_opts$pos,
      lg_lab = legend_opts$label,
      dir = legend_opts$dir
    )
}

#' Function to remove additional parantheses in legend of plotly objects
#'
#' @param ptly Interactive Plot
#'
#' @return Interactive Plot
#'
#' @noRd
plotly_rm_par <- function(ptly) {
  stopifnot("plotly" %in% class(ptly))
  for (j in seq_along(ptly$x$data)) {
    if (!is.null(ptly$x$data[[j]]$name)) {
      ptly$x$data[[j]]$name <-
        sub("\\(", "", sub(",1,NA\\)|,1\\)", "", ptly$x$data[[j]]$name))
    }
  }
  ptly
}
