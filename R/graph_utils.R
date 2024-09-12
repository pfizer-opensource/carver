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
## Transformation Function ###
#' Reverse Log transformation of value to pass to scale options
#'
#' @param base Logarithmic base value.
#'
#' @return Transformation object per given base
#' @export
#'
#' @examples
#' library(tlfcarver)
#' library(ggplot2)
#' ggplot(data = mtcars, mapping = aes(x = mpg, y = hp)) +
#'   geom_point() +
#'   scale_y_continuous(trans = reverselog_trans(10))
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  scales::trans_new(paste0("reverselog-", format(base)), trans, inv,
    scales::log_breaks(base = base),
    domain = c(1e-100, Inf)
  )
}

## Line/Marker/Bar/Box color per Series (Treatment) grouping:##
#' Set Colors to Series Variable to plot
#'
#' Assigns selected colors to levels of a series variable to be used for plotting and legend.
#'
#' @param gdata Data being used to create graph
#' @param series_color Tilde-separated colors corresponding to values in SERIESVAR
#' @param SERIESVAR Series Variable to assign colors to. Examples: "TRTVAR","ETHNIC"
#' Preferably a variable of type factor with distinct levels
#'
#' @return named list of colors per level of grouping variable
#' @export
#'
#' @examples
#' data("adsl")
#' g_seriescol(adsl, "red~cyan~forestgreen~black", "TRT01A")
g_seriescol <- function(gdata,
                        series_color = NA,
                        SERIESVAR = "TRTVAR") {
  if (!all(is.na(series_color))) {
    col_list <- str_to_vec(series_color)
  } else {
    # Standard Colors
    col_list <- c(
      "firebrick2", "blue4", "forestgreen", "gold", "magenta3",
      "aquamarine1", "tan4", "skyblue1", "orchid3", "brown", "pink", "black"
    )
  }

  # If not factor, convert it:
  if (!is.factor(gdata[[SERIESVAR]])) gdata[[SERIESVAR]] <- as.factor(gdata[[SERIESVAR]])

  # Set names as factor levels
  seriescols <- setNames(
    col_list[seq_along(unique(gdata[[SERIESVAR]]))],
    levels(unique(gdata[[SERIESVAR]]))
  )
  return(seriescols)
}

#' Recode Shapes to Numbers
#'
#' @param shape Vector of valid shapes
#'
#' @return Vector of numbers
#' @export
#'
#' @examples
#' shape_to_sym("triangle")
shape_to_sym <- function(shape) {
  recode(
    shape,
    triangle = 2,
    trianglefilled = 17,
    square = 0,
    squarefilled = 15,
    circle = 1,
    circlefilled = 16,
    diamond = 5,
    diamondfilled = 18,
    inverttriangle = 6,
    plus = 3,
    cross = 4,
    asterisk = 8
  )
}

## Marker shapes grouped by Series (Treatment) groups:##
#' Set Shapes to Series Variable to plot
#'
#' Assigns selected point shapes to levels of a series variable to be used for plotting and legend.
#'
#' @param gdata Data being used to create graph
#' @param series_shape Tilde-separated shapes corresponding to values in SERIESVAR
#' @param SERIESVAR Series Variable to assign colors to. Examples: "TRTVAR","ETHNIC"
#' Preferably a variable of type factor with distinct levels
#'
#' @return named list of symbols per level of grouping variable
#' @export
#'
#' @examples
#' data("adsl")
#' g_seriessym(adsl, "triangle~cross~circle~square", "TRT01A")
g_seriessym <- function(gdata,
                        series_shape = NA,
                        SERIESVAR = "TRTVAR") {
  if (!all(is.na(series_shape))) {
    if (is.numeric(series_shape)) {
      shapelist <- series_shape
    } else {
      shapelist <- str_to_vec(series_shape)
      shapelist <- as.numeric(shape_to_sym(shapelist))
    }
  } else {
    # Standard shapes
    shapelist <- c(16, 17, 15, 1, 18, 2, 0, 8, 10, 3, 4, 5)
  }

  # If not factor, convert it:
  if (!is.factor(gdata[[SERIESVAR]])) {
    gdata[[SERIESVAR]] <- as.factor(gdata[[SERIESVAR]])
  }

  # Set names as factor levels
  ptshapes <- setNames(
    shapelist[seq_along(unique(gdata[[SERIESVAR]]))],
    levels(unique(gdata[[SERIESVAR]]))
  )
  return(ptshapes)
}


#' Empty plot with message
#'
#' @param message Required message to be displayed within plot area
#' @param fontsize Set the font size of the message
#'
#' @return a list containing 2 objects
#' \itemize{
#' \item ptly - Interactive empty plot
#' \item plot - Static empty plot
#'  }
#' @export
#'
#' @examples
#' library(tlfcarver)
#' empty_plot()
empty_plot <- function(message = "No data available for these values",
                       fontsize = 8) {
  g_plot <- ggplot() +
    annotate("text",
      x = 1, y = 1, size = fontsize,
      label = message
    ) +
    theme_void()
  fig <- ggplotly(g_plot, height = 200) |>
    layout(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
    )
  return(list(plot = g_plot, ptly = fig))
}


#' Define Axis Specifications
#' @noRd
#'
def_axis_spec <- function(arg, vec, val) {
  if (vec %in% names(arg)) {
    out <- arg[[vec]]
  } else {
    out <- val
  }
  out
}

#' Plot Axes Options
#'
#' @param xlinearopts Linear X axis options for axis breaks and limits.
#' @param ylinearopts Linear Y axis options for axis breaks and limits
#' @param xaxis_scale Scaletype to transform X axis of plot
#' @param yaxis_scale Scaletype to transform Y axis of plot
#' @param xaxis_label Label for X axis
#' @param yaxis_label Label for Y axis
#' @param xopts Options for X axis labels
#' @param yopts Options for Y axis labels
#'
#' @return Combined list of X and Y axis options
#' @export
#'
#' @examples
#' library(tlfcarver)
#'
#' plot_axis_opts(
#'   xlinearopts = list(
#'     breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
#'     limits = c(0.001, 100)
#'   ),
#'   ylinearopts = list(
#'     breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
#'     limits = c(0.001, 1000)
#'   )
#' )
#'
#' plot_axis_opts(
#'   xlinearopts = list(
#'     breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
#'     limits = c(0.001, 100)
#'   ),
#'   ylinearopts = list(
#'     breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
#'     limits = c(0.001, 1000)
#'   ),
#'   xopts = list(
#'     labelsize = 12,
#'     labelface = "plain",
#'     ticksize = 8,
#'     tickface = "plain"
#'   ),
#'   yopts = list(
#'     labelsize = 15,
#'     labelface = "plain",
#'     ticksize = 10,
#'     tickface = "plain"
#'   )
#' )
#'
plot_axis_opts <-
  function(xlinearopts = list(
             breaks = waiver(),
             limits = NULL,
             labels = waiver()
           ),
           ylinearopts = list(
             breaks = waiver(),
             limits = NULL,
             labels = waiver()
           ),
           xaxis_scale = "identity",
           yaxis_scale = "identity",
           xaxis_label = "",
           yaxis_label = "",
           xopts = list(
             labelsize = 12,
             labelface = "plain",
             ticksize = 8,
             tickface = "plain",
             angle = 0
           ),
           yopts = list(
             labelsize = 12,
             labelface = "plain",
             ticksize = 8,
             tickface = "plain",
             angle = 0
           )) {
    stopifnot(is.list(xlinearopts))
    stopifnot(is.list(ylinearopts))

    list(
      Ybrks = def_axis_spec(
        arg = ylinearopts,
        vec = "breaks", val = waiver()
      ),
      Ylims = def_axis_spec(
        arg = ylinearopts,
        vec = "limits", val = NULL
      ),
      Yticks = def_axis_spec(
        arg = ylinearopts,
        vec = "labels", val = waiver()
      ),
      Xbrks = def_axis_spec(
        arg = xlinearopts,
        vec = "breaks", val = waiver()
      ),
      Xlims = def_axis_spec(
        arg = xlinearopts,
        vec = "limits", val = NULL
      ),
      Xticks = def_axis_spec(
        arg = xlinearopts,
        vec = "labels", val = waiver()
      ),
      xsize = def_axis_spec(
        arg = xopts,
        vec = "labelsize", val = 12
      ),
      xface = def_axis_spec(
        arg = xopts,
        vec = "labelface", val = "plain"
      ),
      ysize = def_axis_spec(
        arg = yopts,
        vec = "labelsize", val = 12
      ),
      yface = def_axis_spec(
        arg = yopts,
        vec = "labelface", val = "plain"
      ),
      ytsize = def_axis_spec(
        arg = yopts,
        vec = "ticksize", val = 8
      ),
      ytface = def_axis_spec(
        arg = yopts,
        vec = "tickface", val = "plain"
      ),
      xtsize = def_axis_spec(
        arg = xopts,
        vec = "ticksize", val = 8
      ),
      xtface = def_axis_spec(
        arg = xopts,
        vec = "tickface", val = "plain"
      ),
      xtangle = def_axis_spec(
        arg = xopts,
        vec = "angle", val = 0
      ),
      ytangle = def_axis_spec(
        arg = yopts,
        vec = "angle", val = 0
      ),
      xaxis_scale = xaxis_scale,
      yaxis_scale = yaxis_scale,
      xaxis_label = xaxis_label,
      yaxis_label = yaxis_label
    )
  }

#' Set Series Variable Options for color, shape and size
#'
#' @param datain Input dataset to be plotted
#' @param series_var Series Variable
#' @param series_color Tilde-separated string or vector of colors to be set to
#' levels of `series_var`. Pass NA to get package defaults
#' @param series_shape Tilde-separated string or vector of symbols to be set to
#' levels of `series_var`. Pass NA to get package defaults
#' @param series_size Tilde-separated string or vector of sizes to be set to
#' levels of `series_var`. Pass NA to get package defaults
#' @param series_contrast Tilde-separated string or vector of colors to be set to
#' levels of `series_var`. (Optional)
#'
#' @details The `series_shape` argument can take numeric values such as
#' c(16, 25, 22), "16,25,22" or characters -
#' c("triangle", "square")/"triangle,square"
#'
#' @return a list containing vectors for colors, shapes and sizes matched to levels in
#' Series Variable
#' @export
#'
#' @examples
#' plot_aes_opts(iris,
#'   series_var = "Species",
#'   series_color = NA,
#'   series_shape = "trianglefilled~circlefilled~squarefilled~asterisk",
#'   series_size = c(2, 2, 2, 2),
#'   series_contrast = "black~black~black"
#' )
plot_aes_opts <- function(datain,
                          series_var = "TRTVAR",
                          series_color = NA,
                          series_shape = NA,
                          series_size = rep(1, 10),
                          series_contrast = "") {
  if (!is.numeric(series_size)) series_size <- as.numeric(str_to_vec(series_size))
  if (!is.na(series_var) && series_var != "" && series_var %in% names(datain)) {
    out <- list(
      color = g_seriescol(datain, series_color, series_var),
      shape = g_seriessym(datain, series_shape, series_var),
      size = g_seriessym(datain, series_size, series_var)
    )
    if (all(series_contrast != "")) {
      out$contrast <- g_seriescol(datain, series_contrast, series_var)
    }
  } else {
    datain$aesvar <- "Total"
    out <- list(
      color = unname(g_seriescol(datain, series_color, "aesvar")),
      shape = unname(g_seriessym(datain, series_shape, "aesvar")),
      size = unname(g_seriessym(datain, series_size, "aesvar"))
    )
    if (all(series_contrast != "")) {
      out$contrast <- unname(g_seriescol(datain, series_contrast, "aesvar"))
    }
  }
  return(out)
}

#' Return N count in plot legend (treatment)
#'
#' @param datain Input dataframe for display
#' @param mentry_data Input data from `mentry()`
#' @param bignyn Display count "(N = )" along with Treatment in plot legend? Values: "Y"/"N"
#'
#' @return dataframe with additional variable "TRTTXT" containing the label for Treatment
#' @export
#'
#' @examples
#' data(adsl)
#' adsl_entry <- adsl |>
#'   mentry(
#'     trtvar = "TRT01A",
#'     trtsort = "TRT01AN",
#'     trttotalyn = "N",
#'     add_grpmiss = "N",
#'     pop_fil = "SAFFL"
#'   )
#' msumstat(
#'   adsl_entry,
#'   dptvar = "AGE",
#'   statvar = "meansd"
#' )$gsum |>
#'   plot_display_bign(adsl_entry)
plot_display_bign <- function(datain,
                              mentry_data,
                              bignyn = "Y") {
  stopifnot(nrow(datain) != 0)
  # If TRVAR does not exist, impute it to a Total
  if (!("TRTVAR" %in% names(datain))) {
    datain <- datain |> mutate(TRTVAR = "Total")
  }
  if (bignyn == "Y") {
    grpvar <- c(var_start(datain, "TRTVAR"), var_start(datain, "SUBGRP"))
    stopifnot(nrow(mentry_data) != 0)
    grp <- intersect(grpvar, names(mentry_data))
    if (length(grp) > 0) {
      datain <- add_bigN(datain, mentry_data, grp, "TRTVAR") |>
        rename(any_of(c("TRTTXT" = "TRTVAR_BIGN")))
    } else {
      datain <- datain |>
        mutate(
          TRTTXT =
            paste0(.data[["TRTVAR"]], " (N=", length(unique(mentry_data[["USUBJID"]])), ")")
        )
    }
  } else {
    datain[["TRTTXT"]] <- datain[["TRTVAR"]]
  }
  return(datain)
}

#' Theme for ggplots with only X axis title/ticks
#'
#' When this theme is applies to a plot, results in a white-background, full outer borders, no
#' Y axis label or breaks and center-aligned title.
#'
#' @param xsize Size of X axis label/title text
#' @param xtsize Size of X axis tick labels text
#' @param titlesize  Size of plot title
#' @param legend_opts Legend styling option, a `list` containing `pos`(position) and
#' `dir` (direction).
#' @param lines_color Color to assign to graph panel border as well as axes lines
#'
#' @return A minimal ggplot2 theme without Y axis details
#' @export
#'
#' @examples
#' data(adsl)
#' library(ggplot2)
#' ggplot(
#'   adsl,
#'   aes(
#'     x = .data[["RACE"]], y = .data[["AGE"]],
#'     color = .data[["SEX"]]
#'   )
#' ) +
#'   geom_point() +
#'   theme_cleany(legend_opts = list(pos = "bottom", dir = "horizontal"))
#'
theme_cleany <- function(xsize = 8,
                         xtsize = 6,
                         titlesize = 10,
                         legend_opts = list(pos = "none", dir = "horizontal"),
                         lines_color = "black") {
  theme(
    plot.title = element_text(hjust = 0.1, size = titlesize),
    panel.background = element_blank(),
    panel.border = element_rect(colour = lines_color, fill = NA, linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = xsize),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = xtsize),
    axis.line = element_line(colour = lines_color),
    axis.text.y = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.position = legend_opts$pos,
    legend.direction = legend_opts$dir
  )
}

#' Standard ggplot theme
#'
#' @param axis_opts A `list` of axis specific options retrieved from `plot_axis_opts()`.
#' @param legend_opts Legend styling option, a `list` containing `pos`(position) and
#' `dir` (direction).
#' @param griddisplay Values "Y"/"N" to display grid lines
#'
#' @return Standard ggplot theme for basic figures using axis and legend options
#' @export
#'
#' @examples
#' data(adsl)
#' library(ggplot2)
#' ggplot(
#'   adsl,
#'   aes(
#'     x = .data[["RACE"]], y = .data[["AGE"]],
#'     color = .data[["SEX"]]
#'   )
#' ) +
#'   geom_point() +
#'   theme_std()
theme_std <- function(axis_opts = plot_axis_opts(),
                      legend_opts = list(
                        pos = "bottom",
                        dir = "horizontal"
                      ),
                      griddisplay = "N") {
  t <- theme(
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    plot.title = element_text(hjust = 0.5),
    legend.background = element_rect(linetype = "solid", color = "black"),
    legend.position = legend_opts$pos,
    legend.direction = legend_opts$dir,
    axis.title.x =
      element_text(size = axis_opts$xsize, face = axis_opts$xface),
    axis.title.y =
      element_text(size = axis_opts$ysize, face = axis_opts$yface),
    axis.text.x =
      element_text(size = axis_opts$xtsize, face = axis_opts$xtface, angle = axis_opts$xtangle),
    axis.text.y =
      element_text(size = axis_opts$ytsize, face = axis_opts$ytface, angle = axis_opts$ytangle)
  )
  if (griddisplay == "Y") {
    t <- t + theme(
      panel.grid.major.x = element_line(linewidth = 0.1, color = "grey"),
      panel.grid.major.y = element_line(linewidth = 0.1, color = "grey")
    )
  }
  return(t)
}

#' Calculate plot title N values
#'
#' @param datain Input data to get N count from
#' @param plot_data Plot data to attach N count to
#' @param by Sub group variables to split N count by
#'
#' @return dataframe with variable splitN
#'
#' @noRd
plot_title_nsubj <- function(datain, plot_data, by) {
  adsin_count <- datain |>
    group_by(across(any_of(by))) |>
    summarise(splitN = length(unique(.data[["USUBJID"]]))) |>
    ungroup()
  if (length(by) > 0) {
    plot_data <- plot_data |>
      dplyr::left_join(adsin_count, by = by)
  } else {
    plot_data <- bind_cols(plot_data, adsin_count)
  }
  return(plot_data)
}

#' Convert dataframe into ggplot object table
#'
#' @param datain Input dataframe
#' @param xvar X axis variable, acts as table columns
#' @param yvar Y axis variable, acts as values within columns
#' @param labelvar Label variable
#' @param colorvar Variable to color values by
#' @param colors Vector of colors to be used per `colorvar`
#' @param text_size Size of text printed in 'table'
#' @param axis_opts Axis options, list containing `xsize`, and `xaxis_label` for X axis title;
#' Include `xtsize` element for ticks (column headers).
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(tlfcarver)
#' MPG <- ggplot2::mpg
#' MPG[["cyl"]] <- as.character(MPG[["cyl"]])
#' tbl_to_plot(
#'   MPG,
#'   "cyl",
#'   "manufacturer",
#'   "manufacturer"
#' )
tbl_to_plot <- function(datain,
                        xvar = "XVAR",
                        yvar,
                        labelvar = yvar,
                        colorvar = xvar,
                        colors = rep("black", 10),
                        text_size = 2.4,
                        axis_opts = list(xsize = 8, xtsize = 6, xaxis_label = "")) {
  ggplot(
    datain,
    aes(
      x = .data[[xvar]],
      y = .data[[yvar]],
      label = .data[[labelvar]],
      color = .data[[colorvar]]
    )
  ) +
    scale_x_discrete(name = axis_opts$xaxis_label, position = "top") +
    scale_color_manual(values = colors) +
    geom_text(size = text_size, hjust = 0.5) +
    theme_cleany(xsize = axis_opts$xsize, xtsize = axis_opts$xtsize, lines_color = "white")
}

#' Get legend Level Labels
#'
#' @param datain Input Dataframe
#' @param series_var Series Variable
#' @param series_labelvar Series Label Variable
#'
#' @return Vector of label values
#' @noRd
series_leg_lab <- function(datain,
                           series_var = "TRTVAR",
                           series_labelvar = "TRTTXT") {
  if (series_labelvar != series_var && is.factor(datain[[series_labelvar]])) {
    return(sort(unique(datain[[series_labelvar]])))
  } else {
    return(waiver())
  }
}
