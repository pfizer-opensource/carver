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
#' Process data for eDISH plot
#'
#' @param datain Input dataset.
#' @param xvar To determine how `ast` and `alt` should be derived and which should be the
#' `x` variable for the plot. Possible values are: `"both"`, `"ast"`, `"alt"`.
#' If it is "ast" or "alt", the `AVAL` for the corresponding "PARAMCD" is used as X variable.
#' Alternatively, if "both" then the max value between ast and alt for each record is used as X
#' variable.
#' @param alt_paramcd `PARAMCD` value for `ALANINE AMINOTRANSFERASE` in `datain`.
#' @param ast_paramcd `PARAMCD` value for `ASPARTATE AMINOTRANSFERASE` in `datain`.
#' @param bili_paramcd `PARAMCD` value for `BILIRUBIN` in `datain`.
#' @param legendbign (`string`) Display BIGN in Legend (`Y/N`).
#'
#' @return A `data.frame` required for `edish_plot`.
#' @export
#'
#' @examples
#' data("lab_data")
#'
#' merged_data <- adsl_merge(
#'   adsl = lab_data$adsl,
#'   dataset_add = lab_data$adlb
#' ) |>
#'   mentry(
#'     subset = "SAFFL == 'Y'",
#'     trtvar = "TRT01A",
#'     trtsort = "TRT01AN"
#'   )
#'
#' merged_data |>
#'   process_edish_data(
#'     xvar = "both",
#'     alt_paramcd = "L00030S",
#'     ast_paramcd = "L00028S",
#'     bili_paramcd = "L00021S"
#'   )
#'
process_edish_data <- function(datain,
                               xvar = "both",
                               alt_paramcd = "L00030S",
                               ast_paramcd = "L00028S",
                               bili_paramcd = "L00021S",
                               legendbign = "Y") {
  if (!is.data.frame(datain) || nrow(datain) == 0) {
    return(data.frame())
  }
  stopifnot("`xvar` must be one of 'alt', 'ast' or 'both'" = xvar %in% c("alt", "ast", "both"))
  stopifnot(
    "Please provide valid PARAMCD" =
      all(c(alt_paramcd, ast_paramcd, bili_paramcd) %in% datain$PARAMCD)
  )

  hy_data <- datain |>
    filter(
      .data$PARAMCD %in% c(alt_paramcd, ast_paramcd, bili_paramcd),
      !is.na(.data$ANRHI)
    ) |>
    mutate(maxv = .data$AVAL / .data$ANRHI) |>
    mutate(
      PARM = case_when(
        .data$PARAMCD == alt_paramcd ~ "alt",
        .data$PARAMCD == ast_paramcd ~ "ast",
        TRUE ~ "bili"
      )
    )

  hy <- hy_data |>
    group_by(across(all_of(c("USUBJID", "TRTVAR", "PARAMCD", "PARAM", "PARM")))) |>
    summarise(x = max(.data$maxv)) |>
    pivot_wider(
      id_cols = c(USUBJID, TRTVAR),
      names_from = PARM,
      values_from = x
    )

  if (xvar %in% c("alt", "ast")) {
    hy <- hy |> mutate(XVAR = .data[[xvar]])
  } else {
    hy <- hy |> mutate(XVAR = pmax(.data$ast, .data$alt))
  }

  hy |>
    mutate(
      text = paste0(
        "Subjid = ",
        USUBJID,
        "\n",
        ifelse(xvar == "both", "Max of ALT/AST = ",
          paste("value of", toupper(xvar), "=")
        ),
        round(XVAR, 3),
        "\n",
        "Bilirubin = ",
        round(bili, 3)
      ),
      YVAR = .data[["bili"]]
    ) |>
    plot_display_bign(datain, bignyn = legendbign)
}

#' eDISH Plot
#'
#' @param datain Input dataset for the plot retreived from `process_edish_data()`
#' @inheritParams scatter_plot
#' @param xrefline `Xaxis` reference line format.
#' @param yrefline `Yaxis` reference line format.
#' @param quad_labels Labels for each quadrant in the plot, as a vector or a single tilde-separated
#' string.
#' @param interactive Interactive plot (`'Y'/'N'`).
#'
#' @details
#' \itemize{
#'  \item `axis_opts` - The breaks and limits should not be `NULL`/empty.
#'  \item `quad_labels` - The label for the each quadrant should be populated in the following
#'  order: "upper right~lower right~upper left~lower left" or as a vector of length 4
#' }
#'
#' @return A `ggplot` or `plotly` object
#' @export
#'
#' @examples
#' data("lab_data")
#'
#' merged_data <- adsl_merge(
#'   adsl = lab_data$adsl,
#'   dataset_add = lab_data$adlb
#' ) |>
#'   mentry(
#'     subset = "SAFFL == 'Y'",
#'     trtvar = "TRT01A",
#'     trtsort = "TRT01AN"
#'   )
#'
#' pt_data <- process_edish_data(
#'   datain = merged_data,
#'   xvar = "both",
#'   alt_paramcd = "L00028S",
#'   ast_paramcd = "L00030S",
#'   bili_paramcd = "L00021S"
#' )
#'
#' series_opts <- plot_aes_opts(pt_data,
#'   series_color = NA,
#'   series_shape = "circlefilled~trianglefilled",
#'   series_size = c(1.5, 1.5)
#' )
#' edish_plot(
#'   datain = pt_data,
#'   axis_opts = plot_axis_opts(
#'     xlinearopts = list(
#'       breaks = c(0.1, 1, 2, 5),
#'       limits = c(0.1, 5),
#'       labels = c("0.1", "1", "2x ULN", "5")
#'     ),
#'     ylinearopts = list(
#'       breaks = c(0.1, 1, 3, 10),
#'       limits = c(0.1, 10),
#'       labels = c("0.1", "1", "3x ULN", "10")
#'     )
#'   ),
#'   xrefline = c("2", "gray30", "dashed"),
#'   yrefline = c("3", "gray30", "dashed"),
#'   quad_labels =
#'     "Potential Hy's Law Cases~Temple's Corollary~Gilberts Syndrome or Cholestasis~Normal",
#'   legend_opts = list(
#'     label = "Treatment",
#'     pos = "bottom", dir = "horizontal"
#'   ),
#'   series_opts = series_opts,
#'   plot_title = NULL,
#'   interactive = "N"
#' )
#'
edish_plot <- function(datain,
                       axis_opts = plot_axis_opts(
                         xlinearopts = list(
                           breaks = c(0.1, 1, 2, 10),
                           limits = c(0.1, 10),
                           labels = c("0.1", "1", "2x ULN", "10")
                         ),
                         ylinearopts = list(
                           breaks = c(0.1, 1, 3, 10),
                           limits = c(0.1, 10),
                           labels = c("0.1", "1", "3x ULN", "10")
                         )
                       ),
                       xrefline = c("2", "gray30", "dashed"),
                       yrefline = c("3", "gray30", "dashed"),
                       quad_labels = c(
                         "Potential Hy's Law Cases",
                         "Temple's Corollary",
                         "Gilberts Syndrome or Cholestasis",
                         "Normal"
                       ),
                       series_opts = series_opts,
                       plot_title = "",
                       griddisplay = "N",
                       legend_opts = list(
                         label = "Treatment",
                         pos = "bottom",
                         dir = "horizontal"
                       ),
                       interactive = "N") {
  stopifnot(is.data.frame(datain) && nrow(datain) > 0)
  # plot and options
  # setting labels for each quadrants
  quad_labels <- str_to_vec(quad_labels)
  quad_labels_opts_x <- c(
    max(axis_opts$Xbrks) - 1, max(axis_opts$Xbrks) - 1,
    as.numeric(xrefline[1]) - 1,
    as.numeric(xrefline[1]) - 1
  )
  quad_labels_opts_y <- c(
    max(as.numeric(yrefline[1]), max(axis_opts$Ybrks)),
    as.numeric(yrefline[1]) - 0.2,
    max(as.numeric(yrefline[1]), max(axis_opts$Ybrks)),
    as.numeric(yrefline[1]) - 0.2
  )

  # for ploting values per subject

  sp <- datain |>
    scatter_plot(
      axis_opts = axis_opts,
      series_var = "TRTVAR",
      series_labelvar = ifelse("TRTTXT" %in% names(datain), "TRTTXT", "TRTVAR"),
      series_opts = series_opts,
      legend_opts = legend_opts,
      plot_title = plot_title,
      griddisplay = griddisplay
    ) +
    aes(text = .data[["text"]]) +
    geom_hline(
      yintercept = as.numeric(yrefline[1]),
      color = yrefline[2],
      linetype = yrefline[3]
    ) +
    geom_vline(
      xintercept = as.numeric(xrefline[1]),
      color = xrefline[2],
      linetype = xrefline[3]
    ) +
    annotate(
      geom = "text",
      x = quad_labels_opts_x[1],
      y = quad_labels_opts_y[1],
      label = quad_labels[1]
    ) +
    annotate(
      geom = "text",
      x = quad_labels_opts_x[2],
      y = quad_labels_opts_y[2],
      label = quad_labels[2]
    ) +
    annotate(
      geom = "text",
      x = quad_labels_opts_x[3],
      y = quad_labels_opts_y[3],
      label = quad_labels[3]
    ) +
    annotate(
      geom = "text",
      x = quad_labels_opts_x[4],
      y = quad_labels_opts_y[4],
      label = quad_labels[4]
    )

  # ggplotly if interactive
  if (interactive == "Y") {
    sp <- as_plotly(plot = sp, hover = c("text"))
  }
  sp
}
