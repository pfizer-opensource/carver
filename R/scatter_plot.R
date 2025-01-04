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
#' @param datain `data.frame` retrieved from `process_vx_scatter_data()`.
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
#' library(purrr)
#'
#' # Example 1
#'
#' data("vx_scatter_data")
#'
#' ## process data for plotting
#' scatter_df <-
#'   process_vx_scatter_data(
#'     dataset_adsl = vx_scatter_data[["adsl"]],
#'     adsl_subset = 'EVALFL=="Y"',
#'     dataset_analysis = vx_scatter_data[["adva"]],
#'     analysis_subset = 'ANL01FL=="Y" & PARAMN==23 &
#' ((AVISITN ==1 & EVALFL=="Y")|(AVISITN==2 & EVALFL=="Y" & EXCL5FL=="N")) & # nolint
#'  TRTA!="" & DTYPE=="LLOQIMP" & !is.na(AVAL)', # nolint
#'     split_by = "SEX",
#'     trtvar = "ACTARM",
#'     xvar = "AVISITN == 1",
#'     yvar = "AVISITN == 2",
#'     legendbign = "Y"
#'   )
#'
#' ## shape, color and symbols
#' series_opts <- plot_aes_opts(
#'   datain = scatter_df,
#'   series_color = "#F8766D~#619CFF",
#'   series_shape = "circle~triangle",
#'   series_size = as.numeric(str_to_vec("2~2"))
#' )
#'
#' ## splitting data to generate scatter plots of each subgroup (only if `split_by` is specified in
#' ## `process_vx_scatter_data`)
#' data_list <- split_data_by_var(
#'   datain = scatter_df,
#'   split_by_prefix = "SUBGRPVAR"
#' )
#'
#' ## map over `scatter_plot` on split data
#' purrr::map(data_list, \(p) {
#'   scatter_plot(
#'     datain = p,
#'     axis_opts = plot_axis_opts(
#'       xlinearopts = list(
#'         breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
#'         limits = c(0.001, 100)
#'       ),
#'       ylinearopts = list(
#'         breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
#'         limits = c(0.001, 1000)
#'       ),
#'       xaxis_scale = "log10",
#'       yaxis_scale = "log10",
#'       xaxis_label = "Before Vaccination 1",
#'       yaxis_label = "1 Month after Vaccination 1"
#'     ),
#'     series_var = "TRTVAR",
#'     series_labelvar = "TRTVAR",
#'     series_opts = series_opts,
#'     legend_opts = list(
#'       label = "",
#'       pos = "bottom",
#'       dir = "horizontal"
#'     ),
#'     plot_title = paste("Number of Participants = ", length(unique(p$SUBJID))),
#'     griddisplay = "Y"
#'   )
#' })
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

#' Process data for Vaccines Scatter Plot
#'
#' @param dataset_adsl (`data.frame`) ADSL dataset.
#' @param dataset_analysis (`data.frame`) Analysis Dataset.
#' @param adsl_subset (`string`) Subset condition to be applied on `dataset_adsl`.
#' @param analysis_subset (`string`) Subset Condition to be applied on `dataset_analysis`.
#' @param split_by (`string`) By variable for stratification.
#' @param trtvar (`string`) Treatment Variable to be created for analysis.
#' @param trtsort (`string`) Variable to sort treatment variable by.
#' @param xvar (`string`) Values for X axis, determined by filter condition for
#' analysis visit.
#' @param yvar (`string`) Values for Y axis, determined by filter condition for
#' analysis visit.
#' @param legendbign (`string`) Display count as (N = ..) in Treatment legend? Values: "Y"/"N"
#'
#' @return Grouped Data Frames within a list
#' @export
#'
#' @examples
#' data("vx_scatter_data")
#'
#' process_vx_scatter_data(
#'   dataset_adsl = vx_scatter_data[["adsl"]],
#'   dataset_analysis = vx_scatter_data[["adva"]],
#'   adsl_subset = 'EVALFL=="Y"',
#'   analysis_subset = 'ANL01FL=="Y" & PARAMN==23 &
#' ((AVISITN ==1 & EVALFL=="Y")|(AVISITN==2 & EVALFL=="Y" & EXCL5FL=="N")) &
#'  TRTA!="" & DTYPE=="LLOQIMP" & !is.na(AVAL)', # nolint
#'   split_by = "SEX",
#'   trtvar = "ACTARM",
#'   xvar = "AVISITN == 1",
#'   yvar = "AVISITN == 2"
#' )
#'
process_vx_scatter_data <-
  function(dataset_adsl,
           dataset_analysis,
           adsl_subset,
           analysis_subset = NA_character_,
           split_by = NA_character_,
           trtvar,
           trtsort = NA,
           xvar = "AVISITN == 1",
           yvar = "AVISITN == 2",
           legendbign = "Y") {
    stopifnot(is.data.frame(dataset_adsl))
    stopifnot(is.data.frame(dataset_analysis))
    stopifnot(nrow(dataset_adsl) > 0)
    stopifnot(nrow(dataset_analysis) > 0)
    stopifnot(trtvar %in% toupper(names(dataset_adsl)))
    stopifnot("AVAL" %in% toupper(names(dataset_analysis)))
    stopifnot(xvar != yvar)
    if (!is.na(split_by) && str_squish(split_by) != "") {
      stopifnot(all(str_to_vec(split_by) %in% toupper(names(dataset_adsl))))
    }
    
    adsl_sub <- adsl_merge(
      dataset_adsl,
      adsl_subset,
      dataset_analysis
    )
    
    stopifnot(nrow(adsl_sub) > 0)
    
    mentry_df <- adsl_sub |>
      mentry(
        subset = analysis_subset,
        subgrpvar = str_remove_all(split_by, " "),
        trtvar = trtvar,
        trtsort = trtsort,
        add_grpmiss = "N",
        pop_fil = "Overall Population"
      )
    
    stopifnot(nrow(mentry_df) > 0)
    
    a_dsin <- mentry_df |>
      mutate(Vars = case_when(
        !!!parse_exprs(xvar) ~ "XVAR",
        !!!parse_exprs(yvar) ~ "YVAR"
      ))
    
    if (all(is.na(a_dsin[["Vars"]]))) {
      stop("`xvar/yvar` are invalid")
    }
    
    a_dsin_ <- pivot_wider(
      a_dsin,
      id_cols = c(SUBJID, TRTVAR, starts_with("SUBGRPVAR")),
      names_from = Vars,
      values_from = AVAL
    ) |>
      plot_display_bign(mentry_df, bignyn = legendbign)
    
    return(a_dsin_)
  }
