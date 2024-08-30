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
#' Process data for vaccine boxplot
#'
#' @inheritParams process_vx_scatter_data
#' @param ystat Additional Statistic to be calculated and plotted as markers.
#' Values: 'mean', 'sum', 'sd' etc
#' @param ada_nab_opts List of values of : *PARAMN*, Y axis Label, Reference
#' line value and Dilution (for footnote) corresponding to ADA and NAb titers
#' respectively. Format: list(N = "1~2",
#' LAB = "ADA Titer (log2)~NAb Titer (log2)", REF = "6.23~1.58", DIL = "75~3")
#'
#' @return Dataframe containing analysis values for requisite box plot statistics
#' @export
#'
#' @examples
#' data(vx_box_data)
#' process_vx_box_data(
#'   dataset_adsl = vx_box_data$adsl,
#'   dataset_analysis = vx_box_data$adisda,
#'   adsl_subset = "RANDFL == 'Y'",
#'   analysis_subset = "((ANL08FL == 'Y')|(ANL09FL=='Y'))&(ARMCD!='')&(PARAMN %in% c(1, 2))",
#'   trtvar = "TRTA",
#'   trtsort = "TRTAN",
#'   xvar = "AVISIT",
#'   ystat = "mean"
#' )
process_vx_box_data <-
  function(dataset_adsl,
           dataset_analysis,
           adsl_subset = "SAFFL == 'Y'",
           analysis_subset = "((ANL08FL == 'Y')|(ANL09FL=='Y'))&(ARMCD!='')",
           split_by = NA_character_,
           trtvar = "TRT01A",
           trtsort = "TRT01AN",
           xvar = "AVISIT",
           yvar = "AVAL",
           ystat = "mean",
           legendbign = "Y",
           ada_nab_opts =
             list(
               N = "1~2",
               LAB = "ADA Titer (log2)~NAb Titer (log2)",
               REF = "6.23~1.58",
               DIL = "75~3"
             )) {
    # Check data sets are not empty
    stopifnot("ADSL data is empty" = nrow(dataset_adsl) != 0)
    stopifnot("Analysis data is empty" = nrow(dataset_analysis) != 0)
    # Set subgroup variables (page/split variables)
    subgrps <- ifelse(all(is.na(split_by)), "PARAM",
      paste(c(split_by, "PARAM"), collapse = "~")
    )
    # Merge with adsl
    adsin_entry <- adsl_merge(
      dataset_adsl,
      adsl_subset,
      dataset_analysis
    ) |>
      mentry(
        subset = analysis_subset,
        byvar = xvar,
        subgrpvar = subgrps,
        trtvar = trtvar,
        trtsort = trtsort,
        trttotalyn = "N",
        sgtotalyn = "N",
        add_grpmiss = "N",
        pop_fil = NA
      )
    stopifnot(
      "Given Subsets not present in Analysis Data" =
        nrow(adsin_entry) != 0
    )
    stats_data <- msumstat(
      datain = adsin_entry,
      dptvar = yvar,
      statvar = c(ystat, "box"),
      sigdec = 3
    )
    # Paramn subgrp var name:
    psub <- max(var_start(stats_data$gsum, "SUBGRPVARN"))
    # ADa_NaB data combined:
    ada_nab_data <-
      data.frame(
        as.numeric(str_to_vec(ada_nab_opts$N)),
        as.numeric(str_to_vec(ada_nab_opts$REF)),
        str_to_vec(ada_nab_opts$LAB),
        as.numeric(str_to_vec(ada_nab_opts$DIL)),
        c("ADA", "NAb")
      ) |> setNames(c(psub, "REF", "YLAB", "DIL", "TITER"))
    # Merge with ada opts and create X variable; spltN var for plot title
    stats <- plot_title_nsubj(
      adsin_entry,
      stats_data$gsum,
      var_start(stats_data$gsum, "SUBGRP")
    ) |>
      mutate(XVAR = fct_reorder(str_to_title(.data[["BYVAR1"]]), .data[["BYVAR1N"]])) |>
      dplyr::inner_join(ada_nab_data, by = psub) |>
      plot_display_bign(adsin_entry, bignyn = legendbign)
    return(stats)
  }


#' Generate Vaccine Boxplots for antibody titer using analysed data
#'
#' Creates 2 similar plots with slightly different specifications according to
#' parameter i.e., ADA or NaB titer values are plotted in 2 separate graphs
#'
#' @param datalist List of Input datasets, retrieved from
#' `process_vx_box_data()` and `split_data_by_var()`
#' @inheritParams box_plot
#'
#' @details Input data should come from output of `process_vx_box_data()` and
#' is expected to have the standardised variable XVAR and ada_nab_opts
#' @return a list of lists, each of 2 elements:
#' \itemize{
#' \item `plot` Plot output
#' \item `footnote` Text to be considered as first line of footnote in report
#' }
#' @export
#'
#' @examples
#' data(vx_box_data)
#' plot_data <- process_vx_box_data(
#'   dataset_adsl = vx_box_data$adsl,
#'   dataset_analysis = vx_box_data$adisda,
#'   adsl_subset = "RANDFL == 'Y'",
#'   analysis_subset = "((ANL08FL == 'Y')|(ANL09FL=='Y'))&(ARMCD!='')&(PARAMN %in% c(1, 2))",
#'   trtvar = "TRTA",
#'   trtsort = "TRTAN",
#'   xvar = "AVISIT",
#'   ystat = "mean",
#'   legendbign = "Y"
#' )
#' series_opts <- plot_aes_opts(
#'   datain = plot_data,
#'   series_color = c("red", "blue", "green"),
#'   series_shape = c("circlefilled", "trianglefilled", "squarefilled"),
#'   series_size = c(2, 2, 2)
#' )
#'
#' # Splitting data to generate separate plots by `split_by` variable
#' data_list <- split_data_by_var(
#'   datain = plot_data,
#'   split_by_prefix = "SUBGRPVAR"
#' )
#'
#' vx_box_plot(
#'   datalist = data_list,
#'   axis_opts = plot_axis_opts(
#'     xaxis_label = "Visits"
#'   ),
#'   series_opts = series_opts,
#'   legend_opts = list(
#'     lab = "Treatment",
#'     pos = "bottom",
#'     dir = "horizontal"
#'   ),
#'   ystat = "mean"
#' )[[1]][[1]]
vx_box_plot <- function(datalist,
                        axis_opts,
                        series_opts,
                        legend_opts = list(
                          lab = "", pos = "bottom",
                          dir = "horizontal"
                        ),
                        box_opts = c(0.7, 0.9),
                        ystat = "mean",
                        griddisplay = "N") {
  datalist |>
    map(function(df) {
      stopifnot(is.data.frame(df))
      axis_opts$yaxis_label <- unique(df$YLAB)
      titerp <- box_plot(
        datain = df,
        legend_opts = legend_opts,
        series_opts = series_opts,
        axis_opts = axis_opts,
        series_var = "TRTVAR",
        series_labelvar = ifelse("TRTTXT" %in% names(df), "TRTTXT", "TRTVAR"),
        box_opts = box_opts,
        ystat = ystat,
        griddisplay = griddisplay,
        plot_title = glue("Number of Participants N = {unique(df$splitN)}")
      ) +
        geom_line(
          aes(
            x = XVAR,
            y = .data[[ystat]],
            group = TRTVAR,
            color = TRTVAR
          ),
          position = position_dodge(box_opts[2])
        ) +
        geom_hline(
          yintercept = unique(df$REF),
          linetype = 2,
          color = "black"
        )
      ftnote <- glue(
        "Reference line at Y = {unique(df$REF)} represents the detection limit\\
         in the {unique(df$TITER)} assay (minimum required dilution \\
        {unique(df$DIL)})"
      )
      list(plot = titerp, footnote = ftnote)
    })
}
