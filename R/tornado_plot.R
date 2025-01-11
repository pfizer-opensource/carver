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
#' Tornado Plot
#'
#' @param datain An input dataframe retrieved from `process_tornado_data`()`.
#' @param trt_left_label A Treatment label for displaying left hand side plot.
#' @param trt_right_label A Treatment label for displaying right hand side plot.
#' @inheritParams bar_plot
#'
#' @return Plot object
#' @export
#'
#' @examples
#' data("adsl")
#' data("adae")
#'
#' tornado_df <- process_tornado_data(
#'   dataset_adsl = adsl,
#'   dataset_analysis = adae,
#'   adsl_subset = "SAFFL == 'Y'",
#'   analysis_subset = "TRTEMFL == 'Y'",
#'   obs_residual = "30",
#'   fmq_data = NA,
#'   ae_catvar = "AESEV/AESEVN",
#'   trtvar = "ARM",
#'   trt_left = "Xanomeline High Dose",
#'   trt_right = "Xanomeline Low Dose",
#'   pctdisp = "TRT",
#'   denom_subset = NA_character_,
#'   legendbign = "N",
#'   yvar = "AESOC"
#' )
#'
#' series_opts <- g_seriescol(tornado_df, "blue~yellow~red", "BYVAR1")
#'
#' tornado_plot(
#'   tornado_df,
#'   trt_left_label = "DRUG B",
#'   trt_right_label = "DRUG C",
#'   bar_width = 0.5,
#'   axis_opts = plot_axis_opts(
#'     xaxis_label = "Primary System Organ Class",
#'     yaxis_label = "% of Subjects",
#'     ylinearopts = list(
#'       breaks = seq(-100, 100, 10),
#'       labels = c(seq(100, 0, -10), seq(10, 100, 10))
#'     )
#'   ),
#'   legend_opts = list(
#'     label = "Severity",
#'     pos = c(0.15, 0.15),
#'     dir = "vertical"
#'   ),
#'   series_opts = series_opts,
#'   griddisplay = "N"
#' )
tornado_plot <- function(datain,
                         trt_left_label,
                         trt_right_label,
                         bar_width = 0.5,
                         axis_opts = plot_axis_opts(
                           xaxis_label = "Primary System Organ Class",
                           yaxis_label = "% of Subjects",
                           ylinearopts = list(
                             breaks = seq(-100, 100, 10),
                             labels = (c(seq(100, 0, -10), seq(10, 100, 10)))
                           )
                         ),
                         legend_opts = list(
                           label = "Severity",
                           pos = c(0.15, 0.15),
                           dir = "vertical"
                         ),
                         series_opts,
                         griddisplay = "N") {
  stopifnot(nrow(datain) != 0)
  stopifnot(
    "XVAR Treatment values not in data" =
      all(c("XVAR", "trt_left", "trt_right") %in% names(datain))
  )

  # Tornado plot - Flipped left and right Bar plots to ref line at 0
  g_plot <- datain |>
    ggplot(aes(x = XVAR)) +
    geom_hline(yintercept = 0) +
    geom_col(
      data = datain,
      aes(y = -.data$trt_left, fill = BYVAR1),
      width = bar_width
    ) +
    geom_col(
      data = datain,
      aes(y = .data$trt_right, fill = BYVAR1),
      width = bar_width
    ) +
    coord_flip()

  names(series_opts) <- rev(names(series_opts))
  # Adding Labels, Breaks, Colors, Ticks, Themes
  g_plot +
    labs(
      title = paste(trt_left_label, ":", trt_right_label),
      x = axis_opts$xaxis_label,
      y = axis_opts$yaxis_label
    ) +
    scale_y_continuous(
      breaks = axis_opts$Ybrks,
      labels = axis_opts$Yticks
    ) +
    scale_fill_manual(
      values = rev(series_opts),
      name = legend_opts$label
    ) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_std(legend_opts = legend_opts)
}


#' Pre-process data for tornado plot
#'
#' @param dataset_adsl (`data.frame`) ADSL dataset.
#' @param dataset_analysis (`data.frame`) ADAE dataset.
#' @param adsl_subset (`string`) Subset condition to be applied on `dataset_adsl`.
#' @param analysis_subset Subset conditions for `dataset_analysis`
#' @param ae_catvar Categorical variable for severity analysis and order variable. eg; "ASEV/ASEVN"
#' @param denom_subset Subset condition to be applied to data set for
#' calculating denominator.
#' @param split_by (`string`) By variable for stratification.
#' @param trtvar (`string`) Treatment Variable to be created for analysis.
#' @param trtsort (`string`) Variable to sort treatment variable by.
#' @param trt_left A Treatment value for displaying left hand side plot.
#' @param trt_right A Treatment value for displaying right hand side plot.
#' @param yvar Categorical Analysis variable for Y axis
#' @param pctdisp Method to calculate denominator (for %) by
#' Possible values: "TRT","VAR","COL","SUBGRP","CAT","NONE","NO","DPTVAR"
#' @param subset Overall subset for data set. eg: "EFFFL == 'Y'"
#' eg: `"SAFFL"`, `"EFFFL"` or `NA` for Overall Population.
#' @param legendbign (`string`) Display BIGN in Legend (`Y/N`).
#' @inheritParams ae_pre_processor
#'
#' @details
#' \itemize{
#' \item ae_catvar grouping variable for severity like AESEV(MILD, MODERATE,
#' SEVERE). It must be passed "/" separated with its numeric variable.
#' eg: ASEV/ASEVN; ATOXGR/ATOXGRN
#' \item yvar(dptvar) Adverse Event category, derived term from AE.
#' Possible Values: AEBODSYS, AEDECOD, AEHLT, AEHLGT.
#' }
#' @return mcatstat dataset as data frame.
#' @export
#'
#' @examples
#' data(tornado_plot_data)
#'
#' process_tornado_data(
#'   dataset_adsl = tornado_plot_data[["adsl"]],
#'   dataset_analysis = tornado_plot_data[["adae"]],
#'   adsl_subset = "SAFFL == 'Y'",
#'   analysis_subset = "TRTEMFL == 'Y'",
#'   obs_residual = "30",
#'   fmq_data = NA,
#'   ae_catvar = "AESEV/AESEVN",
#'   trtvar = "ARMCD",
#'   trt_left = "A",
#'   trt_right = "A",
#'   pctdisp = "TRT",
#'   denom_subset = NA_character_,
#'   legendbign = "N",
#'   yvar = "AESOC"
#' )
#'
process_tornado_data <-
  function(dataset_adsl,
           dataset_analysis,
           adsl_subset = NA_character_,
           analysis_subset = NA_character_,
           obs_residual = NA_real_,
           fmq_data = NULL,
           split_by = NA_character_,
           ae_catvar,
           trtvar,
           trt_left,
           trt_right,
           trtsort = NA_character_,
           subset = NA_character_,
           pctdisp = "TRT",
           denom_subset = NA_character_,
           legendbign = "N",
           yvar = "AESOC") {
    # Check data sets are not empty
    stopifnot("ADSL data is empty" = nrow(dataset_adsl) != 0)
    stopifnot("Analysis data is empty" = nrow(dataset_analysis) != 0)
    maxvar <- str_to_vec(ae_catvar, "/")
    numvar <- ifelse(length(maxvar) > 1, maxvar[2], paste0(maxvar[1], "N"))
    stopifnot(all(c(numvar, maxvar[1]) %in% toupper(names(dataset_analysis))))
    # Pre-Processing data for Adverse Event
    data_pre <- ae_pre_processor(
      datain = dataset_analysis,
      subset = analysis_subset,
      obs_residual = obs_residual,
      fmq_data = fmq_data,
      max_sevctc = "SEV",
      sev_ctcvar = numvar,
      hterm = character(0),
      lterm = yvar
    )
    # Merge with adsl
    adsl_merged <- adsl_merge(
      adsl = dataset_adsl,
      adsl_subset = adsl_subset,
      dataset_add = data_pre$data
    )
    # Data mentry processing
    mentry_out <- mentry(
      adsl_merged,
      subset = subset,
      byvar = maxvar,
      subgrpvar = str_remove_all(split_by, " "),
      trtvar = trtvar,
      trtsort = trtsort
    )
    stopifnot(
      "Given Subsets not present in Analysis Data" =
        nrow(mentry_out) != 0
    )

    # Summary analysis for tornado plot dataset
    mcatstat_out <- mcatstat(
      datain = mentry_out,
      a_subset = data_pre$a_subset,
      denom_subset = denom_subset,
      uniqid = "USUBJID",
      dptvar = yvar,
      pctdisp = pctdisp,
      sparseyn = "N"
    ) |> (\(x) {
      mutate(x,
        YVAR = as.numeric(PCT),
        XVAR = factor(x$XVAR,
          levels = rev(x |>
            group_by(XVAR) |>
            mutate(XVARPCTS = sum(as.numeric(PCT))) |>
            arrange(desc(.data$XVARPCTS)) |>
            distinct(XVAR) |>
            pull(XVAR))
        ),
        BYVAR1 = factor(x$BYVAR1, levels = rev(unique(x$BYVAR1)))
      ) |>
        pivot_wider(names_from = "TRTVAR", values_from = "YVAR") |>
        mutate(trt_left = !!sym(trt_left), trt_right = !!sym(trt_right))
    })()

    # Dataset for tornado plot
    plot_title_nsubj(
      mentry_out,
      mcatstat_out,
      var_start(mcatstat_out, "SUBGRP")
    ) |>
      plot_display_bign(mentry_out, bignyn = legendbign)
  }
