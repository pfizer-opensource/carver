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
#' Event graphs for FDA Medical queries
#'
#' @param datain `list` obtained from `process_event_analysis`
#' @param fig.align Alignment of plots, `"h"` for horizontal or `"v"` for vertical.
#' @param disp.proportion Display proportion of plots horizontally or vertically, should sum up
#' to `10`.
#' @param ref_line y axis position of the Horizontal reference line.
#' @param x_tickangle angle of `x` axis `ticks`, default is `15`
#' @param pt_color Color of bars for Preferred Terms.
#' @param interactive Display interactive graph `"Y"/"N"`
#'
#' @return `ggplot` or `plotly` object.
#' @export
#'
#' @examples
#'
#' data(adae)
#' data(FMQ_Consolidated_List)
#'
#' ## process `ADAE` with `ae_pre_processor()`
#' prep_ae <- adae |>
#'   ae_pre_processor(
#'     ae_filter = "ANY",
#'     obs_residual = 0,
#'     fmq_data = FMQ_Consolidated_List
#'   )
#'
#' ## prepare data for plot
#' prep_event_analysis <- prep_ae[["data"]] |>
#'   process_event_analysis(
#'     a_subset = glue::glue("AOCCPFL == 'Y' & {prep_ae$a_subset}"),
#'     trtvar = "TRTA",
#'     trtsort = "TRTAN",
#'     trttotalyn = "Y",
#'     summary_by = "Events",
#'     hterm = "FMQ_NAM",
#'     ht_val = "ABDOMINAL PAIN",
#'     ht_scope = "Narrow",
#'     lterm = "AEDECOD",
#'     lt_val = "ABDOMINAL DISCOMFORT",
#'     lt_scope = "Narrow"
#'   )
#'
#' ## static plot
#' event_analysis_plot(
#'   datain = prep_event_analysis,
#'   disp.proportion = "4~6",
#'   ref_line = 1
#' )
#'
#' ## interactive plot
#' event_analysis_plot(
#'   datain = prep_event_analysis,
#'   ref_line = 1,
#'   interactive = "Y"
#' )
#'
event_analysis_plot <-
  function(datain,
           fig.align = "h",
           disp.proportion = "3~7",
           ref_line = NA_integer_,
           x_tickangle = 15,
           pt_color = "royalblue3",
           interactive = "N") {
    pt_df <- datain[["pt_df"]]
    query_df <- datain[["query_df"]]
    ## plot options
    yscales <- get_yscales(query_df)
    props <- as.numeric(str_to_vec(disp.proportion))
    pt_title <-
      paste0(str_to_title(str_remove_all(toupper(unique(
        pt_df[["DPTVAL"]]
      )), "/NARROW|/BROAD")), " PT")
    query_title <-
      str_wrap(paste0(
        toupper(sub(
          "\\_.*", "", unique(query_df[["HTERM"]])
        )),
        " Categorization of ",
        str_to_title(unique(query_df[["HVAL"]]))
      ), width = 80)
    ## get plots for preferred term and MedDRA queries
    pt_plot <-
      pt_plot(
        pt_df,
        yscales,
        as.numeric(ref_line),
        as.numeric(x_tickangle),
        pt_color
      )
    query_plot <-
      query_plot(
        query_df,
        yscales,
        as.numeric(ref_line),
        as.numeric(x_tickangle),
        pt_color
      )
    ## plot layout
    if (fig.align == "h") {
      rel_widths <- props
      rel_heights <- 1
    } else {
      rel_widths <- 1
      rel_heights <- props
    }
    ## plot type
    if (interactive == "N") {
      pt_plot <- pt_plot +
        geom_hline(yintercept = ref_line, linetype = "dashed") +
        ggtitle(pt_title)
      query_plot <- query_plot +
        geom_hline(yintercept = ref_line, linetype = "dashed") +
        ggtitle(query_title)
      
      p <- cowplot::plot_grid(
        pt_plot,
        query_plot,
        nrow = if_else(fig.align == "h", 1, 2),
        rel_widths = rel_widths,
        rel_heights = rel_heights
      )
    } else {
      pt_plot <- pt_plot |>
        event_plotly(ref_line, pt_title)
      query_plot <- query_plot |>
        event_plotly(ref_line, query_title)
      
      p <- subplot(
        pt_plot,
        query_plot,
        nrows = if_else(fig.align == "h", 1, 2),
        widths = map_dbl(rel_widths, \(x) ifelse(fig.align == "h", x / 10, x)),
        heights = map_dbl(rel_heights, \(x) ifelse(fig.align == "h", x, x / 10)),
        margin = 0.055
      )
    }
    p
  }

#' Process data for Event Analysis
#'
#' @inheritParams mentry
#' @param a_subset Analysis subset condition.
#' @param summary_by Set to `'Events'` or `'Patients'`.
#' @param hterm `MedDRA` queries or high level variables; valid values : `"FMQ_NAM"`, `"AEBODSYS"`
#' @param ht_val Queries/term name captured in the `hl_var` variable, e.g.`"Erythema"`
#' @param ht_scope Scope of the `MedDRA` queries; Valid values : `"Narrow"` or `"Broad"`
#' @param lterm `MedDRA` variable; Valid value : `"AEDECOD"`
#' @param lt_val `MedDRA` value, e.g., `"Erythema"`
#' @param lt_scope Scope of the `MedDRA` queries; Valid values : `"Narrow"` or `"Broad"`
#'
#' @return List of data frames summarized by `hterm` and `lterm`
#' @export
#'
#' @examples
#' data(adae)
#' data(FMQ_Consolidated_List)
#'
#' ## process `ADAE` with `ae_pre_processor()`
#' prep_ae <- adae |>
#'   ae_pre_processor(
#'     ae_filter = "ANY",
#'     obs_residual = 0,
#'     fmq_data = FMQ_Consolidated_List
#'   )
#'
#' ## prepare data for plot
#' prep_event_analysis <- prep_ae[["data"]] |>
#'   process_event_analysis(
#'     a_subset = glue::glue("AOCCPFL == 'Y' & {prep_ae$a_subset}"),
#'     trtvar = "TRTA",
#'     trtsort = "TRTAN",
#'     trttotalyn = "Y",
#'     summary_by = "Events",
#'     hterm = "FMQ_NAM",
#'     ht_val = "ABDOMINAL PAIN",
#'     ht_scope = "Narrow",
#'     lterm = "AEDECOD",
#'     lt_val = "ABDOMINAL DISCOMFORT",
#'     lt_scope = "Narrow"
#'   )
#'
#' prep_event_analysis[["pt_df"]]
#' prep_event_analysis[["query_df"]]
#'
process_event_analysis <-
  function(datain,
           a_subset = NA_character_,
           trtvar,
           trtsort = NA_character_,
           trttotalyn = "N",
           summary_by = "Events",
           hterm,
           ht_val,
           ht_scope,
           lterm,
           lt_val,
           lt_scope) {
    stopifnot(is.data.frame(datain) & nrow(datain) > 0)
    stopifnot(
      "`summary_by` should be one of 'Patients' or 'Events'" =
        toupper(summary_by) %in% c("PATIENTS", "EVENTS")
    )
    ht_val <- get_event_scope(hterm, ht_val, ht_scope)
    lt_val <- get_event_scope(lterm, lt_val, lt_scope, ht_val)
    
    ae_counts <- datain |>
      mentry(
        byvar = hterm,
        trtvar = trtvar,
        trtsort = trtsort,
        trttotalyn = trttotalyn
      ) |>
      mcatstat(
        a_subset = a_subset,
        uniqid = ifelse(toupper(summary_by) == "PATIENTS", "USUBJID", "ALLCT"),
        dptvar = lterm,
        pctdisp = "TRT",
        sparseyn = "N",
        pctsyn = "Y"
      )
    
    hl_summ <- ae_counts |>
      filter_events(hterm, ht_val, "BYVAR1") |>
      mutate(
        HTERM = hterm,
        HVAL = str_remove_all(ht_val, glue("/{toupper(ht_scope)}")),
        LVAL = lt_val,
        Percent = paste0(.data$CPCT, " \n Low Term:", .data$DPTVAL)
      ) |>
      group_by(.data$TRTVAR) |>
      mutate(DECODh = ifelse(str_detect(
        toupper(.data$DPTVAL), toupper(lt_val)
      ), 9999, rank(.data$PCT))) |>
      ungroup()
    
    stopifnot("No data available for higher terms" = nrow(hl_summ) > 0)
    
    ll_summ <- ae_counts |>
      filter_events(lterm, lt_val, "DPTVAL") |>
      mutate(
        Percent = .data$CPCT
      ) |>
      arrange(.data$TRTVAR, .data$PCT)
    
    stopifnot("No data available for preferred terms" = nrow(ll_summ) > 0)
    
    list(hl_summ, ll_summ) |>
      set_names(c("query_df", "pt_df")) |>
      map(\(x) {
        levels <- str_wrap(levels(x$TRTVAR), 15)
        df <- mutate(x, TRTVAR = str_wrap(.data$TRTVAR, 15))
        df[["TRTVAR"]] <- factor(df[["TRTVAR"]], levels = levels)
        df
      })
  }

#' Bar chart for Preferred Term
#'
#' @param df `pt_df` obtained from `process_event_analysis`
#' @param yscales y axis range and breaks
#' @param ref_line Reference Line
#' @param x_tickangle Angle for x axis `ticks`
#' @param pt_color color of Preferred Term bars
#'
#' @return ggplot object
#' @noRd
#'
pt_plot <-
  function(df,
           yscales,
           ref_line = NA_integer_,
           x_tickangle,
           pt_color) {
    df |>
      ggplot(aes(
        x = .data$TRTVAR,
        y = .data$PCT,
        text = .data$Percent
      )) +
      geom_bar(
        stat = "identity",
        width = 0.4,
        fill = pt_color,
        color = "#606060",
        linewidth = 0.4
      ) +
      scale_y_continuous(
        limits = c(0, yscales$ymax),
        breaks = seq(0, yscales$ymax, yscales$ybreak)
      ) +
      labs(x = "Treatment", y = "Percentage of Participants", colour = NULL) +
      theme_std(axis_opts = list_modify(plot_axis_opts(), xtangle = x_tickangle)) +
      theme(legend.text = element_text(size = 8))
  }

#' Bar chart for Higher Terms/MedDRA Queries
#'
#' @param df `query_df` obtained from `process_event_analysis`
#' @param yscales y axis range and breaks
#' @param ref_line Reference Line
#' @param x_tickangle Angle for x axis `ticks`
#' @param pt_color color of Preferred Term bars
#'
#' @return ggplot object
#' @noRd
#'
query_plot <-
  function(df,
           yscales,
           ref_line = NA_integer_,
           x_tickangle,
           pt_color) {
    cols <-
      get_query_bar_colors(
        unique(df[["DPTVAL"]]),
        unique(df[["LVAL"]]),
        pt_color
      )
    
    df |>
      mutate(across("DPTVAL", ~ toupper(.x))) |>
      ggplot(
        aes(
          x = .data$TRTVAR,
          y = .data$PCT,
          fill = reorder(.data$DPTVAL, -.data$DECODh),
          group = .data$DECODh,
          text = .data$Percent
        )
      ) +
      geom_bar(
        position = "stack",
        stat = "identity",
        width = 0.4,
        color = "#606060",
        linewidth = 0.4
      ) +
      scale_fill_manual(
        values = cols,
        name = NULL
      ) +
      scale_y_continuous(
        limits = c(0, yscales$ymax),
        breaks = seq(0, yscales$ymax, yscales$ybreak)
      ) +
      labs(x = "Treatment", y = "Percentage of Participants", colour = NULL) +
      theme_std(
        legend_opts = list(pos = "right", dir = "vertical"),
        axis_opts = list_modify(plot_axis_opts(), xtangle = x_tickangle)
      ) +
      theme(
        legend.text = element_text(size = 8),
        legend.background = element_rect(color = NA)
      )
  }

#' Filter events
#'
#' @param df data frame
#' @param var name of  variable
#' @param val values of `var`
#' @param filter_var variable to filter
#'
#' @noRd
#'
#' @return filtered data frame
#'
filter_events <- function(df, var, val, filter_var) {
  if (var %in% c("FMQ_NAM", "CQ_NAM")) {
    df <- df |>
      filter(str_detect(toupper(.data[[filter_var]]), toupper(val)))
  } else {
    df <- df |>
      filter(toupper(.data[[filter_var]]) == toupper(val))
  }
  df
}

#' Concatenate event with scope
#'
#' @param var name of the variable
#' @param val values of `var`
#' @param scope scope of `MedDRA` queries
#' @param hval name of higher level variable
#'
#' @return `vector` of variable names
#' @noRd
#'
get_event_scope <- function(var, val, scope, hval = NULL) {
  glue_val <- toupper(val)
  if (var %in% c("FMQ_NAM", "CQ_NAM")) {
    if (toupper(scope) == "NARROW") {
      glue_val <- glue("{glue_val}/{toupper(scope)}")
    } else {
      if (!is.null(hval)) {
        glue_val <- toupper(hval)
      }
    }
  }
  glue_val
}

#' Get range and breaks for y axis
#'
#' @param df plot data
#'
#' @return List of y axis range and breaks
#' @noRd
#'
get_yscales <- function(df) {
  ymax <- df |>
    group_by(.data$TRTVAR) |>
    summarise(pct_s = sum(.data$PCT)) |>
    ungroup() |>
    filter(.data$pct_s == max(.data$pct_s, na.rm = TRUE)) |>
    mutate(
      yscal_max = ifelse(.data$pct_s < 5, .data$pct_s + 10, .data$pct_s + 5),
      .keep = "none"
    ) |>
    pull()
  
  list(ymax = ymax, ybreak = ifelse(ymax > 40, 5, 2))
}

#' Get colors for stacked bars
#'
#' @param dptval unique values of `DPTVAL`
#' @param lval lower term
#' @param pt_color color of Preferred Term bars
#'
#' @return Vector of hex colors
#' @noRd
#'
get_query_bar_colors <- function(dptval, lval, pt_color) {
  if (lval %in% toupper(dptval) && length(dptval) > 1) {
    cols <-
      c(pt_color, scales::hue_pal()(length(dptval) - 1)) |>
      set_names(c(lval, setdiff(toupper(dptval), lval)))
  } else if (lval %in% toupper(dptval)) {
    cols <- pt_color
  } else {
    cols <- scales::hue_pal()(length(dptval))
  }
  cols
}

#' Event Analysis plotly
#'
#' @param p `ggplot` object
#' @param ref_line Reference line
#' @param title_text title to display
#'
#' @return `plotly` object
#' @noRd
#'
event_plotly <- function(p, ref_line, title_text) {
  p |>
    as_plotly(legend_opts = list(label = "", pos = "right", dir = "v")) |>
    insert_ref_line(ref_line) |>
    add_annotations(
      text = title_text,
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      showarrow = FALSE,
      yshift = 35,
      font = list(size = 12)
    )
}

#' Overwrite plotly line shape without using `shapes` argument in `plotly::layout()`
#'
#' This is due to an existing bug in `plotly`.[https://community.rstudio.com/t/drawing-lines-from-layouts-doesnt-work-on-ggplotly/54116] # nolint
#'
#' @param p `ggplot` object
#' @param ref_line Reference line
#'
#' @return Modified `plotly` object
#' @noRd
#'
insert_ref_line <- function(p, ref_line) {
  p$x$layout$shapes <- list(
    list(
      type = "line",
      line = list(color = "black", dash = "dot"),
      x0 = 0,
      x1 = 1,
      y0 = ref_line,
      y1 = ref_line,
      xref = "paper",
      yref = "y"
    )
  )
  p
}
