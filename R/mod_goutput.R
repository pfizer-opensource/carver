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
#' goutput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_goutput_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      id = ns("event_box"),
      title = tags$strong("Event Analysis Inputs"),
      maximizable = TRUE,
      width = 12,
      fluidRow(
        column(
          width = 3,
          selectInput(
            ns("hlt_val"),
            "Event Higher Classification",
            choices = NULL
          )
        ),
        column(
          width = 3,
          uiOutput(ns("aeHLT_query_cat_UI"))
        ),
        column(
          width = 3,
          selectInput(
            ns("llt_val"),
            "Event Lower Classification",
            choices = NULL
          )
        ),
        column(
          width = 3,
          uiOutput(ns("aeLLT_query_cat_UI"))
        )
      )
    ),
    box(
      id = ns("box_1"),
      title = tags$strong(htmlOutput(ns("g_title_UI"))),
      maximizable = TRUE,
      width = 12,
      headerBorder = FALSE,
      footer = htmlOutput(ns("g_footnote_UI")),
      div(
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("figure_UI"), width = "80vw"),
          type = 5,
          color = "cadetblue"
        ),
        style = "overflow-x: scroll; height: 650px;"
      )
    ),
    box(
      id = ns("box_2"),
      title = tags$strong("Plot Listing"),
      maximizable = TRUE,
      width = 12,
      DT::dataTableOutput(ns("plot_listing"))
    ),
    mod_plot_profile_ui(ns("plot_profile_1"))
  )
}

#' goutput Server Functions
#'
#' @noRd
mod_goutput_server <- function(id, sourcedata, repName, filters, process_btn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      outdata = NULL,
      goutput = NULL,
      output_trigger = 0
    )

    observe({
      req(repName())
      req(tolower(repName()) %in% c("ae_volcano_plot", "ae_forest_plot"))
      req(filters()$ae_pre)
      req(filters()$ment_out)
      req(filters()$ae_filter)
      req(filters()$statistics)
      req(filters()$ae_hlt)
      req(filters()$ae_llt)
      req(filters()$summary_by)
      if (tolower(repName()) %in% c("ae_volcano_plot")) {
        req(filters()$treatment1)
        req(filters()$treatment2)
      }
      if (tolower(repName()) %in% c("ae_forest_plot")) {
        req(filters()$ctrlgrp)
        req(filters()$trtgrp)
        req(filters()$sort_opt)
        req(filters()$sort_by)
      }
      req(filters()$alpha)
      req(filters()$cutoff)
      print("AE risk_stat process start")
      if (filters()$a_subset == "") {
        a_subset <- filters()$ae_pre$a_subset
      } else {
        a_subset <- paste(na.omit(
          c(filters()$ae_pre$a_subset, filters()$a_subset)
        ), collapse = " & ")
      }
      withProgress(
        rv$outdata <- risk_stat(
          datain = filters()$ment_out,
          a_subset = a_subset,
          eventvar = ifelse(is.null(filters()$ae_llt), filters()$ae_hlt, filters()$ae_llt),
          summary_by = filters()$summary_by,
          ctrlgrp = ifelse(tolower(repName()) == "ae_volcano_plot",
            filters()$treatment1,
            filters()$ctrlgrp
          ),
          trtgrp = ifelse(
            tolower(repName()) == "ae_volcano_plot",
            filters()$treatment2,
            paste(filters()$trtgrp, collapse = "~~")
          ),
          statistics = filters()$statistics,
          alpha = filters()$alpha,
          cutoff = filters()$cutoff,
          sort_opt = ifelse(tolower(repName()) == "ae_forest_plot", filters()$sort_opt,
            "Ascending"
          ),
          sort_var = ifelse(tolower(repName()) == "ae_forest_plot", filters()$sort_by, "Count")
        ),
        message = "Executing Get Statistics for EVENTS/ PT...",
        detail = "This step should take a while.",
        min = 0,
        max = 1,
        value = 1
      )
      rv$outdata <- rv$outdata |>
        filter(!is.nan(.data[["RISK"]]), !is.infinite(.data[["RISK"]]))
      print("AE risk_stat process end")
      rv$output_trigger <- rv$output_trigger + 1
    }) %>%
      bindEvent(process_btn())

    observe({
      req(tolower(repName()) %in% c("ae_forest_plot"))
      req(rv$outdata)
      req(filters()$ment_out)
      req(filters()$ae_filter)
      req(filters()$pvalcut)
      req(filters()$X_ref)
      req(filters()$riskScale)
      req(filters()$trtbign)
      req(filters()$statistics)
      print("AE Forest Plot process start")
      forest_data <- rv$outdata |>
        plot_display_bign(filters()$ment_out, bignyn = filters()$trtbign)
      withProgress(message = "Generating Forest Plot", value = 0, {
        rv$goutput <- try(
          ae_forest_plot(
            datain = forest_data,
            series_opts = plot_aes_opts(
              datain = forest_data,
              series_color = c("black", "royalblue2", "goldenrod", "forestgreen", "magenta", "brown"),
              series_size = rep(1, 5)
            ),
            trtpair_color = "#F8766D~#7CAE00~#00BFC4~#C77CFF",
            axis_opts = plot_axis_opts(
              xaxis_label = filters()$statistics,
              xaxis_scale = tolower(filters()$riskScale),
              xopts = list(labelsize = 8)
            ),
            risk_ref = as.numeric(filters()$X_ref),
            pvalue_sig = filters()$pvalcut,
            highlight_sig = "Y",
            rel_widths = c(0.4, 0.4, 0.2),
            interactive = "Y"
          )
        )[[1]]
      })
      print("AE Forest Plot process end")
    }) %>%
      bindEvent(rv$output_trigger)

    observe({
      req(tolower(repName()) %in% c("ae_volcano_plot"))
      req(filters()$ae_pre)
      req(rv$outdata)
      req(filters()$X_ref)
      req(filters()$summary_by)
      req(filters()$pvalue_label)
      req(filters()$treatment1_label)
      req(filters()$treatment2_label)
      req(filters()$pvalcut)
      print("AE Volcano Plot process start")
      vaxis_opts <- ae_volcano_opts(
        rv$outdata,
        filters()$treatment1_label,
        filters()$treatment2_label,
        pvalue_trans = filters()$pvalue_label
      )

      axis_opts <- plot_axis_opts(
        ylinearopts = vaxis_opts$ylinearopts,
        yaxis_scale = vaxis_opts$yaxis_scale,
        xaxis_label = vaxis_opts$xaxis_label,
        yaxis_label = vaxis_opts$yaxis_label
      )
      withProgress(message = "Generating Volcano Plot", value = 0, {
        rv$goutput <- try(
          ae_volcano_plot(
            datain = rv$outdata,
            axis_opts = axis_opts,
            legend_opts = list(
              label = "",
              pos = "bottom",
              dir = "horizontal"
            ),
            xref = filters()$X_ref,
            pvalue_sig = filters()$pvalcut,
            interactive = "Y"
          )
        )
      })
      print("AE Volcano Plot process end")
    }) %>%
      bindEvent(rv$output_trigger)

    observe({
      req(tolower(repName()) %in% c("tornado_plot"))
      req(filters()$process_tornado_data)

      print("AE Tornado Plot process start")
      withProgress(message = "Generating Tornado Plot", value = 0, {
        rv$goutput <- try(tornado_plot(
          datain = filters()$process_tornado_data,
          trt_left_label = filters()$treatment1_label,
          trt_right_label = filters()$treatment2_label,
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
          series_opts = g_seriescol(filters()$process_tornado_data, "blue~yellow~red", "BYVAR1"),
          griddisplay = "N"
        ))
      })
      print("AE Tornado Plot process end")
    }) %>%
      bindEvent(process_btn())

    observe({
      req(filters()$ae_pre)
      req(tolower(repName()) %in% c("edish_plot"))

      series_opts <- plot_aes_opts(
        filters()$ae_pre,
        series_color = NA,
        series_shape = "circlefilled~trianglefilled",
        series_size = c(1.5, 1.5)
      )

      withProgress(message = "Generating eDISH Plot", value = 0, {
        rv$goutput <- edish_plot(
          datain = filters()$ae_pre,
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
          quad_labels =
            "Potential Hy's Law Cases~Temple's Corollary~Gilberts Syndrome or Cholestasis~Normal",
          legend_opts = list(
            label = "Treatment",
            pos = "bottom", dir = "horizontal"
          ),
          series_opts = series_opts,
          plot_title = NULL,
          interactive = "N"
        )
      })
    }) %>%
      bindEvent(process_btn())

    observe({
      req(repName())
      if (tolower(repName()) %in% c("event analysis")) {
        show("event_box")
      } else {
        hide("event_box")
      }
    })

    observe({
      req(tolower(repName()) %in% c("event analysis"))
      req(filters()$ae_pre)
      req(filters()$ment_out)
      req(filters()$ae_hlt)

      print("AE event analysis hlt list input process start")

      temp1 <- filters()$ment_out

      if (filters()$ae_hlt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM")) {
        if (filters()$ae_hlt %in% names(temp1)) {
          hlt_list <- unlist(strsplit(unique(temp1[[filters()$ae_hlt]]), "~~"))
          hlt_list <-
            sort(unique(gsub("/\\w+", "", hlt_list[!is.na(hlt_list)])))
        } else {
          hlt_list <- NULL
        }
      } else {
        hlt_list <- sort(unique(temp1[[filters()$ae_hlt]]))
      }

      updateSelectInput(
        session,
        "hlt_val",
        "Event Higher Classification",
        choices = hlt_list,
        selected = hlt_list[1]
      )
      print("AE event analysis hlt list input process end")
    })

    observe({
      req(tolower(repName()) %in% c("event analysis"))
      req(filters()$ae_pre)
      req(filters()$ment_out)
      req(filters()$ae_hlt)
      req(input$hlt_val)
      if (filters()$ae_hlt %in% c("FMQ_NAM", "SMQ_NAM", "CQ_NAM")) {
        req(input$hlt_cat)
      }
      print("AE event analysis llt list input process start")
      temp1 <- filters()$ment_out

      if (filters()$ae_hlt %in% c("FMQ_NAM", "SMQ_NAM", "CQ_NAM")) {
        if (toupper(input$hlt_cat) == "NARROW") {
          hl_val1 <- paste0(str_to_upper(input$hlt_val), "/", str_to_upper(input$hlt_cat))
        } else {
          hl_val1 <- input$hlt_val
        }
      } else {
        hl_val1 <- input$hlt_val
      }

      if (filters()$ae_hlt %in% names(temp1) && filters()$ae_llt %in% names(temp1)) {
        llt_list <-
          unique(temp1[[filters()$ae_llt]][str_detect(
            toupper(temp1[[filters()$ae_hlt]]),
            toupper(hl_val1)
          )])
      } else {
        llt_list <- NULL
      }

      if (filters()$ae_llt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM")) {
        if (!is.null(llt_list)) {
          llt_list <- unlist(strsplit(unique(llt_list), "~~"))
          llt_list <- sort(unique(gsub("/\\w+", "", llt_list[!is.na(llt_list)])))
        }
      } else {
        llt_list <- sort(llt_list)
      }

      updateSelectInput(
        session,
        "llt_val",
        "Event Term",
        choices = llt_list,
        selected = llt_list[1]
      )
      print("AE event analysis llt list input process end")
    })

    observe({
      req(tolower(repName()) %in% c("event analysis"))
      req(filters()$ae_pre)
      print("AE event analysis hlt scope input process start")

      output$aeHLT_query_cat_UI <- renderUI({
        req(filters()$ae_hlt)
        req(filters()$ae_hlt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM"))
        tagList(
          selectInput(
            ns("hlt_cat"),
            "Query Scope",
            choices = c("Narrow", "Broad")
          )
        )
      })
      print("AE event analysis hlt scope input process end")
      print("AE event analysis llt scope input process start")

      output$aeLLT_query_cat_UI <- renderUI({
        req(filters()$ae_llt)
        req(filters()$ae_llt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM"))
        tagList(
          selectInput(
            ns("llt_cat"),
            "Query Scope",
            choices = c("Narrow", "Broad")
          )
        )
      })
      print("AE event analysis llt scope  input process end")
    })

    observe({
      req(tolower(repName()) %in% c("event analysis"))
      req(filters()$ae_pre)
      req(filters()$ae_hlt)
      req(filters()$ae_llt)
      req(filters()$ment_out)
      req(input$hlt_val)
      req(input$llt_val)
      if (filters()$ae_hlt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM")) {
        req(input$hlt_cat)
      }
      if (filters()$ae_llt %in% c("SMQ_NAM", "FMQ_NAM", "CQ_NAM")) {
        req(input$llt_cat)
      }
      req(filters()$summary_by)
      print("AE event analysis process start")
      if (filters()$a_subset == "") {
        a_subset <- filters()$ae_pre$a_subset
      } else {
        a_subset <- paste(na.omit(
          c(filters()$ae_pre$a_subset, filters()$a_subset)
        ), collapse = " & ")
      }
      withProgress(message = "Generating AE event analysis", value = 0, {
        rv$outdata <- try(
          process_event_analysis(
            datain = filters()$ment_out,
            a_subset = a_subset,
            summary_by = filters()$summary_by,
            hterm = filters()$ae_hlt,
            ht_val = input$hlt_val,
            ht_scope = input$hlt_cat,
            lterm = filters()$ae_llt,
            lt_val = input$llt_val,
            lt_scope = input$llt_cat
          )
        )
        rv$goutput <- try(
          event_analysis_plot(
            datain = rv$outdata,
            ref_line = filters()$ref_line,
            interactive = "Y"
          )
        )
      })
      print("AE event analysis process end")
    }) %>%
      bindEvent(process_btn())

    plot_data <- reactive({
      event <- plotly::event_data("plotly_click", source = "plot_output")
      req(length(event) > 0)

      if (tolower(repName()) == "ae_forest_plot") {
        test <- rv$goutput$drill_plt$data[as.numeric(event$key), ]
      } else if (tolower(repName()) == "ae_volcano_plot") {
        test <- rv$outdata[as.numeric(event$key), ]
      } else {
        if (event$curveNumber == 0 && event$x > 0) {
          test <- NULL
        } else {
          test <- rv$goutput$rpt_data
          trt_level_diff <- length(levels(test$TRTVAR)) - length(unique(test$TRTVAR))
          test <- test %>%
            mutate(point_n = as.numeric(as.factor(TRTVAR)) - trt_level_diff)
          order_df <- data.frame(DPTVAL = levels(reorder(test$DPTVAL, -test$DECODh))) %>%
            mutate(curve_n = row_number() + 1)
          test <- full_join(test, order_df, by = "DPTVAL") %>%
            filter(point_n == event$x, curve_n == event$curveNumber)
        }
      }

      req(test)

      display <- filters()$ment_out %>%
        select(any_of(c(
          "USUBJID", "TRTVAR", "BYVAR1", filters()$ae_llt, "AESER", "AEOUT", "AESEV",
          "AESTDT", "ASTTM", "AEENDT", "TRTSTDT", "TRTEDT", "TRTEMFL"
        )))
      plot_table <- select(test, "BYVAR1", "DPTVAL", "TRTVAR") %>%
        rename(!!filters()$ae_llt := "DPTVAL") %>%
        inner_join(display) %>%
        relocate(c("USUBJID", "TRTVAR"))

      ## displaying the listing table
      plot_table <- plot_table %>%
        rename(
          !!filters()$ae_hlt := "BYVAR1",
          !!filters()$trt_var := "TRTVAR"
        ) %>%
        distinct()
    }) %>%
      bindEvent(plotly::event_data("plotly_click", source = "plot_output"))

    # set selected point to null every time plot updates
    observe({
      req(length(plotly::event_data("plotly_click", source = "plot_output")) > 0)
      runjs("Shiny.setInputValue('plotly_click-plot_output', null);")
    }) %>%
      bindEvent(list(repName(), rv$goutput$drill_plt$data, rv$goutput$plot$data))

    observe({
      if (is.null(plotly::event_data("plotly_click", source = "plot_output"))) {
        hide("box_2")
        runjs("Shiny.setInputValue('goutput_1-plot_listing_rows_selected', null);")
      } else {
        show("box_2")
      }
    })

    output$figure_UI <- plotly::renderPlotly({
      req(rv$goutput)
      # req(rv$goutput$ptly)
      rv$goutput
    })

    output$g_title_UI <- renderText({
      req(rv$goutput$ptly)
      rpt_title <- str_replace_all(rv$goutput$title, "\n", "<br>")
      return(HTML(rpt_title))
    })

    ## footnote for Plot
    output$g_footnote_UI <- renderText({
      req(rv$goutput$ptly)
      rpt_ftnote <- str_replace_all(rv$goutput$footnote, "\n", "<br>")
      return(HTML(rpt_ftnote))
    })

    output$plot_listing <- DT::renderDataTable(
      {
        req(plot_data())

        datatable(
          plot_data(),
          extensions = "Buttons",
          selection = list(mode = "single"),
          options = list(
            dom = "Bfrtip",
            buttons = I("colvis"),
            pageLength = 10,
            scrollX = TRUE
          )
        )
      },
      server = FALSE
    )

    mod_plot_profile_server(
      "plot_profile_1",
      sourcedata = reactive(sourcedata()),
      sel_rows = reactive(input$plot_listing_rows_selected),
      datain = reactive(filters()$ae_pre$dout),
      plot_data = plot_data
    )

    reactive(rv$goutput)
  })
}
