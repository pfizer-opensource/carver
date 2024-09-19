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
#' toutput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_toutput_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      id = ns("box_1"),
      title = tags$strong(htmlOutput(ns("t_title_UI"))),
      maximizable = TRUE,
      width = 12,
      headerBorder = FALSE,
      footer = htmlOutput(ns("t_footnote_UI")),
      div(
        shinycssloaders::withSpinner(uiOutput(ns("table_UI")), type = 5, color = "cadetblue"),
        style = "overflow-x: scroll;"
      )
    )
  )
}

#' toutput Server Functions
#'
#' @noRd
mod_toutput_server <- function(id, repName, filters, popfilter, process_btn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      outdata = NULL,
      toutput = NULL,
      title = "",
      footnote = ""
    )

    observe({
      req(repName())
      req(popfilter())
      req(filters())
      req(tolower(repName()) %in% c("adae_tier_summary"))
      req(filters()$ae_pre)
      req(filters()$ae_filter)
      req(filters()$trtbign)
      req(filters()$ae_llt)
      req(filters()$ae_hlt)
      req(filters()$summary_by)
      req(filters()$ment_out)
      req(filters()$a_subset)
      req(filters()$ui_pctdisp)
      req(filters()$cutoff)
      req(filters()$sort_opt)
      req(filters()$sort_by)
      print("AE Summary table process start")
      if (filters()$a_subset == "") {
        a_subset <- filters()$ae_pre$a_subset
      } else {
        a_subset <- paste(na.omit(
          c(filters()$ae_pre$a_subset, filters()$a_subset)
        ), collapse = " & ")
      }
      withProgress(message = "Generating AE Summary table", value = 0, {
        rv$outdata <- try(
          occ_tier_summary(
            filters()$ment_out,
            a_subset = a_subset,
            summary_by = filters()$summary_by,
            hterm = filters()$ae_hlt,
            lterm = filters()$ae_llt,
            pctdisp = filters()$ui_pctdisp,
            cutoff = filters()$cutoff,
            apply_hrow_cutoff = "N",
            sort_opt = filters()$sort_opt,
            sort_var = filters()$sort_by
          ) |>
            display_bign_head(
              mentry_data = filters()$ment_out,
              trtbignyn = filters()$trtbign,
              subbignyn = "N"
            )
        )
        rv$toutput <- try(
          rv$outdata |>
            tbl_processor() |>
            tbl_display() |>
            autofit()
        )
      })
      print("AE Summary table process end")
    }) %>%
      bindEvent(process_btn())


    observe({
      req(repName())
      req(popfilter())
      req(filters())
      req(tolower(repName()) %in% c("adae_risk_summary"))
      req(filters()$ae_pre)
      req(filters()$ae_filter)
      req(filters()$ae_llt)
      req(filters()$ae_hlt)
      req(filters()$summary_by)
      req(filters()$trtbign)
      req(filters()$ment_out)
      req(filters()$a_subset)
      req(filters()$treatment1)
      req(filters()$treatment2)
      req(filters()$statistics)
      req(filters()$alpha)
      req(filters()$ui_pctdisp)
      req(filters()$cutoff)
      req(filters()$sort_opt)
      req(filters()$sort_by)
      print("AE Summary table process start")
      if (filters()$a_subset == "") {
        a_subset <- filters()$ae_pre$a_subset
      } else {
        a_subset <- paste(na.omit(
          c(filters()$ae_pre$a_subset, filters()$a_subset)
        ), collapse = " & ")
      }
      withProgress(message = "Generating AE Risk table", value = 0, {
        rv$outdata <- try(
          adae_risk_summary(
            filters()$ment_out,
            a_subset = a_subset,
            summary_by = filters()$summary_by,
            hterm = filters()$ae_hlt,
            lterm = filters()$ae_llt,
            ctrlgrp = filters()$treatment1,
            trtgrp = filters()$treatment2,
            statistics = filters()$statistics,
            alpha = filters()$alpha,
            cutoff = filters()$cutoff,
            sort_opt = filters()$sort_opt,
            sort_var = filters()$sort_by
          ) |>
            display_bign_head(
              mentry_data = filters()$ment_out,
              trtbignyn = filters()$trtbign,
              subbignyn = "N"
            )
        )
        rv$toutput <- try(
          rv$outdata |>
            tbl_processor(keepvars = c("Risk Ratio (CI)", "P-value")) |>
            tbl_display() |>
            autofit()
        )
      })
      print("AE Risk table process end")
    }) %>%
      bindEvent(process_btn())

    output$table_UI <- renderUI({
      req(rv$toutput)
      ft <- rv$toutput %>%
        border_inner(officer::fp_border(color = "cadetblue")) %>%
        fontsize(size = 12, part = "header") %>%
        fontsize(size = 9, part = "body")
      htmltools_value(ft)
    })

    # output$t_title_UI <- renderText({
    #   req(rv$toutput)
    #   rpt_title <- str_replace_all(rv$toutput$title, "\n", "<br>")
    #   return(HTML(rpt_title))
    # })
    #
    # output$t_footnote_UI <- renderText({
    #   req(rv$toutput)
    #   rpt_ftnote <- str_replace_all(rv$toutput$footnote, "\n", "<br>")
    #   return(HTML(rpt_ftnote))
    # })

    reactive(rv$toutput)
  })
}
