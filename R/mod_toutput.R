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
      tout = NULL,
      title = "",
      footnote = ""
    )


    observe({
      req(repName())
      req(popfilter())
      req(filters())
      if (tolower(repName()) %in% c("adae_risk_summary", "adae_tier_summary")) {
        req(filters()$ae_pre)
        req(filters()$ae_filter)
        req(filters()$ae_llt)
        req(filters()$ae_hlt)
        req(filters()$summary_by)
        req(filters()$trtbign)
        req(filters()$cutoff)
        req(filters()$sort_opt)
        req(filters()$sort_by)
        print("ADAE table process start")
        # Title and Footnote
        rv$title <- paste0(
          "Participants With ", filters()$ae_filter,
          " Adverse Events by Higher Term and Lower Term \n",
          "Population: ",
          popfilter()
        )
        rv$footnote <- paste0(
          "* n is the number of participants with ", filters()$ae_filter,
          " adverse events."
        )
        if (tolower(repName()) == "adae_risk_summary") {
          req(filters()$ment_out)
          req(filters()$a_subset)
          req(filters()$treatment1)
          req(filters()$treatment2)
          req(filters()$statistics)
          req(filters()$alpha)
          withProgress(message = "Generating AE Risk table", value = 0, {
            rv$outdata <- try(
              adae_risk_summary(
                filters()$ment_out,
                a_subset = filters()$ae_pre$a_subset,
                summary_by = filters()$summary_by,
                hterm = filters()$ae_hlt,
                lterm = filters()$ae_llt,
                ctrlgrp = filters()$treatment1,
                trtgrp = filters()$treatment2,
                statistics = filters()$statistics,
                alpha = filters()$alpha,
                cutoff_where = paste0("PCT > ", filters()$cutoff),
                sort_opt = filters()$sort_opt,
                sort_var = filters()$sort_by
              )
            )
            keepvars <- c(paste(filters()$statistics, "(CI)"), "P-value")
            rv$footnote <- paste0(
              rv$footnote, "\n", filters()$statistics, " is shown between ",
              filters()$treatment1, " and ", filters()$treatment2
            )
            print("AE Risk table process end")
          })
        } else {
          req(filters()$ui_pctdisp)
          rv$outdata <- try(
            occ_tier_summary(
              filters()$ment_out,
              a_subset = filters()$ae_pre$a_subset,
              summary_by = filters()$summary_by,
              hterm = filters()$ae_hlt,
              lterm = filters()$ae_llt,
              pctdisp = filters()$ui_pctdisp,
              cutoff_where = paste0("PCT > ", filters()$cutoff),
              apply_hrow_cutoff = "N",
              sort_opt = filters()$sort_opt,
              sort_var = filters()$sort_by
            )
          )
          keepvars <- ""
          print("AE Summary table process end")
          rv$footnote <- paste0(
            rv$footnote, " \n Cut off >", filters()$cutoff,
            "% is applied only to event term"
          )
        }
        if (ncol(rv$outdata) == 1) {
          rv$tout <- flextable(rv$outdata) |>
            autofit()
        } else {
          rv$outdata <- rv$outdata |>
            display_bign_head(
              mentry_data = filters()$ment_out,
              trtbignyn = filters()$trtbign,
              subbignyn = "N"
            )
          rv$tout <- try(
            rv$outdata |>
              tbl_processor(keepvars = keepvars) |>
              tbl_display() |>
              autofit()
          )
        }
      } else if (tolower(repName()) == "adsl_summary") {
        print("ADSL Summary Output starts")
        req(filters()$ae_pre)
        req(filters()$ment_out)
        if (!is.null(filters()$bylabel)) {
          bylabel <- filters()$bylabel
        } else {
          bylabel <- NA_character_
        }
        rv$title <- paste0("Demographic Summary Table \n", popfilter(), " population")
        withProgress(message = "Generating ADSL Summary table", value = 1, {
          rv$outdata <- try(
            display_bign_head(
              datain = filters()$ae_pre,
              mentry_data = filters()$ment_out,
              trtbignyn = filters()$trtbign,
              subbignyn = ifelse(!is.null(filters()$subbign), filters()$subbign, "N")
            ) |>
              tbl_processor(
                dptlabel = filters()$dptlabel,
                statlabel = filters()$statlabel,
                addrowvars = "DPTVAR"
              )
          )
          print("Out data created")
          rv$tout <- try(
            tbl_display(
              datain = rv$outdata,
              bylabel = bylabel
            ) |>
              autofit()
          )
        })
        print("ADSL Summary Output ends")
      }
    }) %>%
      bindEvent(process_btn())

    output$table_UI <- renderUI({
      req(rv$tout)
      ft <- rv$tout %>%
        border_inner(officer::fp_border(color = "cadetblue")) %>%
        fontsize(size = 12, part = "header") %>%
        fontsize(size = 9, part = "body")
      htmltools_value(ft)
    })

    output$t_title_UI <- renderText({
      req(rv$title)
      rpt_title <- str_replace_all(rv$title, "\n", "<br>")
      return(HTML(rpt_title))
    })

    output$t_footnote_UI <- renderText({
      req(rv$footnote)
      rpt_ftnote <- str_replace_all(rv$footnote, "\n", "<br>")
      return(HTML(rpt_ftnote))
    })

    reactive(list(
      outdata = rv$outdata,
      tout = rv$tout,
      title = rv$title,
      footnote = rv$footnote
    ))
  })
}
