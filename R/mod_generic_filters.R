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
#' generic_filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_generic_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      id = ns("box_1"),
      title = tags$strong("Generic Inputs"),
      maximizable = TRUE,
      width = 12,
      fluidRow(
        column(
          width = 4,
          textInput(ns("overall_subset"),
            "Overall Subset Condition",
            value = "USUBJID!=''"
          )
        ),
        column(
          width = 4,
          textInput(
            inputId = ns("a_subset"),
            label = "Analysis Subset",
            value = ""
          )
        ),
        column(
          width = 2,
          offset = 1,
          radioButtons(
            inputId = ns("trttotalyn"),
            label = "Total treatment",
            choices = c("Y", "N"),
            selected = "N",
            inline = TRUE
          )
        ),
        column(
          width = 2,
          radioButtons(
            inputId = ns("grpvarmiss"),
            label = "Keep Missing Group Variable",
            choices = c("Y", "N"),
            selected = "N",
            inline = TRUE
          )
        )
      )
    ),
    box(
      id = ns("box_2"),
      title = tags$strong("Adverse Events Inputs"),
      maximizable = TRUE,
      width = 12,
      fluidRow(
        column(
          width = 4,
          selectInput(
            ns("ae_filter"),
            "Adverse Event Filter(s)",
            choices = c(
              "Any", "Treatment Emergent", "Serious", "Drug-related", "Mild", "Moderate",
              "Severe", "Recovered/Resolved", "Recovering/Resolving",
              "Not Recovered/Not Resolved", "Fatal"
            ),
            selected = "Any",
            multiple = TRUE
          )
        ),
        column(
          width = 4,
          selectInput(
            ns("period"),
            "Period",
            choices = c("Overall Duration", "Other")
          )
        ),
        column(
          width = 4,
          numericInput(
            ns("period_spec"),
            HTML("Residual period (in days)"),
            value = 30,
            min = 0,
            max = 10^5
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          selectInput(
            ns("ae_hlt"),
            "Higher Level Event Term",
            choices = get_ae_term(),
            selected = c("Body System or Organ Class (AEBODSYS)" = "AEBODSYS")
          )
        ),
        column(
          width = 3,
          selectInput(
            ns("ae_llt"),
            "Event Term",
            choices = get_ae_term(),
            selected = c("AE Dictionary-Derived Term (AEDECOD)" = "AEDECOD")
          )
        ),
        column(
          width = 3,
          selectInput(
            ns("summary_by"),
            "Summary By",
            choices = c("Participants" = "Patients", "Events" = "Events")
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = ns("pctdisp"),
            label = "Percentage Denominator",
            choices = NULL
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          selectInput(
            ns("sort_by"),
            "Sorting Variable",
            choices = NULL
          )
        ),
        column(
          width = 4,
          sliderInput(
            ns("cutoff"),
            "Cutoff of Incidence (%)",
            min = 0,
            max = 10,
            value = 5
          )
        ),
      )
    ),
    box(
      id = ns("box_3"),
      title = tags$strong("AE Statistics"),
      maximizable = TRUE,
      width = 12,
      fluidRow(
        column(
          width = 4,
          selectInput(
            ns("statistics"),
            "Measure of Association",
            choices = c("Risk Ratio", "Risk Difference")
          ),
          numericInput(
            ns("X_ref"),
            "Risk Reference Lines",
            value = 0,
          )
        ),
        column(
          width = 4,
          numericInput(
            ns("alpha"),
            "Alpha Value(CI)",
            value = 0.05,
            min = 0.01,
            max = 0.1
          ),
          selectInput(
            ns("riskScale"),
            "Risk Axis Scale",
            choices = c("Log10", "Identity", "Log2")
          )
        ),
        column(
          width = 4,
          numericInput(
            ns("pvalcut"),
            "p Value Cutoff",
            value = 0.05,
            min = 0.01,
            max = 0.1
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          numericInput(
            ns("ref_line"),
            "Reference Line (%)",
            min = 0,
            max = 100,
            value = 5
          )
        ),
        column(
          width = 4,
          selectInput(
            ns("pvalue_label"),
            "P-value Transformation",
            choices = c("None", "-log10"),
            selected = "None"
          )
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 4,
          uiOutput(ns("treatment1_UI")),
          uiOutput(ns("treatment1_label_UI")),
          uiOutput(ns("ctrlgrp_UI")),
          selectInput(
            ns("sort_opt"),
            "Sorting Option",
            choices = c("Ascending", "Descending", "Alphabetical")
          )
        ),
        column(
          width = 4,
          uiOutput(ns("treatment2_UI")),
          br(),
          uiOutput(ns("treatment2_label_UI")),
          uiOutput(ns("trtgrp_UI"))
        )
      )
    )
  )
}

#' generic_filters Server Functions
#'
#' @noRd
mod_generic_filters_server <-
  function(id, sourcedata, domain, repName, repType, trt_var, trt_sort, popfilter) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      rv <- reactiveValues(ae_pre = NULL, ae_pre_comp = 0, mentry_out = NULL)

      observe({
        req(domain())
        hide("pctdisp")
        req(repName())
        req(repName() %in% c("adae_tier_summary", "adae_risk_summary", "Event Analysis"))
        show("pctdisp")
        if (repName() == "adae_tier_summary") {
          pct_denom <- c(
            "Treatment" = "TRT", "Total" = "VAR", "None" = "NO",
            "By High Term" = "BYVAR1N"
          )
        } else {
          pct_denom <- c("Treatment" = "TRT")
        }
        updateSelectInput(
          session,
          "pctdisp",
          choices = pct_denom
        )
      })

      # observer to control showing/hiding inputs based on report selection
      observe({
        req(domain())
        req(repType())
        req(repName())
        if (domain() == "ADAE") {
          show("box_2")
          if (input$period == "Other") {
            show("period_spec")
          } else {
            hide("period_spec")
          }
          if (!repName() %in% "tornado_plot") {
            show("ui_hlt")
            show("pctdisp")
          } else {
            hide("ui_hlt")
            hide("pctdisp")
          }
        } else {
          hide("box_2")
        }

        if (repName() %in% c("ae_forest_plot", "ae_volcano_plot", "adae_risk_summary")) {
          show("box_3")
        } else {
          hide("box_3")
        }
        if (repName() %in% c("ae_forest_plot", "ae_volcano_plot")) {
          show("pvalcut")
          show("X_ref")
          show("pvalue_label")
        } else {
          hide("pvalcut")
          hide("X_ref")
          hide("pvalue_label")
        }

        if (repName() %in% c("ae_forest_plot", "adae_tier_summary", "adae_risk_summary")) {
          show("sort_opt")
          show("sort_by")
        } else {
          hide("sort_opt")
          hide("sort_by")
        }

        if (repName() == "ae_forest_plot") {
          show("riskScale")
        } else {
          hide("riskScale")
        }

        if (repName() == "Event Analysis") {
          show("ref_line")
          hide("summary_by")
          hide("cutoff")
        } else {
          hide("ref_line")
          show("summary_by")
          show("cutoff")
        }
      })

      observe({
        req(repName())
        req(tolower(repName()) %in% c("ae_forest_plot", "adae_tier_summary", "adae_risk_summary"))
        req(input$sort_opt != "Alphabetical")

        if (tolower(repName()) == "adae_risk_summary") {
          by_var <- c("Count", "Percent", "RiskValue")
        } else if (tolower(repName()) == "ae_forest_plot") {
          by_var <- c("Count", "Percent", "RiskValue")
        } else if (tolower(repName()) == "adae_tier_summary") {
          by_var <- c("Count", "Percent")
        }

        updateSelectInput(
          session,
          "sort_by",
          choices = by_var
        )
        print("AE Sort Variable updated")
      })

      observe({
        req(sourcedata())
        req(domain())
        req(repName())
        req(repType())
        req(input$ae_filter)
        if (tolower(domain()) == "adae") {
          print("AE preprocessing start")
          ### calling Pre Processing AE data
          withProgress(
            rv$ae_pre <- ae_pre_processor(
              datain = sourcedata()[[domain()]],
              fmq_data = utils::read.csv(paste0(
                app_sys("extdata"), "/FMQ_Consolidated_List.csv"
              )),
              ae_filter = input$ae_filter,
              obs_residual = ifelse(input$period == "Other", input$period_spec, NA)
            ),
            message = "Executing pre processing for AE...",
            min = 0,
            max = 1,
            value = 1
          )
          print("AE preprocessing end")

          rv$ae_pre_comp <- rv$ae_pre_comp + 1
        }
      }) %>%
        bindEvent(
          list(
            repName(), input$ae_filter, input$period, input$period_spec
          )
        )

      observe({
        req(sourcedata())
        req(domain())
        req(repName())
        req(repType())
        req(trt_var())
        req(trt_sort())
        req(popfilter())
        req(input$overall_subset)
        if (tolower(domain()) == "adae") {
          req(rv$ae_pre)
          print("AE byVar processing start")
          ## evaluating the by variables based on report selection
          if (repType() == "Table") {
            byv <- input$ae_hlt
          } else {
            if (tolower(repName()) %in% c("ae_volcano_plot", "ae_forest_plot", "event analysis")) {
              byv <- input$ae_hlt
            } else {
              byv <- NA
            }
          }
          print("AE byVar processing end")
        } else {
          byv <- NA
          ## Take input$byvar here, if applicable
        }
        # Mentry processing - common
        if (!tolower(repName()) %in% c("tornado_plot", "event analysis", "km plot")) {
          print("Start Mentry process")
          if (toupper(domain()) == "ADAE") {
            datain <- rv$ae_pre[["data"]]
          } else {
            datain <- sourcedata()[[domain()]]
          }
          withProgress(
            rv$ment_out <- mentry(
              datain = datain,
              subset = input$overall_subset,
              byvar = byv,
              subgrpvar = NA, # take input$subgrpvar as required here
              trtvar = toupper(trt_var()),
              trtsort = trt_sort(),
              pop_fil = str_trim(unlist(strsplit(
                unique(popfilter()), "~"
              ))[1]),
              trttotalyn = ifelse(repType() == "Table", input$trttotalyn, "N"),
              sgtotalyn = "N",
              add_grpmiss = ifelse(repType() == "Table", input$grpvarmiss, "N")
            ),
            message = "Executing mentry processing...",
            detail = "This step should take a while.",
            min = 0,
            max = 1,
            value = 1
          )
        }
      }) |>
        bindEvent(
          list(
            repName(), input$overall_subset, input$grpvarmiss, input$trttotalyn, trt_var(),
            trt_sort(), popfilter()
          )
        )

      # Forest, Volcano processing
      observe({
        req(rv$ae_pre)
        req(rv$ment_out)
        if (tolower(repName()) %in% c("ae_volcano_plot", "ae_forest_plot", "adae_risk_summary")) {
          print("AE treatment pair processing start")

          TRTCD <- unique(rv$ment_out$TRTVAR[rv$ment_out$TRTVAR != ""])

          ## Single pair radio button selection for Volcano plot
          output$treatment1_UI <- renderUI({
            req(tolower(repName()) %in% c("ae_volcano_plot", "adae_risk_summary"))
            radioButtons(
              ns("treatment1"),
              "Control Group",
              choices = TRTCD,
              selected = TRTCD[1]
            )
          })

          output$treatment2_UI <- renderUI({
            req(tolower(repName()) %in% c("ae_volcano_plot", "adae_risk_summary"))
            radioButtons(
              ns("treatment2"),
              "Treatment Group",
              choices = setdiff(TRTCD, input$treatment1),
              selected = setdiff(TRTCD, input$treatment1)[1]
            )
          })

          output$treatment1_label_UI <- renderUI({
            req(tolower(repName()) == "ae_volcano_plot")
            textInput(ns("treatment1_label"),
              "Label for Control Group",
              value = "Control"
            )
          })

          output$treatment2_label_UI <- renderUI({
            req(tolower(repName()) == "ae_volcano_plot")
            textInput(ns("treatment2_label"),
              "Label for Treatment Group",
              value = "Treatment"
            )
          })


          ## Multiple pair check box selection for forest plot

          output$ctrlgrp_UI <- renderUI({
            req(tolower(repName()) == "ae_forest_plot")
            radioButtons(
              ns("ctrlgrp"),
              "Control Group",
              choices = TRTCD,
              selected = TRTCD[1],
              inline = FALSE
            )
          })

          output$trtgrp_UI <- renderUI({
            req(tolower(repName()) == "forest plot")
            checkboxGroupInput(
              ns("trtgrp"),
              "Treatment Group",
              choices = setdiff(TRTCD, input$ctrlgrp),
              selected = setdiff(TRTCD, input$ctrlgrp)[1]
            )
          })
          print("AE treatment pair processing end")
        }
      }) %>%
        bindEvent(list(rv$ae_pre_comp, rv$ment_out))

      filters <- reactive({
        req(rv$ae_pre)
        req(input$ae_filter)
        req(rv$ment_out)
        if (repName() == "adae_risk_summary") {
          req(input$treatment1)
          req(input$treatment2)
          req(input$statistics)
          req(input$alpha)
        }
        if (repName() == "ae_volcano_plot") {
          req(input$treatment1)
          req(input$treatment2)
          req(input$treatment1_label)
          req(input$treatment2_label)
        }
        if (tolower(repName()) == "ae_forest_plot") {
          req(input$ctrlgrp)
          req(input$trtgrp)
          req(!identical(input$ctrlgrp, input$trtgrp))
        }

        list(
          ae_pre = rv$ae_pre,
          ment_out = rv$ment_out,
          trt_var = trt_var(),
          ae_filter = input$ae_filter,
          ae_hlt = input$ae_hlt,
          ae_llt = input$ae_llt,
          summary_by = input$summary_by,
          treatment1 = input$treatment1,
          treatment2 = input$treatment2,
          treatment1_label = input$treatment1_label,
          treatment2_label = input$treatment2_label,
          statistics = input$statistics,
          alpha = input$alpha,
          ui_pctdisp = input$pctdisp,
          cutoff = input$cutoff,
          sort_opt = input$sort_opt,
          sort_by = input$sort_by,
          ctrlgrp = input$ctrlgrp,
          trtgrp = input$trtgrp,
          pvalcut = input$pvalcut,
          riskScale = input$riskScale,
          X_ref = input$X_ref,
          pvalue_label = input$pvalue_label,
          ref_line = input$ref_line
        )
      })
      filters
    })
  }
