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
            "Overall Subset",
            value = "STUDYID != ''"
          )
        ),
        column(
          width = 4,
          textInput(
            inputId = ns("a_subset"),
            label = "Analysis Subset",
            value = "USUBJID != ''"
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          radioButtons(
            inputId = ns("trttotalyn"),
            label = "Total Treatment",
            choices = c("Y", "N"),
            selected = "N",
            inline = TRUE
          )
        ),
        column(
          width = 4,
          radioButtons(
            inputId = ns("trtbign"),
            label = "Display Treatment 'N'",
            choices = c("Y", "N"),
            selected = "N",
            inline = TRUE
          )
        ),
        column(
          width = 4,
          radioButtons(
            inputId = ns("grpvarmiss"),
            label = "Keep Missing Group",
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
          uiOutput(ns("ae_catvar_UI"))
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
          sliderInput(
            ns("cutoff"),
            "Cutoff of Incidence (%)",
            min = 0,
            max = 10,
            value = 5
          )
        ),
        column(
          width = 4,
          selectInput(
            ns("sort_opt"),
            "Sorting Option",
            choices = c("Ascending", "Descending", "Alphabetical")
          )
        ),
        column(
          width = 4,
          selectInput(
            ns("sort_by"),
            "Sorting Variable",
            choices = NULL
          )
        )
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
          ),
          selectInput(
            ns("pvalue_label"),
            "P-value Transformation",
            choices = c("None", "-log10"),
            selected = "None"
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
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 4,
          uiOutput(ns("treatment1_UI")),
          uiOutput(ns("treatment1_label_UI")),
          uiOutput(ns("ctrlgrp_UI"))
        ),
        column(
          width = 4,
          uiOutput(ns("treatment2_UI")),
          br(),
          uiOutput(ns("treatment2_label_UI")),
          uiOutput(ns("trtgrp_UI"))
        )
      )
    ),
    box(
      id = ns("adsl_1"),
      title = tags$strong("ADSL Summary Inputs"),
      maximizable = TRUE,
      width = 12,
      fluidRow(
        column(
          width = 4,
          uiOutput(ns("byvar_ui"))
        ),
        column(
          width = 4,
          textInput(
            ns("bylabel"),
            "By Group Label",
            value = NULL
          )
        ),
        column(
          width = 4,
          uiOutput(ns("subgrp_ui"))
        ),
        column(
          width = 4,
          uiOutput(ns("subtot_ui"))
        ),
        column(
          width = 4,
          uiOutput(ns("subbign_ui"))
        )
      ),
      fluidRow(
        column(width = 6, uiOutput(ns("dptvar_ui"))),
        column(width = 6, uiOutput(ns("dptlabel_ui")))
      ),
      fluidRow(
        column(
          width = 4,
          selectInput(
            ns("pctdisp_adsl"),
            "Percentage Denominator",
            choices = c(
              "Treatment" = "TRT", "None" = "NO", "Total" = "VAR",
              "Row-wise" = "CAT", "Column-wise" = "COL", "Treat-Subgrp" = "SUBGRP",
              "Group-category" = "DPTVAR"
            )
          )
        ),
        column(
          width = 4,
          radioButtons(
            inputId = ns("misscat"),
            label = "Display Missing Categories",
            choices = c("Y", "N"),
            selected = "N",
            inline = TRUE
          )
        ),
        column(
          width = 4,
          radioButtons(
            inputId = ns("totcat"),
            label = "Display Total Category row",
            choices = c("Y", "N"),
            selected = "N",
            inline = TRUE
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          textInput(
            ns("statvar"),
            label = "Statistics",
            value = "N~minmaxc~mean(sd)~Median~q1q3"
          )
        ),
        column(
          width = 4,
          textInput(
            ns("statlabel"),
            label = "Statistics Labels",
            value = "N~(Min,Max)~Mean (SD)~Median~(Q1,Q3)"
          )
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

      rv <- reactiveValues(
        ae_pre = NULL, ae_pre_comp = 0,
        ment_out = NULL, process_tornado_data = NA
      )

      # Generic Outputs change between graph and table:
      observe({
        req(domain())
        req(repType())
        if (tolower(repType()) == "table") {
          show("grpvarmiss")
        } else {
          hide("grpvarmiss")
        }
      })
      observe({
        req(domain())
        hide("pctdisp")
        req(repName())
        req(repName() %in% c("adae_tier_summary"))
        show("pctdisp")
        pct_denom <- c(
          "Treatment" = "TRT", "Total" = "VAR", "None" = "NO",
          "By High Term" = "BYVAR1N"
        )
        updateSelectInput(
          session,
          "pctdisp",
          choices = pct_denom
        )
      })

      observe({
        req(domain())
        req(repName())
        if (repName() == "eDISH_plot") {
          text <- "PARAMCD %in% c('ALT', 'AST', 'BILI')"
        } else {
          text <- "USUBJID != ''"
        }
        updateTextInput(
          session,
          "a_subset",
          "Analysis Subset",
          text
        )
      }) %>%
        bindEvent(repName())

      # observer to control showing/hiding inputs based on report selection
      observe({
        req(domain())
        req(repType())
        req(repName())
        if (toupper(domain()) == "ADAE") {
          show("box_2")
          if (input$period == "Other") {
            show("period_spec")
          } else {
            hide("period_spec")
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

        if (repName() == "tornado_plot") {
          hide("ui_llt")
          hide("ae_llt")
          hide("summary_by")
        } else {
          show("ui_hlt")
          show("treatment1")
          show("treatment2")
          show("treatment1_label")
          show("treatment2_label")
        }

        if (tolower(repName()) == "adsl_summary") {
          show("adsl_1")
        } else {
          hide("adsl_1")
        }
      })

      observe({
        req(repName())
        req(tolower(repName()) %in% c("ae_forest_plot", "adae_tier_summary", "adae_risk_summary"))
        req(input$sort_opt)
        if (input$sort_opt != "Alphabetical") {
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
        } else {
          updateSelectInput(
            session,
            "sort_by",
            choices = "None"
          )
        }
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
        req(!tolower(repName()) %in% c("tornado_plot", "km plot", "eDISH_plot"))
        byv <- NA
        subv <- NA
        subtotyn <- "N"
        if (tolower(domain()) == "adae") {
          req(rv$ae_pre)
          print("AE byVar processing start")
          ## evaluating the by variables based on report selection
          if (repType() == "Table" ||
            tolower(repName()) %in% c("ae_volcano_plot", "ae_forest_plot", "event analysis")) {
            byv <- input$ae_hlt
          }
          print("AE byVar processing end")
          datain <- rv$ae_pre[["data"]]
        } else {
          datain <- sourcedata()[[domain()]]
          ## Take input$byvar here, if applicable
        }
        if (repName() == "adsl_summary") {
          if (!is.null(input$byvar) && input$byvar != "") {
            byv <- input$byvar
          }
          if (!is.null(input$subgrp) && input$subgrp != "") {
            subv <- input$subgrp
            if (!is.null(input$subtotyn)) {
              subtotyn <- input$subtotyn
            }
          }
        }
        # Mentry processing - common
        print("Start Mentry process")
        withProgress(
          rv$ment_out <- mentry(
            datain = datain,
            subset = input$overall_subset,
            byvar = byv,
            subgrpvar = subv, # take input$subgrpvar as required here
            trtvar = toupper(trt_var()),
            trtsort = trt_sort(),
            pop_fil = str_trim(unlist(strsplit(
              unique(popfilter()), "~"
            ))[1]),
            trttotalyn = ifelse(repType() == "Table", input$trttotalyn, "N"),
            sgtotalyn = subtotyn,
            add_grpmiss = ifelse(repType() == "Table", input$grpvarmiss, "N")
          ),
          message = "Executing mentry processing...",
          detail = "This step should take a while.",
          min = 0,
          max = 1,
          value = 1
        )
        print("End Mentry process")
      }) |>
        bindEvent(
          list(
            repName(), input$overall_subset, input$trttotalyn, trt_var(),
            trt_sort(), popfilter(), input$ae_hlt, input$byvar, input$subgrp, input$subtotyn
          )
        )

      # Forest, Volcano processing
      observe({
        req(rv$ae_pre)
        req(rv$ment_out)
        if (tolower(repName()) %in% c(
          "tornado_plot", "ae_volcano_plot",
          "ae_forest_plot", "adae_risk_summary"
        )) {
          print("AE treatment pair processing start")

          TRTCD <- unique(rv$ment_out$TRTVAR[rv$ment_out$TRTVAR != ""])

          ## Single pair radio button selection for Volcano plot
          output$treatment1_UI <- renderUI({
            req(tolower(repName()) %in% c("tornado_plot", "ae_volcano_plot", "adae_risk_summary"))
            radioButtons(
              ns("treatment1"),
              if (repName() == "tornado_plot") "Treatment Left" else "Control Group",
              choices = TRTCD,
              selected = TRTCD[1]
            )
          })

          output$treatment2_UI <- renderUI({
            req(tolower(repName()) %in% c("tornado_plot", "ae_volcano_plot", "adae_risk_summary"))
            radioButtons(
              ns("treatment2"),
              if (repName() == "tornado_plot") "Treatment Right" else "Treatment Group",
              choices = setdiff(TRTCD, input$treatment1),
              selected = setdiff(TRTCD, input$treatment1)[1]
            )
          })

          output$treatment1_label_UI <- renderUI({
            req(tolower(repName()) %in% c("tornado_plot", "ae_volcano_plot"))
            textInput(ns("treatment1_label"),
              if (repName() == "tornado_plot") "Treatment Left Label" else "Label for Control Group", # nolint
              value = if (repName() == "tornado_plot") input$treatment1 else "Control"
            )
          })

          output$treatment2_label_UI <- renderUI({
            req(tolower(repName()) %in% c("tornado_plot", "ae_volcano_plot"))
            textInput(ns("treatment2_label"),
              if (repName() == "tornado_plot") "Treatment Right Label" else "Label for Treatment Group", # nolint
              value = if (repName() == "tornado_plot") input$treatment2 else "Treatment"
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
            req(tolower(repName()) == "ae_forest_plot")
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

      # Tornado Plot

      observe({
        req(sourcedata())
        req(domain())
        req(repName())
        req(repType())
        if (tolower(repName()) %in% c("tornado_plot")) {
          output$ae_catvar_UI <- renderUI({
            selectInput(
              inputId = ns("ae_catvar"),
              label = "AE Categorical Variable",
              choices = colnames(sourcedata()[[domain()]])[
                colnames(sourcedata()[[domain()]]) %in% c("AESEV", "ASEV", "AETOXGR", "ATOXGR")
              ],
              selected = "AESEV"
            )
          })
          updateSelectInput(
            session,
            "ae_hlt",
            selected = c("Primary System Organ Class (AESOC)" = "AESOC")
          )
        }
      }) %>%
        bindEvent(list(
          repName()
        ))

      # ADSL table
      observe({
        req(sourcedata())
        req(domain())
        req(repName())
        if (tolower(repName()) == "adsl_summary") {
          print("adsl inputs")
          tempdata <- sourcedata()[[domain()]]
          byvars <- names(which(sapply(tempdata, \(.) !is.numeric(.))))
          output$byvar_ui <- renderUI({
            req(tolower(repName()) == "adsl_summary")
            selectInput(
              ns("byvar"),
              "By group Variable",
              choices = byvars,
              multiple = TRUE,
              selected = NULL
            )
          })
          output$subgrp_ui <- renderUI({
            req(tolower(repName()) == "adsl_summary")
            if (!is.null(input$byvar) && input$byvar != "") {
              subvar <- byvars[!byvars %in% input$byvar]
            } else {
              subvar <- names(which(sapply(tempdata, \(.) !is.numeric(.))))
            }
            selectInput(
              ns("subgrp"),
              "Sub-group Variable",
              choices = subvar,
              multiple = TRUE,
              selected = NULL
            )
          })

          output$subbign_ui <- renderUI({
            req(input$subgrp)
            req(tolower(repName()) == "adsl_summary")
            radioButtons(
              inputId = ns("subbign"),
              label = "Display Subgroup 'N'",
              choices = c("Y", "N"),
              selected = "N",
              inline = TRUE
            )
          })
          output$subtot_ui <- renderUI({
            req(input$subgrp)
            req(tolower(repName()) == "adsl_summary")
            radioButtons(
              inputId = ns("subtotyn"),
              label = "Display Subgroup Total Column",
              choices = c("Y", "N"),
              selected = "N",
              inline = TRUE
            )
          })
          output$dptvar_ui <- renderUI({
            req(tolower(repName()) == "adsl_summary")
            textInput(
              ns("dptvar"),
              "Analysis Variables",
              value = "AGEGR1~AGE-S~SEX~RACE"
            )
          })
          output$dptlabel_ui <- renderUI({
            req(tolower(repName()) == "adsl_summary")
            textInput(
              ns("dptlabel"),
              "Analysis Variable Labels",
              value = "Age Group, n (%)~Age (Years)~Sex, n (%)~Race, n (%)"
            )
          })
        }
      }) |>
        bindEvent(repName())

      # ADSL table process start
      observe({
        req(sourcedata())
        req(domain())
        req(repName())
        req(tolower(repName()) == "adsl_summary")
        req(rv$ment_out)
        req(input$dptvar)
        req(input$statvar)
        req(input$a_subset)
        req(input$pctdisp_adsl)
        req(input$totcat)
        req(input$misscat)
        print("adsl_summary starts")
        withProgress(
          message = "Processing ADSL Summary",
          value = 1,
          {
            rv$ae_pre <- adsl_summary(
              datain = rv$ment_out,
              vars = input$dptvar,
              stat_vars = input$statvar,
              pctdisp = input$pctdisp_adsl,
              total_catyn = input$totcat,
              total_catlabel = "Total",
              miss_catyn = input$misscat,
              miss_catlabel = "Missing",
              a_subset = input$a_subset,
              denom_subset = NA_character_
            )
          }
        )
        print("adsl_summary ends")
      }) |>
        bindEvent(list(
          repName(), rv$ment_out, input$dptvar, input$statvar, input$pctdisp_adsl,
          input$totcat, input$misscat, input$a_subset
        ))

      observe({
        req(sourcedata())
        req(domain())
        req(repName())
        req(repType())
        req(trt_var())
        req(trt_sort())
        req(popfilter())
        req(input$ae_filter)
        req(input$ae_catvar)
        req(input$treatment1)
        req(input$treatment2)
        req(input$trtbign)
        req(input$ae_hlt)
        if (tolower(repName()) %in% c("tornado_plot")) {
          print("AE Tornado plot pre-processing start")

          data <- sourcedata()[[domain()]]

          ae_catvarN <- paste0(input$ae_catvar, "N")
          if (!(ae_catvarN %in% colnames(data))) {
            data <- data |>
              mutate(!!ae_catvarN := as.numeric(factor(.data[[input$ae_catvar]])))
          }

          data <- data |>
            mutate(SUBJID := .data[["USUBJID"]])

          rv$process_tornado_data <- process_tornado_data(
            dataset_adsl = NULL,
            dataset_analysis = data,
            adsl_subset = "",
            analysis_subset = input$a_subset,
            ae_filter = input$ae_filter,
            obs_residual = ifelse(input$period == "Other", input$period_spec, NA),
            fmq_data = utils::read.csv(paste0(
              app_sys("extdata"), "/FMQ_Consolidated_List.csv"
            )),
            split_by = NA,
            ae_catvar = input$ae_catvar,
            trtvar = trt_var(),
            trt_left = input$treatment1,
            trt_right = input$treatment2,
            trtsort = trt_sort(),
            pop_fil = popfilter(),
            pctdisp = "TRT",
            denom_subset = NA,
            legendbign = input$trtbign,
            yvar = input$ae_hlt
          )

          print("AE Tornado plot pre-processing end")
        }
      }) %>%
        bindEvent(list(
          repName(), trt_var(), trt_sort(), popfilter(),
          input$ae_filter, input$ae_catvar, input$period, input$period_spec,
          input$treatment1, input$treatment2, input$pctdisp, input$denom_subset,
          input$trtbign, input$ae_hlt
        ))

      observe({
        req(sourcedata())
        req(domain())
        req(trt_var())
        req(trt_sort())
        req(popfilter())
        req(repName() == "eDISH_plot")
        req(input$a_subset)
        req(input$overall_subset)
        print("Edish process start")

        merged_df <- sourcedata()$adsl %>%
          adsl_merge(
            dataset_add = filter(sourcedata()$adlb, !!!rlang::parse_exprs(input$a_subset))
          ) %>%
          mentry(
            subset = input$overall_subset,
            byvar = NA_character_,
            subgrpvar = NA_character_,
            trtvar = toupper(trt_var()),
            trtsort = trt_sort(),
            pop_fil = str_trim(unlist(strsplit(
              unique(popfilter()), "~"
            ))[1]),
            trttotalyn = ifelse(repType() == "Table", input$trttotalyn, "N"),
            sgtotalyn = "N",
            add_grpmiss = ifelse(repType() == "Table", input$grpvarmiss, "N")
          )
        rv$ae_pre <- merged_df %>%
          process_edish_data(
            alt_paramcd = "ALT",
            ast_paramcd = "AST",
            bili_paramcd = "BILI"
          )
        print("Edish process ends")
      }) %>%
        bindEvent(list(
          trt_var(), trt_sort(), popfilter(), input$a_subset, input$overall_subset
        ))

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
          a_subset = input$a_subset,
          trtbign = input$trtbign,
          ae_filter = input$ae_filter,
          ae_hlt = input$ae_hlt,
          ae_llt = input$ae_llt,
          summary_by = input$summary_by,
          treatment1 = input$treatment1,
          treatment2 = input$treatment2,
          treatment1_label = input$treatment1_label,
          treatment2_label = input$treatment2_label,
          process_tornado_data = rv$process_tornado_data,
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
          ref_line = input$ref_line,
          dptlabel = input$dptlabel,
          statlabel = input$statlabel,
          bylabel = input$bylabel,
          subbign = input$subbign
        )
      })
      filters
    })
  }
