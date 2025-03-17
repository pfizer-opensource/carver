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
        ),
        fluidRow(
          column(
            width = 4,
            tagAppendAttributes(
              actionButton(
                inputId = ns("apply_gen_filt"),
                label = "Apply"
              ),
              class = "sidebar-btn"
            )
          )
        )
      )
    ),
    mod_ae_inputs_ui(ns("ae_inputs_1")),
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

        if (tolower(repName()) == "adsl_summary") {
          show("adsl_1")
        } else {
          hide("adsl_1")
        }
      })
      
      ae_inputs <-
        mod_ae_inputs_server(
          "ae_inputs_1",
          sourcedata(),
          domain(),
          repName(),
          repType(),
          trt_var(),
          trt_sort(),
          popfilter()
        )

      observe({
        req(sourcedata())
        req(domain())
        req(repName())
        req(repType())
        req(input$ae_filter)
        req(input$a_subset)
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
              obs_residual = ifelse(input$period == "Other", input$period_spec, NA),
              subset = input$a_subset
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
            repName(), input$apply_gen_filt, trt_var(),
            trt_sort(), popfilter(), input$ae_hlt, input$byvar, input$subgrp, input$subtotyn
          )
        )

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
          input$totcat, input$misscat, input$apply_gen_filt
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
        req(input$ae_llt)
        if (tolower(repName()) %in% c("tornado_plot")) {
          print("AE Tornado plot pre-processing start")

          data <- sourcedata()[[domain()]]
          if (!is.null(sourcedata()[["ADSL"]])) {
            data_adsl <- sourcedata()[["ADSL"]]
          } else {
            data_adsl <- NULL
          }
          ae_catvarN <- paste0(input$ae_catvar, "N")
          if (!(ae_catvarN %in% colnames(data))) {
            data <- data |>
              mutate(!!ae_catvarN := as.numeric(factor(.data[[input$ae_catvar]])))
          }

          data <- data |>
            mutate(SUBJID := .data[["USUBJID"]])

          rv$process_tornado_data <- process_tornado_data(
            dataset_adsl = data_adsl,
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
            yvar = input$ae_llt
          )

          print("AE Tornado plot pre-processing end")
        }
      }) %>%
        bindEvent(list(
          repName(), trt_var(), trt_sort(), popfilter(),
          input$ae_filter, input$ae_catvar, input$period, input$period_spec,
          input$treatment1, input$treatment2, input$pctdisp, input$apply_gen_filt,
          input$ae_hlt
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
          trt_var(), trt_sort(), popfilter(), input$apply_gen_filt
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
        if (repName() %in% c("tornado_plot", "ae_volcano_plot")) {
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
          ae_catvar = input$ae_catvar,
          bylabel = input$bylabel,
          subbign = input$subbign
        )
      })
      filters
    })
  }
