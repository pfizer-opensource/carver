#' ae_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ae_inputs_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      id = ns("box_1"),
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
          uiOutput(ns("ae_catvar_UI"))
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
      id = ns("box_1"),
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
    )
  )
}
    
#' ae_inputs Server Functions
#'
#' @noRd 
mod_ae_inputs_server <- function(id, sourcedata, domain, repName, repType, trt_var, trt_sort, popfilter){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    rv <- reactiveValues(
      ae_pre = NULL,
      ae_pre_comp = 0,
      process_tornado_data = NA
    )

    observe({
      req(domain)
      hide("pctdisp")
      req(repName)
      req(repName %in% c("adae_tier_summary"))
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
      req(domain)
      req(repType)
      req(repName)
      if (toupper(domain) == "ADAE") {
        show("box_1")
        if (input$period == "Other") {
          show("period_spec")
        } else {
          hide("period_spec")
        }
      } else {
        hide("box_1")
      }
      if (repName %in%
          c("ae_forest_plot", "ae_volcano_plot", "adae_risk_summary", "tornado_plot")) {
        show("box_2")
      } else {
        hide("box_2")
      }
      if (repName %in% c("ae_forest_plot", "ae_volcano_plot")) {
        show("pvalcut")
        show("X_ref")
        show("pvalue_label")
      } else {
        hide("pvalcut")
        hide("X_ref")
        hide("pvalue_label")
      }
      
      if (repName %in% c("ae_forest_plot", "adae_tier_summary", "adae_risk_summary")) {
        show("sort_opt")
        show("sort_by")
      } else {
        hide("sort_opt")
        hide("sort_by")
      }
      
      if (repName == "ae_forest_plot") {
        show("riskScale")
      } else {
        hide("riskScale")
      }
      
      if (repName == "Event Analysis") {
        show("ref_line")
        hide("cutoff")
        updateSelectInput(
          session,
          "summary_by",
          choices = c("Events" = "Events", "Participants" = "Patients")
        )
      } else {
        hide("ref_line")
        show("cutoff")
      }
      
      if (repName == "tornado_plot") {
        hide("ui_hlt")
        hide("ae_hlt")
        hide("summary_by")
        hide("cutoff")
        hide("statistics")
        hide("alpha")
      } else {
        show("ui_llt")
        show("treatment1")
        show("treatment2")
        show("treatment1_label")
        show("treatment2_label")
      }
    })
    
    observe({
      req(repName)
      req(tolower(repName) %in% c("ae_forest_plot", "adae_tier_summary", "adae_risk_summary"))
      req(input$sort_opt)
      if (input$sort_opt != "Alphabetical") {
        if (tolower(repName) == "adae_risk_summary") {
          by_var <- c("Count", "Percent", "RiskValue")
        } else if (tolower(repName) == "ae_forest_plot") {
          by_var <- c("Count", "Percent", "RiskValue")
        } else if (tolower(repName) == "adae_tier_summary") {
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
      req(sourcedata)
      req(domain)
      req(repName)
      req(repType)
      req(input$ae_filter)
      req(input$a_subset)
      if (tolower(domain) == "adae") {
        print("AE preprocessing start")
        ### calling Pre Processing AE data
        withProgress(
          rv$ae_pre <- ae_pre_processor(
            datain = sourcedata[[domain]],
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
          repName, input$ae_filter, input$period, input$period_spec
        )
      )
    
    # Forest, Volcano processing
    observe({
      req(rv$ae_pre)
      if (tolower(repName) %in% c(
        "tornado_plot", "ae_volcano_plot",
        "ae_forest_plot", "adae_risk_summary"
      )) {
        print("AE treatment pair processing start")
        
        TRTCD <- rv$ae_pre$data |>
          filter(.data[[trt_var]] != "") |>
          pull() |>
          unique()
        
        ## Single pair radio button selection for Volcano plot
        output$treatment1_UI <- renderUI({
          req(tolower(repName) %in% c("tornado_plot", "ae_volcano_plot", "adae_risk_summary"))
          radioButtons(
            ns("treatment1"),
            if (repName == "tornado_plot") "Treatment Left" else "Control Group",
            choices = TRTCD,
            selected = TRTCD[1]
          )
        })
        
        output$treatment2_UI <- renderUI({
          req(tolower(repName) %in% c("tornado_plot", "ae_volcano_plot", "adae_risk_summary"))
          radioButtons(
            ns("treatment2"),
            if (repName == "tornado_plot") "Treatment Right" else "Treatment Group",
            choices = setdiff(TRTCD, input$treatment1),
            selected = setdiff(TRTCD, input$treatment1)[1]
          )
        })
        
        output$treatment1_label_UI <- renderUI({
          req(tolower(repName) %in% c("tornado_plot", "ae_volcano_plot"))
          textInput(ns("treatment1_label"),
                    if (repName == "tornado_plot") "Treatment Left Label" else "Label for Control Group", # nolint
                    value = if (repName == "tornado_plot") input$treatment1 else "Control"
          )
        })
        
        output$treatment2_label_UI <- renderUI({
          req(tolower(repName) %in% c("tornado_plot", "ae_volcano_plot"))
          textInput(ns("treatment2_label"),
                    if (repName == "tornado_plot") "Treatment Right Label" else "Label for Treatment Group", # nolint
                    value = if (repName == "tornado_plot") input$treatment2 else "Treatment"
          )
        })
        
        
        ## Multiple pair check box selection for forest plot
        
        output$ctrlgrp_UI <- renderUI({
          req(tolower(repName) == "ae_forest_plot")
          radioButtons(
            ns("ctrlgrp"),
            "Control Group",
            choices = TRTCD,
            selected = TRTCD[1],
            inline = FALSE
          )
        })
        
        output$trtgrp_UI <- renderUI({
          req(tolower(repName) == "ae_forest_plot")
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
      bindEvent(list(rv$ae_pre_comp))
    
    # Tornado Plot
    
    observe({
      req(sourcedata)
      req(domain)
      req(repName)
      req(repType)
      if (tolower(repName) %in% c("tornado_plot")) {
        output$ae_catvar_UI <- renderUI({
          selectInput(
            inputId = ns("ae_catvar"),
            label = "AE Categorical Variable",
            choices = colnames(sourcedata[[domain]])[
              colnames(sourcedata[[domain]]) %in% c("AESEV", "ASEV", "AETOXGR", "ATOXGR")
            ],
            selected = "AESEV"
          )
        })
        updateSelectInput(
          session,
          "ae_llt",
          selected = c("Primary System Organ Class (AESOC)" = "AESOC")
        )
      }
    }) %>%
      bindEvent(repName)

    return(
      list(
        ae_pre = rv$ae_pre,
        ae_filter = input$ae_filter,
        ae_hlt = input$ae_hlt,
        ae_llt = input$ae_llt,
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
        ref_line = input$ref_line,
        ae_catvar = input$ae_catvar
      )
    )
  })
}
