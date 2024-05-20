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
#' download_report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("btn_down"),
      downloadButton(
        ns("save_btn"),
        "Save Report",
        class = "download-btn"
      )
    )
  )
}

#' download_report Server Functions
#'
#' @noRd
mod_download_report_server <- function(id, repType, repName, repNum, save_fmt, toutput, goutput) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      saveObj = NULL,
      Save_Filename = NULL,
      g_ind = FALSE,
      t_ind = FALSE
    )

    observe({
      req(repType())
      req(repName())
      req(repNum())
      req(save_fmt())
      rv$Save_Filename <- paste0(
        repName(), "_",
        str_replace(repNum(), "[[:punct:][:space:]]", "_"),
        str_replace_all(Sys.time(), "[[:punct:][:space:]]", "_"),
        ifelse(save_fmt() == "interactive", "_I.html", paste0(".", save_fmt()))
      )
    })

    observe({
      if (repType() == "Table") {
        req(toutput())
      } else {
        req(goutput())
      }

      print("create save report object start")

      if (!is.null(toutput()) || !is.null(goutput())) {
        if (repType() == "Table") {
          rv$t_ind <- TRUE
          rv$saveObj <- toutput()
        } else {
          rv$g_ind <- TRUE
          rv$saveObj <- goutput()
        }
      }
      print("save report object created")
    }) %>%
      bindEvent(list(repType(), toutput(), goutput(), save_fmt()))

    observe({
      if (is.null(rv$saveObj)) {
        disable("save_btn")
      } else {
        if (repType() == "Table") {
          toggleState("save_btn", condition = rv$t_ind)
        } else {
          toggleState("save_btn", condition = rv$g_ind)
        }
      }
    })

    ## Download the generated report if the data source is local or default
    output$save_btn <- downloadHandler(
      filename = function() {
        rv$Save_Filename
      },
      content = function(file) {
        withProgress(
          save_file(
            save_object = rv$saveObj,
            file_format = save_fmt(),
            report_type = repType(),
            report_name = repName(),
            file = file
          ),
          message = "Saving file...",
          detail = "This step should take a while.",
          min = 0,
          max = 1,
          value = 1
        )
      }
    )

    reactive(rv$Save_Filename)
  })
}
