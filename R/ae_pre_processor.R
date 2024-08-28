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
#' Pre-processing data for Adverse Event domain specific reports
#'
#' @param datain the input ADaM (*ADAE*) dataset.
#' @param date_vars Vector of variables to be converted to R date format
#' @param fmq_data FMQ table dataframe, if passed, will be merged to adae date by PT.
#' @param ae_filter Vector of adverse event types to be used as filter conditions.
#' Permissible Values: "ANY", "ANY EVENT", "TREATMENT EMERGENT", "SERIOUS",
#' "DRUG-RELATED", "RELATED", "MILD", "MODERATE", "SEVERE", "RECOVERED/RESOLVED",
#' "RECOVERING/RESOLVING", "NOT RECOVERING/NOT RESOLVING", "FATAL", "GRADE N"
#' @param obs_residual If not NA, use this argument to pass a period (numeric) to extend the
#' observation period. If passed as NA, overall study duration is considered for analysis.
#' eg. if 5, only events occurring upto 5 days past the TRTEDT are considered.
#' @return : a list containing 2 objects
#'  \itemize{
#'  \item data - Processed dataframe output for further utilities (pass to `mentry()`)
#'  \item a_subset - Analysis Subset condition as a string
#'  (pass to `mentry()` OR further utilities as required)
#'  }
#'
#' @export
#'
#' @examples
#' data(adae)
#' data(FMQ_Consolidated_List)
#'
#' data_pre <- ae_pre_processor(
#'   datain = adae,
#'   ae_filter = "Serious",
#'   obs_residual = 0,
#'   fmq_data = FMQ_Consolidated_List
#' )
#' data_pre$data
#' data_pre$a_subset
ae_pre_processor <- function(datain,
                             fmq_data = NULL,
                             date_vars = c("ASTDT", "AENDT", "TRTSDT", "TRTEDT"),
                             ae_filter = "Any Event",
                             obs_residual = NA_real_) {
  stopifnot("Empty Data Frame passed" = nrow(datain) != 0)
  # Processing FMQ values if exists
  if (is.data.frame(fmq_data)) {
    fmq <- fmq_data |>
      mutate(
        FMQ_NAM = paste0(.data[["FMQ"]], "/", .data[["FMQCAT"]]),
        PT = str_trim(toupper(.data[["PT"]]))
      ) |>
      group_by(PT) |>
      summarise(FMQ_NAM = paste(FMQ_NAM, collapse = "~~"))
    # merging FMQ to AE ADaM Data
    datain <- datain |>
      mutate(PT = str_trim(toupper(.data[["AEDECOD"]]))) |>
      left_join(fmq, by = "PT")
  }

  # Standardizing date format to common format
  data_pro <- datain |>
    mutate(across(
      any_of(date_vars),
      ~ as.Date(.x, tryFormats = c("%d%b%Y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y"), optional = FALSE)
    ))
  # Marking non-coded AE terms
  data_pro <- data_pro |>
    filter(if_any(any_of("TRTSDT"), ~ !is.na(.x)))
  if ("ASTDT" %in% names(data_pro) && any(is.na(data_pro[["AEDECOD"]]))) {
    data_pro <- data_pro |>
      mutate(AEDECOD = if_else(!is.na(.data[["ASTDT"]]) & is.na(.data[["AEDECOD"]]),
        "Not Yet Coded", .data[["AEDECOD"]]
      ))
  }
  # AE-Specific filter conditions
  filters <- get_ae_filter(
    data_pro,
    ae_filter
  )

  # filter for events occurring in given observation period
  obs_residual <- as.numeric(obs_residual)
  if (!is.na(obs_residual) && obs_residual >= 0) {
    stopifnot(
      "Obs period cannot be used; dates unavailable" =
        all(c("ASTDT", "TRTSDT", "TRTEDT") %in% names(data_pro))
    )
    filters <- c(filters, glue("(ASTDT > TRTSDT) & (ASTDT < (TRTEDT + {obs_residual}))")) |>
      na.omit() |>
      paste(collapse = " & ")
  }
  # Return processed dataframe and filter conditions
  return(list(data = data_pro, a_subset = filters))
}

#' Create filter condition for Adverse Events from keyword
#'
#' @param ae_filter Events to be filtered.
#' Permissible Values: "ANY", "ANY EVENT", "TREATMENT EMERGENT", "SERIOUS",
#' "DRUG-RELATED", "RELATED", "MILD", "MODERATE", "SEVERE", "RECOVERED/RESOLVED",
#' "RECOVERING/RESOLVING", "NOT RECOVERING/NOT RESOLVING", "FATAL", "GRADE N"
#'
#' @return a string to act as filter condition for AE dataset
#'
#' @examples
#' data(adae)
#' get_ae_filter(adae, "Serious")
#'
#' @noRd
#'
get_ae_filter <- function(datain,
                          ae_filter) {
  ae_filter <- toupper(ae_filter)
  # No filter if Any Event
  if (all(is.na(ae_filter)) || all(ae_filter %in% c("ANY EVENT", "ANY", ""))) {
    return(NA)
  }
  filters_data <- tibble::tribble(
    ~AETYP, ~CONDN,
    "TREATMENT EMERGENT", "TRTEMFL == 'Y'",
    "SERIOUS", "AESER == 'Y'",
    "DRUG-RELATED", "AEREL == 'RELATED'",
    "RELATED", "AEREL == 'RELATED'",
    "MILD", "ASEV == 'MILD'",
    "MODERATE", "ASEV == 'MODERATE'",
    "SEVERE", "ASEV == 'SEVERE'",
    "RECOVERED/RESOLVED", "AEOUT == 'RECOVERED/RESOLVED'",
    "RECOVERING/RESOLVING", "AEOUT == 'RECOVERING/RESOLVING'",
    "NOT RECOVERED/NOT RESOLVED", "AEOUT == 'NOT RECOVERED/NOT RESOLVED'",
    "FATAL", "AEOUT == 'FATAL'"
  )
  gr <- ae_filter[grepl("GRADE", ae_filter)]
  if (length(gr) > 0) {
    filters_data <- filters_data |>
      bind_rows(
        data.frame(AETYP = gr, CONDN = glue("ATOXGR == '{gr}'"))
      )
  }
  stopifnot(
    "Invalid Adverse Event Filter" =
      any(ae_filter %in% unique(filters_data$AETYP))
  )
  condn <- filters_data |>
    filter(.data[["AETYP"]] %in% ae_filter) |>
    pull(.data[["CONDN"]]) |>
    na.omit()
  aevars <- unlist(map(strsplit(condn, " "), 1))
  nevar <- aevars[!aevars %in% names(datain)]
  if (length(nevar) > 0) {
    stop(paste(paste(nevar, collapse = ","), "not found in data. Cannot apply ae_filter"))
  }
  condn |>
    paste(collapse = " & ")
}
