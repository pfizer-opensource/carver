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
#' @param subset Analysis subset condition to be applied to `ADAE` dataset prior to ADSL join;
#' will be appended to `ae_filter`
#' @param obs_residual If not NA, use this argument to pass a period (numeric) to extend the
#' observation period. If passed as NA, overall study duration is considered for analysis.
#' eg. if 5, only events occurring upto 5 days past the TRTEDT are considered.
#' @param max_sevctc If needed to filter maximum severity/ctc grade rows. Values: NA/"SEV"/"CTC"
#' @param sev_ctcvar Variable to determine max severity. eg: ASEVN, ATOXGRN
#' @param hterm High Level Event Term (req for max Sev tables only)
#' @param lterm Low Level Event Term (req for max Sev tables only)
#' @param rpt_byvar Page/report by variable if any, to identify max sev/ctc
#' @param trtvar Treatment Variable
#' @param pt_total Required to calculate total of preferred terms? Y/N
#'
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
                             subset = NA,
                             obs_residual = NA_real_,
                             max_sevctc = NA_character_,
                             sev_ctcvar = "ASEVN",
                             hterm = "AEBODSYS",
                             lterm = "AEDECOD",
                             rpt_byvar = character(0),
                             trtvar = "TRTA",
                             pt_total = "N") {
  if (nrow(datain) == 0) {
    return(list(data = datain, a_subset = NA_character_))
  }
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
  if (!is.na(subset)) {
    filters <- paste(na.omit(c(filters, subset)), collapse = " & ")
  }
  # filter for events occurring in given observation period
  obs_residual <- as.numeric(obs_residual)
  if (!is.na(obs_residual) && obs_residual >= 0) {
    stopifnot(
      "Obs period cannot be used; dates unavailable" =
        all(c("ASTDT", "TRTSDT", "TRTEDT") %in% names(data_pro))
    )
    filters <- paste(na.omit(
      c(filters, glue("(ASTDT > TRTSDT) & (ASTDT < (TRTEDT + {obs_residual}))"))
    ), collapse = " & ")
  }

  # Apply AE filters if exist:
  if (!is.na(filters) && filters != "") {
    data_pro <- data_pro |>
      filter(!!!parse_exprs(filters))
    if (nrow(data_pro) < 1) {
      return(list(data = data_pro, a_subset = filters))
    }
  }
  ################### Max SEV/CTC##############
  # If maximum severity or CTC required:
  # Filter analysis dataset and also flag max variable
  # Any AE flag to be set
  # Flag for high level term and max sevc/ctc
  if (!is.na(max_sevctc) && toupper(max_sevctc) %in% c("SEV", "CTC")) {
    data_pro <- data_pro |>
      group_by(across(
        any_of(c(rpt_byvar, trtvar, "USUBJID", hterm, lterm))
      )) |>
      mutate(
        MAX_SEVCTC = ifelse(.data[[sev_ctcvar]] == max(.data[[sev_ctcvar]], na.rm = TRUE), 1, 0)
      ) |>
      filter(MAX_SEVCTC == 1) |>
      group_by(across(any_of(c(rpt_byvar, trtvar, "USUBJID")))) |>
      mutate(
        ANY = ifelse(.data[[sev_ctcvar]] == max(.data[[sev_ctcvar]], na.rm = TRUE), 1, 0)
      ) |>
      group_by(across(
        any_of(c(rpt_byvar, trtvar, "USUBJID", hterm))
      )) |>
      mutate(
        HT_FL = ifelse(.data[[sev_ctcvar]] == max(.data[[sev_ctcvar]], na.rm = TRUE), 1, 0)
      )
    # For preferred term total count
    if (toupper(max_sevctc) == "SEV" && pt_total == "Y") {
      data_pro <- data_pro |>
        group_by(across(any_of(c(rpt_byvar, trtvar, lterm, "USUBJID")))) |>
        mutate(
          PT_CNT = ifelse(.data[[sev_ctcvar]] == max(.data[[sev_ctcvar]], na.rm = TRUE), 1, 0)
        )
    }
    filters <- paste(na.omit(c(filters, "MAX_SEVCTC == 1")), collapse = " & ")
  }
  ################### ENDax SEV/CTC##############

  # Return processed dataframe and filter conditions
  return(list(data = ungroup(data_pro), a_subset = filters))
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
