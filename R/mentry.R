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
#' Read and process data with subsets and variables
#'
#' @description
#'
#' The input data gets processed based on `TRT`, `BY` and/or `SUBGROUP` variables and filtered by
#' subset conditions. This processed data flows into categorical and numerical utilities such as
#' `mcatstat()` and `msumstat()` for further analysis.
#'
#' @param datain Input dataset can be any `ADaM` data.
#' @param subset Subset conditions for analysis of dependent variable.
#' @param byvar Tilde(`~`) separated strings for grouping variable names. Assigns to
#' `BYVAR1, BYVAR2`, etc. Use `'/'` to specify ordering variables. eg: `SEX/SEXN~COUNTRY`.`
#' @param subgrpvar Tilde(`~`) separated strings for grouping variable names. Assigns to
#' `SUBGRPVAR1`, `SUBGRPVAR2`, etc. Use `'/'` to specify ordering variables.
#' eg: `RACE/RACEN~ETHNIC`.
#' @param trtvar Treatment variable name string assigned as `TRTVAR`.
#' @param trtsort Treatment sorting variable name string - numeric or
#' character variable assigned as `TRTSORT`.
#' @param trttotalyn Add total treatment values to be displayed as column in
#' table or category in plot (`"Y"/"N"`).
#' @param trttotlabel Label for total Treatment column/group
#' @param trtmissyn Retain Missing treatment counts in Total (if `trttotalyn` = Y). Missing
#' treatment will not be considered as a column in analysis in any case.
#' @param sgtotalyn Add total subgroup values to be displayed as column in
#' table or category in plot (`"Y"/"N"`).
#' @param add_grpmiss Add row or column for missing category in grouping
#' variables. (`"Y"/"N"`)
#' @param pop_fil Population Filter for data set: Name of flag variable.
#' eg: `"SAFFL"`, `"EFFFL"` or `NA` for Overall Population.
#'
#' @details
#'    1. **By Variable Processing**
#'        Identify `byvars` if byvar is given, create `BYVAR1`, `BYVAR2`, etc.
#'        Check for numeric code list variable for the grouping var, if not
#'        create numeric variable for sorting as `BYVAR1N`, `BYVAR2N` etc.
#'        Convert to `character` if `numeric`.
#'        Re-code blanks from grouping variables as `"Missing"`
#'        Remove missing category value from grouping variable per user choice.
#'    2. **Treatment Variable Processing**
#'        Identify Treatment Variable, treatment sorting variable.
#'        Remove non treatment values.
#'        If treatment sort variable doesn't exist, make it same as `TRTVAR`
#'        If treatment sort is not numeric, convert to numeric
#'        Add treatment total column based on `trttotalyn`
#'        Convert `TRTVAR` to a factor in same order as `TRTSORT`
#'    3. **Subgroup Variable processing**
#'        Identify Subgroup variable: create `SUBGRPVAR1N`, `SUBGRPVAR2N` etc if `subgrpvar` is
#'        specified.
#'        Check presence of numeric code list variables for the subgroup variables,
#'        if not create numeric variable for sorting purpose as `SUBGRPVAR1N` etc
#'        Re-code blanks from By and Subgroup vars as `Missing`
#'        Remove missing category value from grouping variable per user choice
#'        Add Subgroup total column for the last subgroup variable
#'    4. **Subset Processing**
#'        Applying population subset selected
#'        Applying denominator/overall subset condition passed by the user
#'        Applying analysis subset condition passed by the user
#'    5. **Calculate Big N for displaying in final output**
#'        `data.frame` with Count `N` for each treatment/subgroup if they exist.
#'        If no `Big N` required, then `NA` is returned.
#'
#' @return a `data.frame` output to be used in further utilities with applied `subset`
#' @export
#'
#' @examples
#' data(adsl)
#'
#' df_mentry <-
#'   adsl |> mentry(
#'     subset = "EFFFL=='Y'",
#'     byvar = "SEX/SEXN~AGEGR1",
#'     trtvar = "TRT01A",
#'     trtsort = "TRT01AN",
#'     subgrpvar = "RACE/RACEN~BMIBLGR1",
#'     trttotalyn = "N",
#'     add_grpmiss = "N",
#'     sgtotalyn = "N",
#'     pop_fil = "Overall Population"
#'   )
#'
#' df_mentry
mentry <- function(datain,
                   subset = NA,
                   byvar = NA,
                   subgrpvar = NA,
                   trtvar = NA,
                   trtsort = NA,
                   trttotalyn = "N",
                   trttotlabel = "Total",
                   trtmissyn = "N",
                   sgtotalyn = "N",
                   add_grpmiss = "N",
                   pop_fil = "Overall Population") {
  if (!is.data.frame(datain) || nrow(datain) == 0) {
    return(data.frame())
  }
  byvarlist <- sep_var_order(byvar)
  byvar <- byvarlist[["vars"]]
  sgvarlist <- sep_var_order(subgrpvar)
  subgrpvar <- sgvarlist[["vars"]]
  check_vars <- c(byvar, subgrpvar, trtvar)
  check_vars <- check_vars[!is.na(check_vars) & str_squish(check_vars) != ""]
  if (length(check_vars) > 0) {
    stopifnot(
      "One or more values of `byvar`, `subgrpvar` and/or `trtvar`
      not present in `datain`" =
        all(check_vars %in% names(datain))
    )
  }
  # store input data in `dsin`
  dsin <- datain
  # applying population subset selected
  if (!is.na(pop_fil) && pop_fil != "Overall Population") {
    dsin <- dsin |>
      filter(!!!parse_exprs(paste0(pop_fil, "== 'Y'")))
  }
  # applying analysis subset condition passed by the user
  if (!is.na(subset) && str_squish(subset) != "") {
    dsin <- dsin |>
      filter(!!!parse_exprs(subset))
  }
  # Conditionally create `BYVARx`, `BYVARxN`, SUBGRPVARx and so on...
  ## hide row/column for missing category in grouping
  if (add_grpmiss == "N") {
    dsin <- dsin |>
      filter(if_all(all_of(check_vars[!check_vars %in% trtvar]), \(x) !(x %in% c("", NA))))
  }
  ## BYVARs
  if (all(!is.na(byvar)) && all(str_squish(byvar) != "")) {
    dsin <- dsin |>
      create_grpvars(byvar, byvarlist[["order"]], "BYVAR")
  }
  ## SUBGRPVARs
  if (all(!is.na(subgrpvar)) && all(str_squish(subgrpvar) != "")) {
    dsin <- dsin |>
      create_grpvars(subgrpvar, sgvarlist[["order"]], "SUBGRPVAR", sgtotalyn)
  }
  ## TRTVARs
  if (!is.na(trtvar) && str_squish(trtvar) != "") {
    dsin <- dsin |>
      create_trtvar(trtvar, trtsort, trttotalyn, trttotlabel, trtmissyn)
  }
  # Remove invalid Treatment Values
  if ("TRTVAR" %in% names(dsin)) {
    dsin <- filter(dsin, !(
      toupper(.data[["TRTVAR"]]) %in% c(
        "NOT ASSIGNED",
        "SCREEN FAILURE",
        "SCRNFAIL",
        "NOTRT",
        "NOTASSGN",
        "",
        NA_character_
      )
    ))
  }
  dsin
}

#' Create Grouping Variables (`BYVAR1`, `BYVAR1N`, `SUBGRPVAR1`, `...`)
#'
#' @param dsin Input dataset.
#' @param vars Vector of grouping variables.
#' @param varN Numeric equivalent of `vars`.
#' @param new_var Prefix of the added grouping variables.
#' @param totalyn Display total subgroup column (`Y/N`).
#'
#' @return Data frame with new grouping variables.
#' @noRd
#'
create_grpvars <- function(dsin, vars, varN, new_var = "BYVAR", totalyn = "N", totlabel = "Total") {
  map(seq_along(vars), \(x) {
    grpvar <- paste0(toupper(new_var), x)
    grpvarN <- paste0(grpvar, "N")
    var <- vars[x]
    varN <- varN[x]
    
    df <- dsin |>
      mutate(!!grpvar := as.character(.data[[var]]))
    
    if (varN %in% names(df)) {
      df <- df |>
        mutate(!!grpvarN := .data[[varN]])
    } else {
      df <- df |>
        mutate(!!grpvarN := as.numeric(factor(.data[[var]])))
    }
    
    if (totalyn == "Y") {
      df <- df |>
        bind_rows(mutate(df, !!grpvar := totlabel, !!grpvarN := 9999))
    }
    df
  }) |>
    reduce(full_join, by = names(dsin)) |>
    mutate(across(where(is.numeric) & starts_with(new_var), ~ replace_na(.x, 998))) |>
    mutate(across(
      where(is.character) & starts_with(new_var),
      ~ case_when(str_squish(.x) %in% c(NA_character_, "") ~ "Missing", TRUE ~ .x)
    ))
}

#' Create TRTVAR
#'
#' @param dsin Input dataset.
#' @param trtvar Treatment Variable
#' @param trtsort Treatment Sorting variable
#' @param trttotalyn Display treatment total (`Y/N`)
#' @param trtmissyn Retain Missing treatment counts in Total (if `trttotalyn` = Y)
#'
#' @return Data frame with added `TRT` variables
#' @noRd
create_trtvar <- function(dsin, trtvar, trtsort, trttotalyn, trttotlabel = "Total", trtmissyn) {
  map <- c(trt = "TRTVAR", sort = "TRTSORT")
  
  df <- dsin |>
    mutate(!!unname(map["trt"]) := .data[[trtvar]])
  # keep missing treatments in total if trtmissyn is given (always removed in post)
  if (trtmissyn != "Y") {
    df <- df |>
      filter(!(.data[["TRTVAR"]] %in% c("", NA_character_)))
  }
  # Create trt sorting variable
  if (is.na(trtsort) || str_squish(trtsort) == "") {
    trtsort <- trtvar
  }
  
  if (trtsort %in% names(df) && is.numeric(df[[trtsort]])) {
    df <- df |>
      mutate(!!unname(map["sort"]) := .data[[trtsort]])
  } else {
    df <- df |>
      mutate(!!unname(map["sort"]) := as.numeric(factor(.data[[trtsort]])))
  }
  
  if (trttotalyn == "Y") {
    df <- df |>
      bind_rows(mutate(df, !!unname(map["trt"]) := trttotlabel, !!unname(map["sort"]) := 999))
  }
  
  df |>
    mutate(!!unname(map["trt"]) := factor(.data[[unname(map["trt"])]],
                                          levels = unique(.data[[unname(map["trt"])]][order(.data[[unname(map["sort"])]])]),
                                          ordered = TRUE
    ))
}

#' Separate variables and their grouping order
#'
#' @param vars Vector of grouping variables.
#' @param var_sep Variable separator.
#' @param ord_sep Order separator.
#'
#' @return `tibble` of variables with corresponding order variables
#' @noRd
#'
sep_var_order <- function(vars, var_sep = "~", ord_sep = "/") {
  var_list <-
    map(str_to_vec(vars, var_sep), \(vec) str_to_vec(vec, ord_sep)) |>
    map(\(split) {
      if (length(split) > 1) {
        sep_vars <- list(vars = split[1], order = split[2])
      } else {
        sep_vars <- list(vars = split[1], order = glue("{split[1]}N"))
      }
      sep_vars
    })
  
  bind_cols(map(set_names(unique(names(flatten(var_list)))), \(col) {
    map_chr(var_list, \(df) pluck(df, col))
  }))
}
