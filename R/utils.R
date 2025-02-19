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
#' Variable Labels and Corresponding names from dataframe
#'
#' @param datain Input dataframe to get variable labels for
#'
#' @return a dataframe with corresponding label for each variable
#'
#' @examples
#' data("adae")
#'
#' data_attrib(adae)
#' @noRd
data_attrib <- function(datain) {
  # converting all the column names to upcase
  names(datain) <- toupper(names(datain))
  # Get variable names and labels into dataframe
  data_attr <- setNames(
    data.frame(
      names(datain),
      unlist(lapply(
        names(datain),
        function(x) {
          ifelse(is.null(attr(datain[[x]], "label")),
            x, attr(datain[[x]], "label")
          )
        }
      ))
    ),
    c("VAR_NAMES", "VAR_LABEL")
  )
  data_attr
}

## Identify any variable names starting with given string. Useful for Byvar,subgrp identification ##
#' Find column names with starting pattern
#'
#' Searches column names of given `data.frame` to identify which start with the given pattern.
#' If the pattern given ends with `'N'`, it looks for variables that also end with `N`.
#' If the pattern does not end with `'N'`, it excludes variables that end with `'N'` while returning
#' those that start with the pattern. eg. `pattern = "BYVAR"` may return `c("BYVAR1","BYVAR2")`
#' Whereas `"BYVARN"` may return `c("BYVAR1N","BYVAR2N")`, if they exist.
#'
#' @param df `data.frame` input from which to get names
#' @param pattern String. If not ends with `N`, only matches start. If ends with `N`, matches start
#' and checks if column name ends in `"N"`
#'
#' @return Vector of column names starting with given pattern
#' @export
#'
#' @examples
#' data("ae_pre_process")
#'
#' ae_pre <- mentry(
#'   datain = ae_pre_process$data,
#'   subset = NA,
#'   byvar = "AEBODSYS~SEX",
#'   trtvar = "TRTA",
#'   trtsort = "TRTAN",
#'   subgrpvar = NA,
#'   trttotalyn = "N",
#'   add_grpmiss = "N",
#'   sgtotalyn = "N",
#'   pop_fil = "SAFFL"
#' )
#'
#' var_start(ae_pre, "BYVARN")
#' var_start(ae_pre, "BYVAR")
#'
var_start <- function(df, pattern) {
  if (endsWith(pattern, "N")) {
    sort(names(df)[startsWith(
      names(df),
      sub("N$", "", pattern)
    ) & (endsWith(names(df), "N"))])
  } else {
    sort(names(df)[startsWith(
      names(df),
      pattern
    ) & (!endsWith(names(df), "N"))])
  }
}

#' Round and Format values
#'
#' Rounds value and formats it to retain number of decimals in 2nd argument
#'
#' @param x Numeric value to be rounded
#' @param digits Number of decimal places
#'
#' @return Rounded and formatted character value
#' @export
#'
#' @examples
#' round_f(12.31, 3)
round_f <- function(x, digits = 2) {
  format(round(x, digits), nsmall = digits)
}


#' Convert string to vector
#'
#' @param var The string object
#' @param sep Separator between strings. Default: "~"
#'
#' @return Character vector
#'
#' @export
#'
str_to_vec <- function(var, sep = "~") {
  str_squish(unlist(str_split(var, sep)))
}

#' Split data by given variables into list
#'
#' @param datain Input data
#' @param split_by Variables to split data by (used if `split_by_prefix` is "")
#' @param split_by_prefix Prefix to identify splitting variables
#'
#' @return list of dataframe(s)
#' @export
#'
#' @examples
#' split_data_by_var(iris, "Species")
split_data_by_var <- function(datain,
                              split_by = "",
                              split_by_prefix = "") {
  stopifnot(is.data.frame(datain))
  if (all(split_by == "") && split_by_prefix == "") {
    return(list(datain))
  }
  if (split_by_prefix != "") {
    split_by <- var_start(datain, split_by_prefix)
    stopifnot("No variables with split_by_prefix" = length(split_by) != 0)
  }
  stopifnot("split_by var should exist in data" = all(split_by %in% names(datain)))
  data_list <- datain |>
    group_by(!!!syms(split_by)) |>
    group_split()
  return(data_list)
}

#' Return section headers according to splitting variables specified
#'
#' @param datain Input data
#' @param split_by Variables to split data by (used if `split_by_prefix` is "")
#' @param split_by_prefix Prefix to identify splitting variables
#' @param split_lab Text to prefix the section values with
#' @param sep Separating character if multiple split_by are present
#'
#' @return list of dataframe(s)
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' data(adsl)
#'
#' adsl |>
#'   mutate(SUBGRPVAR1 = SEX, SUBGRPVAR2 = AGEGR1) |>
#'   split_section_headers(
#'     split_by = "",
#'     split_by_prefix = "SUBGRPVAR",
#'     split_lab = "Group:~Age Group",
#'     sep = " & "
#'   )
#'
#' iris |>
#'   split_section_headers("Species", split_lab = "Name of species:")
split_section_headers <- function(datain,
                                  split_by = "",
                                  split_by_prefix = "",
                                  split_lab = " ",
                                  sep = "~") {
  stopifnot(is.data.frame(datain))
  if (split_by == "" && split_by_prefix == "") {
    return("")
  } else if (split_by != "") {
    split_by <- str_to_vec(split_by)
    stopifnot("split_by var should exist in data" = all(split_by %in% names(datain)))
  } else {
    split_by <- var_start(datain, split_by_prefix)
    stopifnot("No variables with split_by_prefix" = length(split_by) > 0)
  }

  if (split_lab != "") {
    split_lab <- str_to_vec(split_lab)
  } else {
    split_lab <- NA_character_
  }

  header_list <- datain |>
    group_by(!!!syms(split_by)) |>
    group_keys()

  map(seq_along(split_by), \(x) {
    header_list |>
      mutate(
        !!glue::glue("head{x}") := glue::glue("{replace_na(split_lab[x], split_by[x])}{.data[[split_by[x]]]}") # nolint
      )
  }) |>
    reduce(full_join, by = split_by) |>
    unite("head", starts_with("head"), sep = sep) |>
    pull("head")
}

#' Order Summary Table
#'
#' @param df Data frame
#' @inheritParams risk_stat
#'
#' @return Ordered data frame
#' @noRd
ord_summ_df <- function(df, sort_var, sort_opt, g_sort_by_ht = "N") {
  if (g_sort_by_ht == "Y") {
    df <- df |> group_by(across(any_of("BYVAR1")))
  }
  if (sort_opt != "Descending") {
    df <- arrange(df, by = !!!syms(sort_var), .by_group = TRUE)
  } else {
    df <- arrange(df, by = desc(!!!syms(sort_var)), .by_group = TRUE)
  }
  return(ungroup(df))
}

#' Get Variables for ordering
#'
#' @param sort_var Sorting variable
#'
#' @return Vector of length `1`.
#' @noRd
get_sort_var <- function(sort_var) {
  recode(
    sort_var,
    "Count" = "CTRL_N",
    "Percent" = "CTRL_PCT",
    "RiskValue" = "RISK",
    "Alphabetical" = "DPTVAL"
  )
}

#' Output a Dataset in a Vignette
#'
#' @param df Dataset to output in the vignette
#' @param disp_vars Variables selected to demonstrate the outcome of the derivation
#' @param subset Subset condition
#'
#' @return A HTML table
#' @export
#'
dataset_vignette <- function(df = NULL, disp_vars = NULL, subset = NA_character_) {
  out <- df |>
    mutate(across(where(is.character), as.factor))
  if (!is.na(subset)) {
    out <- out |>
      filter(!!!parse_exprs(subset))
  }

  if (!is.null(disp_vars)) {
    hide_columns <- which(!(colnames(out) %in% str_to_vec(disp_vars)))
    cols_to_hide <- list(list(targets = hide_columns - 1, visible = FALSE))
  } else {
    cols_to_hide <- list()
  }

  DT::datatable(
    out,
    rownames = FALSE,
    filter = "top",
    height = "auto",
    width = "auto",
    extensions = c("Buttons", "ColReorder", "Scroller"),
    options = list(
      columnDefs = cols_to_hide,
      searchHighlight = TRUE,
      searching = TRUE,
      pageLength = 5,
      scrollX = TRUE,
      lengthMenu = c(5, 10, 15, 20, 50, 100),
      dom = "<Bfr<\"dt-scroll\"t>ipl>",
      buttons = list(list(
        extend = "colvis",
        text = "Choose the columns to display",
        scroller = TRUE,
        collectionLayout = "fixed two-column"
      )),
      colReorder = TRUE
    )
  )
}

#' Create `Big N` data frame
#'
#' @param data Analysis Dataset (for output)
#' @param dsin Input dataset (usually `mentry()` output)
#' @param grpvar Grpuping variables based on which `Big N` will be calculated.
#' @param modvar Variable name to be modified for new bign column. eg. if "TRTVAR", "TRTVAR_BIGN"
#' will be created.
#' @param subjid Subject ID variable. Values: "USUBJID"/"SUBJID"
#'
#' @return Data frame with `Big N`.
#' @noRd
add_bigN <- function(data, dsin, grpvar, modvar, subjid = "USUBJID") {
  newvar <- paste0(modvar, "_BIGN")
  data <- dsin |>
    group_by(!!!syms(grpvar)) |>
    distinct(!!!syms(subjid)) |>
    summarise(BIGN = n()) |>
    (\(.) left_join(data, ., by = grpvar))() |>
    mutate(!!newvar := paste0(.data[[modvar]], " (N=", .data[["BIGN"]], ")")) |>
    select(-all_of("BIGN"))
  if (length(modvar) == 1 && is.factor(data[[modvar]])) {
    newvar <- paste0(modvar, "_BIGN")
    data[[newvar]] <- factor(data[[newvar]],
      levels = unique(data[[newvar]][order(data[[modvar]])])
    )
  }
  data
}

#' Attach bign value to table columns, modify column header labels
#'
#' @param datain Input dataframe for display
#' @param mentry_data Input data from `mentry()`
#' @param trtbignyn Append (N = trt count) to Treatment Column headers? Values: Y/N
#' @param subbignyn Append (N = trt*subgrp count) to Subgroup Column headers? Values: Y/N
#' @param colformat String (usually indicating format) to add to column (treatment/total) names.
#' @param notrthead If no Treatment or subgroup column exists, label to give to "CVALUE" column
#'
#' @return a dataframe with/without selected columns (trt/subgrp) appended with (N = ) values,
#' or modified column header
#' @export
#'
#' @examples
#' adsl_entry <- mentry(
#'   datain = adsl,
#'   trtvar = "TRT01A",
#'   trtsort = "TRT01AN",
#'   subgrpvar = "SEX"
#' )
#'
#' msumstat(
#'   adsl_entry,
#'   dptvar = "AGE",
#'   statvar = "mean",
#'   dptvarn = 2
#' )$tsum |>
#'   display_bign_head(adsl_entry)
display_bign_head <- function(datain,
                              mentry_data = NULL,
                              trtbignyn = "Y",
                              subbignyn = "Y",
                              colformat = "",
                              notrthead = "Total") {
  stopifnot(nrow(datain) != 0)
  grpvar <- c(var_start(datain, "TRTVAR"), var_start(datain, "SUBGRP"))
  stopifnot(nrow(mentry_data) != 0)
  if (length(grpvar) > 0) {
    grp <- intersect(grpvar, names(mentry_data))
    # If bign is required
    if (length(grp) > 0 && any(c(trtbignyn, subbignyn) == "Y")) {
      # bign for both grouping variables, treatment and subgroup
      opvar <- grp
      if (trtbignyn != "Y") {
        opvar <- grp[grp != "TRTVAR"]
      }
      if (subbignyn != "Y") {
        opvar <- opvar[!startsWith(opvar, "SUBGRP")]
      }
      for (g in seq_along(grp)) {
        if (grp[g] %in% opvar) {
          datain <- add_bigN(datain, mentry_data, grp[1:g], grp[g])
        }
      }
      datain <- datain |>
        select(-any_of(opvar)) |>
        (\(.) setNames(., sub("\\_BIGN", "", names(.))))()
    }
    if (colformat != "") {
      lastvar <- grpvar[length(grpvar)]
      ord <- order(datain[["TRTVAR"]])
      datain <- datain |>
        mutate(across(any_of(lastvar), ~ paste0(.x, colformat)))
      if (lastvar == "TRTVAR") {
        datain[["TRTVAR"]] <- factor(datain[["TRTVAR"]],
          levels = unique(datain[["TRTVAR"]][ord])
        )
      }
    }
  } else {
    notrthead <- ifelse(any(c(trtbignyn, subbignyn) == "Y"),
      paste0(notrthead, " (N = ", length(unique(mentry_data[["USUBJID"]])), ")"), notrthead
    )
    datain <- datain |>
      mutate(!!notrthead := as.character(.data[["CVALUE"]])) |>
      select(-all_of("CVALUE"))
  }
  return(datain)
}

#' Sparse empty categories/treatments with 0
#'
#' @param datain Input data to be sparsed for missing categories/treatments/by vars
#' @param data_sparse Initial data to sparse with
#' @param sparseyn Sparse categories within by groups. (Y/N)
#' @param sparsebyvalyn Sparse by groups in data - takes precedence over `sparseyn` (Y/N)
#' @param BYVAR By Variables in data
#' @param BYVARN By Variables N equivalent
#' @param SUBGRP Subgroup Variables in data
#' @param SUBGRPN Subgroup Variables N equivalent
#' @param fillvar Variables to fill with `fill_with`
#' @param fill_with Value to fill empty `fillvar` with
#'
#' @return dataframe sparsed with values for empty categories
#'
#' @examples
#' data(adsl)
#' library(dplyr)
#' adsl_entry <- mentry(adsl,
#'   byvar = "SEX",
#'   trtvar = "TRT01A",
#'   trtsort = "TRT01AN",
#'   subset = "SAFFL == 'Y'"
#' )
#' count <- adsl_entry |>
#'   filter(SEX == "F") |>
#'   group_by(BYVAR1, TRTVAR) |>
#'   summarise(FREQ = length(unique(USUBJID)))
#' sparse_vals(count,
#'   data_sparse = adsl_entry,
#'   sparseyn = "N",
#'   sparsebyvalyn = "Y",
#'   "BYVAR1",
#'   character(0),
#'   "BYVAR1N",
#'   character(0)
#' )
#'
#' @noRd
sparse_vals <- function(datain,
                        data_sparse,
                        sparseyn = "Y",
                        sparsebyvalyn = "N",
                        BYVAR,
                        SUBGRP,
                        BYVARN,
                        SUBGRPN,
                        fillvar = "FREQ",
                        fill_with = 0) {
  # Exit if neither are Y
  if (!(sparseyn == "Y" || sparsebyvalyn == "Y")) {
    return(datain)
  }
  TRTVAR <- var_start(datain, "TRTVAR")
  if (sparsebyvalyn == "Y") {
    byn <- c(BYVAR, SUBGRP)
    if ("DPTVAL" %in% names(datain)) {
      df_exp <- data_sparse |>
        tidyr::expand(!!!rlang::syms(c(BYVAR, TRTVAR, SUBGRP)), tidyr::nesting(DPTVAL, DPTVALN))
      dptn <- "DPTVALN"
    } else {
      if (!any(c(SUBGRP, "TRTVAR") %in% names(datain))) {
        return(datain)
      }
      # Processing if msumstat output/without DPTVAL column
      df_exp <- data_sparse |>
        tidyr::expand(!!!rlang::syms(c(BYVAR, TRTVAR, SUBGRP)))
      dptn <- character()
    }
  } else if (sparseyn == "Y") {
    # Sparse only category columns
    byn <- SUBGRP
    dptn <- "DPTVALN"
    df_exp <- data_sparse |>
      tidyr::expand(!!!rlang::syms(c(TRTVAR, SUBGRP)), tidyr::nesting(DPTVAL, DPTVALN)) |>
      left_join(distinct(data_sparse, across(any_of(starts_with(c("DPTVAL", "BYVAR"))))),
        by = c("DPTVAL", "DPTVALN")
      )
  }
  data_sparse <- ungroup(data_sparse)
  if (length(byn) > 0) {
    for (b in byn) {
      df_exp <- df_exp |>
        left_join(distinct(data_sparse, across(all_of(starts_with(b)))), by = b)
    }
  }
  df_exp <- distinct(df_exp)
  datain |>
    select(-any_of(c(SUBGRPN, BYVARN, dptn))) |>
    (\(.) full_join(., df_exp, by = intersect(names(.), names(df_exp))))() |>
    mutate(across(any_of(fillvar), ~ replace_na(.x, fill_with))) |>
    ungroup() |>
    distinct()
}

#' Report Metadata
#'
#' @return `data.frame` containing report metadata
#' @noRd
#'
get_report_meta <- function() {
  tibble::tribble(
    ~TA,
    ~DOMAIN,
    ~REPNAME,
    ~REPDESC,
    ~REPNO,
    ~REPTYPE,
    "Any",
    "ADAE",
    "adae_tier_summary",
    "Summary of Adverse Events by System Organ Class and Preferred Term",
    "2.1",
    "Table",
    "Any",
    "ADAE",
    "adae_risk_summary",
    "Relative Risk of Adverse Events by System Organ Class and Preferred Term",
    "2.2",
    "Table",
    "Any",
    "ADAE",
    "ae_forest_plot",
    "Forest plot for Adverse Events",
    "2.3",
    "Figure",
    "Any",
    "ADAE",
    "ae_volcano_plot",
    "Volcano plot for Adverse Events",
    "2.4",
    "Figure",
    "Any",
    "ADAE",
    "Event Analysis",
    "Event analysis of MedRA query",
    "2.5",
    "Figure",
    "Any",
    "ADAE",
    "tornado_plot",
    "Tornado plot for Adverse Events",
    "2.6",
    "Figure",
    "Any",
    "ADLB",
    "eDISH_plot",
    "eDISH Plot to Assess Liver Safety Data",
    "2.7",
    "Figure",
    "Any",
    "ADSL",
    "adsl_summary",
    "Demographic Summary",
    "3.1",
    "Table",
    "Oncology",
    "ADTTE",
    "KM Plot",
    "Kaplan Meier Plot for Time to Events",
    "4.1",
    "Figure",
  )
}

get_ae_term <- function() {
  aeTerm <- c(
    "Reported Term for the Adverse Event (AETERM)" = "AETERM",
    "AE Lowest Level Term (AELLT)" = "AELLT",
    "AE Dictionary-Derived Term (AEDECOD)" = "AEDECOD",
    "AE High Level Term (AEHLT)" = "AEHLT",
    "AE High Level Group Term (AEHLGT)" = "AEHLGT",
    "Primary System Organ Class (AESOC)" = "AESOC",
    "Body System or Organ Class (AEBODSYS)" = "AEBODSYS",
    "FMQ Name (FMQ_NAM)" = "FMQ_NAM"
  )
}
