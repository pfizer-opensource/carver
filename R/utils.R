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
  return(data_attr)
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

#' Update functions to round values
#'
#' @param f List of names of summary statistics.
#' @param d Numeric values
#' @param ...
#'
#' @return Rounded numeric values
#' @noRd
#'
fmtrd <- function(f, d = 2, ...) {
  function(x) {
    dc <- do.call(f, args = list(x, na.rm = TRUE, ...))
    return(ifelse(is.na(dc), "-", round_f(dc, d)))
  }
}

#' Create summary stats function for use within `msumstat()`
#'
#' @param sigdec Number of significant decimal places (base)
#'
#' @return a named list containing function definition for all defined summary
#' statistics - mean, min, max, median, iqr, var, sum, sd, q25, q75, p1, p5,
#' p10, p90, p95, p99 (where last digits represent % of quantile), meansd,
#' range, q1q3, medianrange (concatenation of indicated names), whiskerlow,
#' whiskerup, outliers in the Tukey method for box statistics
#' @export
summary_functions <- function(sigdec = 2) {
  # Basic functions for summary
  base_fns <- c("mean", "min", "max", "median", "IQR", "var", "sum", "sd")
  d <- c(rep(sigdec, 7), sigdec + 1)
  fns_list <- map(seq_along(base_fns), function(f) {
    fmtrd(f = base_fns[[f]], d = d[[f]])
  }) |> set_names(tolower(base_fns))
  # Quantiles for data:
  q_fns <- c("q25", "q75", "p1", "p10", "p5", "p90", "p95", "p99")
  q_pct <- as.numeric(gsub("\\D", "", q_fns)) / 100
  q_list <- seq_along(q_fns) |>
    map(function(f) fmtrd(f = "quantile", d = sigdec, q_pct[[f]])) |>
    set_names(q_fns)
  # Combined list
  fns_list <- append(fns_list, q_list)
  # Concatenated and Compound functions
  fns_list <- append(
    fns_list,
    list(
      meansd =
        function(x) paste0(fns_list[["mean"]](x), " (", fns_list[["sd"]](x), ")"),
      range = function(x) {
        paste0(
          "(", fns_list[["min"]](x), ", ",
          fns_list[["max"]](x), ")"
        )
      },
      q1q3 = function(x) {
        paste0(
          "(", fns_list[["q25"]](x), ", ",
          fns_list[["q75"]](x), ")"
        )
      },
      medianrange =
        function(x) paste(fns_list[["median"]](x), fns_list[["range"]](x)),
      whiskerlow = function(x) {
        fns_list[["min"]](
          x[(x >= (quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE))) &
              (x <= quantile(x, 0.25, na.rm = TRUE))])
      },
      whiskerup = function(x) {
        fns_list[["max"]](
          x[(x <= (quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE))) &
              (x >= quantile(x, 0.75, na.rm = TRUE))])
      },
      outliers = function(x) {
        x <- x[!is.na(x)]
        paste(unique(x[x < as.numeric(fns_list[["whiskerlow"]](x)) |
                         x > as.numeric(fns_list[["whiskerup"]](x))]), collapse = "~")
      },
      geom_lowci = function(x) {
        ci <- 0.95
        x <- log(x)
        margin_error <- qt(ci + (1 - ci) / 2, df = length(x) - 1) * sd(x) / sqrt(length(x))
        paste(exp(mean(x, na.rm = TRUE) - margin_error))
      },
      geom_upci = function(x) {
        ci <- 0.95
        x <- log(x)
        margin_error <- qt(ci + (1 - ci) / 2, df = length(x) - 1) * sd(x) / sqrt(length(x))
        paste(exp(mean(x, na.rm = TRUE) + margin_error))
      },
      geommean = function(x) {
        x <- log(x)
        paste(exp(mean(x, na.rm = TRUE)))
      },
      n = function(x) paste(n())
    )
  )
  return(fns_list)
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
                                  split_lab = "",
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
  data <- dsin |>
    group_by(!!!syms(grpvar)) |>
    distinct(!!!syms(subjid)) |>
    summarise(BIGN = n()) |>
    (\(.) left_join(data, ., by = grpvar))() |>
    mutate(across(any_of(modvar),
                  ~ paste0(.x, " (N=", .data[["BIGN"]], ")"),
                  .names = "{.col}_BIGN"
    )) |>
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
#'   statvar = "meansd",
#'   sigdec = 2,
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
    "adae_r001",
    "Summary of Adverse Events by System Organ Class and Preferred Term",
    "2.1",
    "Table",
    "Any",
    "ADAE",
    "Forest Plot",
    "Forest plot for adverse events",
    "2.2",
    "Figure",
    "Any",
    "ADAE",
    "Volcano Plot",
    "Volcano plot for adverse events",
    "2.4",
    "Figure",
    "Any",
    "ADAE",
    "Event Analysis",
    "Event analysis of MedRA query",
    "2.5",
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
