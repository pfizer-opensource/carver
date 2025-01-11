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
#' Generic Occurrence Summary Tiered Table
#'
#' @inheritParams risk_stat
#' @param datain Input dataset (generally the output from `mentry()`)
#' @param hterm High Level Event term variable, used for analysis (tilde-separated)
#' @param lterm Low Level Event term variable, used for analysis
#' @param htermctyn To show count of high term rows or not. Should correspond to and be same number
#' of terms passed in `hterm` (tilde-separated). To suppress showing counts for any term pass "N"
#' @param pctdisp Method to calculate denominator (for %) by.
#' Possible values: `"TRT"`, `"VAR"`, `"COL"`, `"SUBGRP"`, `"CAT"`, `"NONE"`, `"NO"`, `"DPTVAR"`,
#' `"BYVARxyN"`
#' @param sum_row To show summary/any term row or not. 'Y'/'N'
#' @param sum_row_label Label for Summary Row to be displayed, if Y.
#' @param apply_hrow_cutoff To apply `cutoff_where` value to high terms in addition to low term.
#' If set to "Y" same cutoff is applied to remove both  high and low level terms that don't meet
#' the criteria.
#' If set to "N" (default), cutoff is applied only to Lower Level term. The terms that do not fit
#' the criteria are then excluded from the counts for High Level term. This does not happen in case
#' of "N" - all low terms are included in high term which is displayed as long as it meets the
#' criteria as well.
#' @param sort_col Which treatment column to sort by. (Depends on trt levels) eg: 1, 2, 3
#' @param nolwrtierdispyn When`apply_hrow_cutoff` = Y, to display high level terms with zero low
#' level terms satisfying the cutoff threshold or not? If Y, high terms will be displayed even with
#' no corresponding lower levels in the table.
#' @param sigdec_cat Number of decimal places for % displayed in output
#' @param pctsyn Display Percentage Sign in table or not. Values: `"Y"/"N"`
#' @param stathead Label for sub-column header in output. eg. "n (%)"
#'
#' @details
#' \itemize{
#' \item `cutoff_where` is applied to event lower term only, unless `apply_hrow_cutoff` is given.
#' \item If `apply_hrow_cutoff` is Y, cutoff_where is applied to higher terms as well. If it is N,
#' lower terms which do not meet criteria are removed from higher term count. eg: if `cutoff_where`
#' is set to "PCT >= 2" and `hterm` and `lterm` are AEBODSYS and AEDECOD:
#'
#' EYE DISORDERS         9 (3.1)
#'  Dry eye        3 (1.4)
#'  Wet eye        6 (2.4)
#'
#' Here if `apply_hrow_cutoff` is set to N then 'Dry eye' row will be excluded and the 3 excluded
#' from count of EYE DISORDERS as well (9). If Y, then 'Dry eye' will be excluded but EYE DISORDERS
#' not impacted as it is 4.4% and its PCT >= 2.
#'
#' \item If `cutoff_where` is PCT >= 3 and `nolwrtierdispyn` set to Y, then
#' neither Dry eye nor Wet eye will be shown, but EYE DISORDERS will still be displayed.
#'
#' If `nolwrtierdispyn` is N in this case, EYE DISORDERS will also be removed as no low terms meet
#' the criteria.
#' }
#'
#' @return Summarized data frame for Adverse Events based on high and lower terms.
#' @export
#'
#' @examples
#' ae_entry <- ae_pre_process[["data"]] |>
#'   mentry(
#'     subset = NA,
#'     byvar = "AEBODSYS",
#'     trtvar = "TRTA",
#'     trtsort = "TRTAN",
#'     trttotalyn = "N",
#'     add_grpmiss = "N",
#'     sgtotalyn = "N",
#'     pop_fil = "Overall Population"
#'   )
#' output <- occ_tier_summary(
#'   ae_entry,
#'   a_subset = ae_pre_process[["a_subset"]],
#'   summary_by = "Patients",
#'   hterm = "AEBODSYS",
#'   lterm = "AEDECOD",
#'   pctdisp = "TRT",
#'   cutoff_where = "PCT > 2",
#'   apply_hrow_cutoff = "N",
#'   sort_opt = "Ascending",
#'   sort_var = "Count"
#' )
#' output |>
#'   tbl_processor() |>
#'   tbl_display()
#' # Example 2: ADAE table with max sev/ctc grade:
#' ae_pre <- ae_pre_processor(
#'   adae,
#'   subset = "TRTEMFL == 'Y'",
#'   max_sevctc = "SEV",
#'   sev_ctcvar = "AESEVN",
#'   pt_total = "Y"
#' )
#' ae_entry_max <- adsl_merge(
#'   adsl,
#'   adsl_subset = 'SAFFL == "Y"',
#'   ae_pre[["data"]]
#' ) |>
#'   mentry(
#'     subset = NA,
#'     byvar = "AEBODSYS",
#'     trtvar = "TRTA",
#'     trtsort = "TRTAN",
#'     trttotalyn = "N",
#'     add_grpmiss = "N",
#'     subgrpvar = "AESEV",
#'     sgtotalyn = "N",
#'     pop_fil = "Overall Population"
#'   )
#' rpt_data <- occ_tier_summary(
#'   ae_entry_max,
#'   a_subset = ae_pre[["a_subset"]],
#'   summary_by = "Patients",
#'   hterm = "AEBODSYS",
#'   lterm = "AEDECOD",
#'   cutoff_where = "FREQ > 2",
#'   pctdisp = "TRT",
#'   sum_row = "Y",
#'   sum_row_label = "Any Adverse Event",
#'   nolwrtierdispyn = "N",
#'   sort_opt = "Alphabetical",
#'   stathead = "n (%)"
#' )
#' rpt_data |>
#'   tbl_processor() |>
#'   tbl_display(dpthead = "No. of Adverse Events_SOC and PT") |>
#'   flextable::autofit()
#' ## ADPR Example:
#' \dontrun{
#' pr_entry <- adsl |>
#'   adsl_merge(
#'     adsl_subset = "SAFFL == 'Y'",
#'     dataset_add = adpr
#'   ) |>
#'   mentry(
#'     subset = NA,
#'     byvar = "PRSOC",
#'     trtvar = "TRT01A",
#'     trtsort = "TRT01AN",
#'     trttotalyn = "N",
#'     add_grpmiss = "N",
#'     sgtotalyn = "N",
#'     pop_fil = "Overall Population"
#'   )
#' output <- occ_tier_summary(
#'   pr_entry,
#'   a_subset = "ONPERFL == 'Y' & PRDECOD != ''",
#'   summary_by = "Patients",
#'   hterm = "PRSOC",
#'   lterm = "PRDECOD",
#'   pctdisp = "TRT",
#'   apply_hrow_cutoff = "N",
#'   sort_opt = "Ascending",
#'   sort_var = "Count",
#'   sum_row = "Y",
#'   sum_row_label = "Participants with 1 term",
#'   htermctyn = "N"
#' )
#' output |>
#'   display_bign_head(
#'     mentry_data = pr_entry
#'   ) |>
#'   tbl_processor() |>
#'   tbl_display()
#' }
#'
occ_tier_summary <- function(datain,
                             a_subset = NA_character_,
                             summary_by = "Patients",
                             hterm = "AEBODSYS",
                             lterm = "AEDECOD",
                             htermctyn = "Y",
                             pctdisp = "TRT",
                             cutoff_where = NA,
                             sum_row = "N",
                             sum_row_label = "Number of Participants with Any AE",
                             apply_hrow_cutoff = "N",
                             sort_opt = "Ascending",
                             sort_var = "Count",
                             sort_col = 1,
                             nolwrtierdispyn = "N",
                             sigdec_cat = 2,
                             pctsyn = "Y",
                             stathead = "n (%)") {
  if (nrow(datain) == 0) {
    return(datain)
  }
  stopifnot(
    "`byvar` in `mentry()` cannot be `NA` or ''" =
      identical(var_start(datain, "BYVAR"), "BYVAR1")
  )
  byvars <- var_start(datain, "BYVAR")
  rows <- c(byvars, "DPTVAL")
  # Lowest Term Calculation
  lcat <- mcatstat(
    datain = datain,
    a_subset = a_subset,
    uniqid = ifelse(summary_by == "Events", "ALLCT", "USUBJID"),
    dptvar = lterm,
    pctdisp = pctdisp,
    sigdec = sigdec_cat,
    pctsyn = pctsyn,
    sparseyn = "N"
  )
  if (!is.data.frame(lcat) || nrow(lcat) == 0) {
    return(data.frame())
  }
  # Applying Cutoff to lower level term counts/pct
  if (!is.na(cutoff_where) && str_detect(cutoff_where, "PCT|FREQ")) {
    lcat_cut <- lcat |>
      filter(!!!parse_exprs(cutoff_where)) |>
      select(all_of(rows)) |>
      mutate(CUTFL = "Y")
    lcat <- lcat |>
      semi_join(lcat_cut, by = rows)
    if (nrow(lcat) < 1) {
      return(data.frame())
    }
    # Rows under cutoff to be excluded from higher level term counts
    if (apply_hrow_cutoff != "Y") {
      datain <- datain |>
        left_join(rename(lcat_cut, !!lterm := DPTVAL), by = gsub("DPTVAL", lterm, rows))
      a_subset <- paste(na.omit(c(a_subset, "CUTFL == 'Y'")), collapse = "&")
      if (nolwrtierdispyn == "N") {
        datain <- semi_join(datain, lcat_cut, by = byvars[length(byvars)])
      }
    } else {
      if (nolwrtierdispyn == "N") {
        datain <- datain |>
          left_join(lcat_cut |> select(-all_of("DPTVAL")), by = byvars[length(byvars)])
        a_subset <- paste(na.omit(c(a_subset, "CUTFL == 'Y'")), collapse = "&")
      }
    }
  }
  hterm <- str_to_vec(hterm)
  # Higher Terms calculation:
  hcat <- map(hterm, \(h_term) {
    h <- mcatstat(
      datain = datain,
      a_subset = ifelse("HT_FL" %in% names(datain), "HT_FL == 1", a_subset),
      uniqid = ifelse(summary_by == "Events", "ALLCT", "USUBJID"),
      dptvar = h_term,
      pctdisp = pctdisp,
      sigdec = sigdec_cat,
      pctsyn = pctsyn,
      sparseyn = "N"
    )
    # Apply CUTOFF based on parameter and lower term value
    if (!is.na(cutoff_where) && str_detect(cutoff_where, "PCT|FREQ") && apply_hrow_cutoff == "Y") {
      h <- h |>
        semi_join(h |> filter(!!!parse_exprs(cutoff_where)), by = rows)
      if (nrow(h) < 1) {
        return(data.frame())
      }
    }
    h
  })

  # Summary/Any Row Output
  if (sum_row != "N") {
    sum_data <- summary_row_cat(
      datain,
      sum_row_label,
      byvaryn = "N",
      a_subset,
      pctdisp,
      sigdec_cat,
      pctsyn,
      "ANY"
    )
  } else {
    sum_data <- NULL
  }

  if ("PT_CNT" %in% names(datain)) {
    pt_data <- datain |>
      summary_row_cat(
        a_subset = a_subset,
        var = "PT_CNT",
        pctdisp = "NONE",
        sum_row_label = "Total preferred term events",
        uniqid = c("AEDECOD", "USUBJID")
      )
  } else {
    pt_data <- NULL
  }
  ### Combined Processing for lower and higher terms
  # Sort Values and Options
  if (sort_opt == "Alphabetical") {
    sort_var <- get_sort_var(sort_opt)
  } else {
    sort_var <- get_sort_var(sort_var)
  }
  ctrlgrp <- get_ctrlgrp(datain, sort_col)
  comb <- append(hcat, list(lcat))
  # Mapping over each categorical dataset and applying post-processing function to concatenate
  # the dataframes into single output
  outdata <- map(seq_along(comb), \(i) {
    xout <- comb[[i]] |>
      mutate(
        CTRL_N = ifelse(.data[["TRTVAR"]] == ctrlgrp, .data[["FREQ"]], NA),
        CTRL_PCT = ifelse(.data[["TRTVAR"]] == ctrlgrp, .data[["PCT"]], NA)
      )
    # Sort lowest terms
    if (i == length(comb) && sort_opt == "Alphabetical") {
      sort_var <- c(var_start(xout, "BYVAR"), sort_var)
    }
    ## Order Summary tables
    xout |>
      ord_summ_df(sort_var, sort_opt)
  }) |>
    post_occ_tier(ctrlgrp = ctrlgrp, sum_row = sum_data, pt_row = pt_data, stathead = stathead)

  # To suppress any high term percentage counts, per variable htermctyn
  if (any(str_to_vec(htermctyn) == "N")) {
    htermctyn <- str_to_vec(htermctyn)
    stopifnot(length(htermctyn) == length(hterm))
    blankterm <- hterm[which(htermctyn == "N")]
    outdata <- outdata |>
      mutate(CVALUE = ifelse(
        toupper(.data[["DPTVAR"]]) %in% toupper(blankterm),
        "",
        .data[["CVALUE"]]
      ))
  }
  outdata |>
    mutate(DPTVAR = "TIER")
}

#' Prepare Occurrence summary for Tabular Display
#'
#' @param occ_summ Grouped Occurrence Summary
#' @inheritParams adae_risk_summary
#'
#' @return Flextable object
#' @noRd
post_occ_tier <-
  function(occ_summ, riskyn = "N", ctrlgrp, sum_row = NULL, risklabels = tbl_risk_labels(),
           pt_row = NULL, stathead = "n (%)") {
    occ_summ <- occ_summ |>
      setNames(c("hterm_summ", "lterm_summ"))
    final_cts <- occ_summ |>
      bind_rows() |>
      select(-any_of(c("DPTVARN", "DPTVALN"))) |>
      inner_join(ord_by_ht(occ_summ, ctrlgrp), by = c("DPTVAR", "BYVAR1", "DPTVAL")) |>
      select(-any_of(c("BYVAR1", "BYVAR1N", "CUTFL"))) |>
      select(-starts_with("CTRL_"))
    if (is.data.frame(sum_row) && nrow(sum_row) > 0) {
      final_cts <- bind_rows(final_cts, sum_row |> mutate(DPTVARN = 0, DPTVALN = 0))
    }
    if (is.data.frame(pt_row) && nrow(pt_row) > 0) {
      final_cts <- bind_rows(
        final_cts, pt_row |> mutate(DPTVALN = 0, DPTVARN = max(final_cts$DPTVARN, na.rm = TRUE) + 1)
      )
    }
    SUBGRPN <- var_start(final_cts, "SUBGRPN")
    if (length(SUBGRPN) > 0) {
      repvar <- SUBGRPN[length(SUBGRPN)]
    } else {
      repvar <- "TRTVAR"
    }
    final_cts <- final_cts |>
      mutate(
        DPTVAL = ifelse(.data[["DPTVALN"]] == 0,
          .data[["DPTVAL"]], paste0("\t\t\t", .data[["DPTVAL"]])
        ),
        SUBGRPVARX = paste0(stathead, strrep(" ", as.numeric(as.factor(.data[[repvar]])))),
        SUBGRPVARXN = 1
      )
    if (riskyn == "Y") {
      # Rename variable containing RISK_CI based user inputs
      final_cts <- final_cts |>
        filter(!is.nan(.data[["RISK"]]), !is.infinite(.data[["RISK"]])) |>
        mutate(
          !!risklabels$riskci := .data[["RISK_CI"]],
          !!risklabels$risk := .data[["RISK"]],
          !!risklabels$p := .data[["PVALUE"]],
          !!risklabels$low := .data[["RISKCIL"]],
          !!risklabels$up := .data[["RISKCIU"]],
          !!risklabels$lowup := paste0("(", .data[["RISKCIL"]], ",", .data[["RISKCIU"]], ")")
        )
    }
    final_cts
  }

#' Common sorting for both Risk/No risk cases
#'
#' @param df List of data frames summarized by events.
#' @param ctrlgrp Treatment Control value.
#'
#' @return Data frame
#' @noRd
ord_by_ht <- function(df, ctrlgrp) {
  uniqHT <-
    union(
      unique(pull(
        filter(df[["hterm_summ"]], .data[["TRTVAR"]] == ctrlgrp), "DPTVAL"
      )),
      unique(pull(
        filter(df[["hterm_summ"]], .data[["TRTVAR"]] != ctrlgrp), "DPTVAL"
      ))
    )

  map(names(df), \(x) {
    match_var <- recode(x, "hterm_summ" = "DPTVAL", "lterm_summ" = "BYVAR1")
    df_out <- df[[x]] |>
      select(all_of(c("DPTVAR", "BYVAR1", "DPTVAL"))) |>
      distinct() |>
      mutate(DPTVARN = match(.data[[match_var]], uniqHT)) |>
      filter(!is.na(.data[["DPTVARN"]]))

    if (x == "hterm_summ") {
      df_out <- df_out |>
        mutate(DPTVALN = 0)
    } else {
      df_out <- df_out |>
        group_by(.data[["DPTVARN"]]) |>
        mutate(DPTVALN = row_number()) |>
        ungroup()
    }
    df_out
  }) |>
    bind_rows()
}

#' Get Control Group
#'
#' @param df Data frame
#'
#' @return Name of control group
#' @noRd
get_ctrlgrp <- function(df, col = 1) {
  levels(df[["TRTVAR"]])[as.numeric(col)]
}

#' Insert Overall/Summary Row
#'
#' @param datain Input dataset `ADAM` or intermediate within summary function
#' @param sum_row_label Label for Summary Row to be displayed, if Y.
#' @param byvaryn Include by variable or not? For single overally row, "N"
#' @param var Flag Variable to identify Any/Summary Rows
#' @inheritParams mcatstat
#'
#' @return dataframe with single overall row count
#'
#' @export
#'
#' @examples
#' data("adae")
#' summary_row_cat(
#'   adae,
#'   a_subset = "TRTEMFL == 'Y'"
#' )
#'
summary_row_cat <- function(datain,
                            sum_row_label = "Any Term",
                            byvaryn = "N",
                            a_subset = NA,
                            pctdisp = "TRT",
                            sigdec = 2,
                            pctsyn = "Y",
                            var = "ANY",
                            uniqid = "USUBJID") {
  if (nrow(datain) < 1) {
    return(datain)
  }
  if (!(var %in% names(datain))) {
    datain <- datain |> mutate(!!var := 1)
  }
  if (byvaryn == "N") {
    byvar <- var_start(datain, "BYVAR")
    datain <- datain |>
      select(-starts_with(byvar[length(byvar)]))
  }
  datain |>
    mcatstat(
      a_subset = paste(na.omit(c(a_subset, glue("{var} == 1"))), collapse = "&"),
      dptvar = var,
      pctdisp = pctdisp,
      uniqid = uniqid,
      sigdec = sigdec,
      pctsyn = pctsyn
    ) |>
    mutate(DPTVAL = sum_row_label)
}
