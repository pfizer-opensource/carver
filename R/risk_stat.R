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
options(warn = -1)

options(warn = -1)

#' Calculate Risk Statistics for treatment pairs from pre-processed Adverse Events data
#'
#' @inheritParams mcatstat
#' @param summary_by Measure to construct the summary by. Values: `'Patients' or 'Events'`.
#' @param eventvar Event Variable to review by. Example: `'AEDECOD', 'AEBODSYS'`.
#' @param ctrlgrp Treatment Control value.
#' @param trtgrp Treatment(s) to create pairs. Only 1 value for Volcano/table, can be multiple
#' for `forest_plot()`.
#' @param statistics Statistic to be calculated. Values: `'Risk Ratio' or 'Risk Difference'`.
#' @param alpha Alpha value to determine confidence interval for risk calculation. Default: `0.05`
#' @param cutoff_where Filter condition for incidence/pct. Consider only terms with
#' eg: "FREQ > 5" or "PCT <3". Must contain FREQ or PCT (count or percent)
#' @param sort_opt How to sort terms, only for table/forest plot.
#' Values: `'Ascending','Descending','Alphabetical'`.
#' @param sort_var Metric to sort by. Values: `'Count','Percent','RiskValue'`.
#' @param g_sort_by_ht For Forest Plot only - include sorting by high term/*BYVAR1*?
#' Values: "Y"/"N". In the output, terms will be sorted by group first, then term. To be used
#' along with `ht_dispyn` = Y in `ae_forest_plot()`
#' @param riskdiff_pct To display risk and CI as % if `statistic` = risk difference (Y/N)
#' @param hoveryn Include hover information (for graphs) Y/N
#'
#' @return A dataset containing risk statistic calculations for given treatment pair(s).
#' @export
#'
#' @examples
#' ## Pre-processed AE data
#' data("ae_pre_process")
#'
#' ae_entry <- mentry(
#'   datain = ae_pre_process$data,
#'   subset = NA,
#'   byvar = "AEBODSYS",
#'   trtvar = "TRTA",
#'   trtsort = "TRTAN",
#'   subgrpvar = NA,
#'   trttotalyn = "N",
#'   add_grpmiss = "N",
#'   sgtotalyn = "N",
#'   pop_fil = "SAFFL"
#' )
#'
#' risk_stat(
#'   datain = ae_entry,
#'   a_subset = ae_pre_process$a_subset,
#'   summary_by = "Patients",
#'   eventvar = "AEDECOD",
#'   ctrlgrp = "Placebo",
#'   trtgrp = "Xanomeline High Dose",
#'   statistics = "Risk Ratio",
#'   alpha = 0.05,
#'   cutoff_where = "PCT > 2",
#'   sort_opt = "Ascending",
#'   sort_var = "Count"
#' )
risk_stat <-
  function(datain,
           a_subset = NA_character_,
           summary_by = "Patients",
           eventvar,
           ctrlgrp,
           trtgrp,
           statistics = "Risk Ratio",
           alpha = 0.05,
           cutoff_where = NA,
           sort_opt,
           sort_var,
           g_sort_by_ht = "N",
           riskdiff_pct = "N",
           sigdec = 1,
           pctsyn = "Y",
           hoveryn = "Y") {
    trtgrp <- str_to_vec(trtgrp, "~~")
    stopifnot(
      "Invalid Risk Statistics; specify any one of `Risk Ratio` or `Risk Difference`" =
        tolower(statistics) %in% c("risk ratio", "risk difference")
    )
    trt_list <- levels(datain[["TRTVAR"]])
    ## getting equivalent data variable for given summary by selection
    summ_var <-
      recode(tolower(summary_by),
             "participants" = "USUBJID",
             "patients" = "USUBJID",
             "events" = eventvar
      )
    ## get sort variables to apply sorting post risk statistics calculation
    if (sort_opt == "Alphabetical") {
      sort_var <- get_sort_var(sort_opt)
    } else {
      sort_var <- get_sort_var(sort_var)
    }
    
    id_vars <- c("BYVAR1", "DPTVAL")
    value_vars <- c("FREQ", "PCT", "DENOMN")
    mcat_out <- mcatstat(
      datain = datain,
      a_subset = a_subset,
      uniqid = ifelse(tolower(summary_by) == "events", "ALLCT", summ_var),
      dptvar = eventvar,
      pctdisp = "TRT",
      sigdec = sigdec,
      pctsyn = pctsyn
    )
    if (nrow(mcat_out) == 0) {
      return(mcat_out)
    }
    ## calculating risk statistics mapping over each treatment
    risk_out <-
      map(set_names(trtgrp), \(trt) {
        mcatin <- mcat_out |>
          filter(.data[["TRTVAR"]] %in% c(ctrlgrp, trt))
        if (!is.na(cutoff_where) && str_detect(cutoff_where, "PCT|FREQ")) {
          mcat_cut <- mcatin |>
            filter(!!!parse_exprs(cutoff_where)) |>
            distinct(across(all_of(id_vars))) |>
            mutate(CUTFL = "Y")
          mcatin <- mcatin |>
            left_join(mcat_cut, by = id_vars) |>
            filter(CUTFL == "Y")
        }
        if (nrow(mcatin) < 1) {
          return(data.frame())
        }
        rout <- add_risk_stat(
          mcatout = mcatin,
          ctrlgrp = ctrlgrp,
          trtgrp = trt,
          id_vars = id_vars,
          value_vars = value_vars,
          statistics = statistics,
          riskdiff_pct = riskdiff_pct,
          alpha = alpha
        )
        if (nrow(rout) > 0) {
          rout <- mcat_out |>
            filter(.data[["TRTVAR"]] %in% c(ctrlgrp, trt) |
                     str_detect(.data[["TRTVAR"]], "Total")) |>
            left_join(rout, by = intersect(names(mcat_out), names(rout))) |>
            mutate(across(any_of(c(value_vars)), \(x) as.double(x)), TOTAL_N = DENOMN) |>
            filter(!.data[["RISK"]] %in% c(NA, Inf, NaN))
        }
        rout
      }) |>
      bind_rows()
    if (nrow(risk_out) > 0 && ncol(risk_out) > 1) {
      ## Add hover_text and order the final table
      risk_out <- risk_out |>
        ord_summ_df(sort_var, sort_opt, g_sort_by_ht)
      risk_out[["TRTVAR"]] <- factor(risk_out[["TRTVAR"]],
                                     levels = trt_list, ordered = TRUE
      )
      if (hoveryn == "Y") {
        risk_out <- risk_hover_text(risk_out, summary_by, eventvar)
      }
    }
    risk_out
  }

#' Add calculated risk statistics to data
#'
#' @inheritParams risk_stat
#' @param mcatout Output from `mcatstat()`
#' @param id_vars ID cvariables required for pivoting.
#' @param value_vars Values required for pivoting.
#'
#' @return Data frame with added Risk Statistics
#' @noRd
add_risk_stat <- function(mcatout,
                          ctrlgrp,
                          trtgrp,
                          id_vars = c("BYVAR1", "DPTVAL"),
                          value_vars = c("FREQ", "PCT", "DENOMN"),
                          statistics = "Risk Ratio",
                          riskdiff_pct = "N",
                          alpha = 0.05) {
  if (nrow(mcatout) < 1 || !all(c(ctrlgrp, trtgrp) %in% unique(mcatout$TRTVAR))) {
    return(data.frame())
  }
  risk_prep <- mcatout |>
    mutate(
      TRTCD =
        case_when(
          .data[["TRTVAR"]] == ctrlgrp ~ "CTRLGRP",
          .data[["TRTVAR"]] == trtgrp ~ "TRTGRP",
          TRUE ~ ""
        ),
      PCT = as.double(.data[["PCT"]])
    ) |>
    pivot_wider(
      id_cols = any_of(c(id_vars, "CUTFL")),
      names_from = "TRTCD",
      values_from = any_of(c(value_vars))
    ) |>
    mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
  ## Calculate Risk Statistics in `risk_out` column using `calc_risk_stat`
  risk_prep <- risk_prep |>
    group_by(across(any_of(c(id_vars)))) |>
    mutate(risk_out = pmap(
      list(
        .data[["FREQ_CTRLGRP"]], .data[["FREQ_TRTGRP"]],
        .data[["DENOMN_CTRLGRP"]], .data[["DENOMN_TRTGRP"]],
        statistics, alpha
      ),
      calc_risk_stat
    )) |>
    ungroup()
  # Show as percent if required
  if (tolower(statistics) == "risk difference" && riskdiff_pct == "Y") {
    ppct <- 100
  } else {
    ppct <- 1
  }
  ## Extract Risk Statistics from the added `risk_out` column by each row
  rowwise(risk_prep) |>
    mutate(
      PVALUE = round(flatten(.data[["risk_out"]])[["pval"]], 4),
      RISK = round(ppct * flatten(.data[["risk_out"]])[["risk"]], 3),
      RISKCIL = round(ppct * flatten(.data[["risk_out"]])[["low_ci"]], 2),
      RISKCIU = round(ppct * flatten(.data[["risk_out"]])[["upp_ci"]], 2)
    ) |>
    mutate(
      ADJPVALUE = p.adjust(.data[["PVALUE"]], method = "fdr"),
      RISK_CI = paste0(
        .data[["RISK"]], " (",
        .data[["RISKCIL"]], ", ", .data[["RISKCIU"]], ")"
      ),
      TRTPAIR = paste0(ctrlgrp, " -vs- ", trtgrp),
      CTRL = ctrlgrp,
      ACTIVE = trtgrp
    ) |>
    rename(CTRL_N = "FREQ_CTRLGRP", CTRL_PCT = "PCT_CTRLGRP") |>
    select(-c("risk_out", any_of(starts_with("DENOMN")), any_of(ends_with("TRTGRP"))))
}

#' Calculate Risk Statistics
#'
#' @param count1,count2 Vector of counts
#' @param denom1,denom2 Vector of denominator values
#' @param statistic Statistic to be displayed (`Risk Difference`/`Risk Ratio`)
#' @param alpha Level of significance
#'
#' @return List of Risk Statistics
#' @noRd
calc_risk_stat <-
  function(count1,
           count2,
           denom1,
           denom2,
           statistic = "Risk Ratio",
           alpha = 0.05) {
    risk_mat <-
      matrix(
        c(
          denom1 - count1,
          denom2 - count2,
          count1, count2
        ),
        nrow = 2
      )
    
    if (statistic == "Risk Difference") {
      risk_mat <-
        suppressWarnings(riskdiff_wald(risk_mat, conf.level = 1 - alpha))
    } else {
      risk_mat <-
        suppressWarnings(
          epitools::riskratio.wald(risk_mat, conf.level = 1 - alpha, correction = TRUE)
        )
    }
    extract_riskstats(risk_mat, statistic)
  }

#' Calculate Risk difference
#'
#' Function to calculate risk difference by unconditional maximum likelihood estimation (Wald)
#' for any given treatment pairs.
#'
#' @param x input data
#'      input data can be one of the following: r x 2 table, vector of numbers from a
#'      contigency table (will be transformed into r x 2 table in row-wise order),
#'      or single factor or character vector that will be combined with y into a table.
#' @param conf.level confidence level (default is 0.95)
#'
#' @return a  list containg a data,measure,p.value,correction
#' @export
#'
#' @examples
#' riskdiff_wald(
#'   x = matrix(c(178, 79, 1411, 1486), 2, 2),
#'   conf.level = 0.95
#' )
riskdiff_wald <-
  function(x, conf.level = 0.95) {
    x <- epitools::epitable(x, rev = "neither")
    tmx <- epitools::table.margins(x)
    Z <- qnorm(0.5 * (1 + conf.level))
    nr <- nrow(x)
    wald <- matrix(NA, nr, 3)
    wald[1, 1] <- 1
    for (i in 2:nr) {
      a <- x[i, 2]
      b <- x[i, 1]
      c <- x[1, 2]
      d <- x[1, 1]
      p2 <- a / (a + b)
      p1 <- c / (c + d)
      est <- p1 - p2
      se_RD <- sqrt((p1 * (1 - p1) / (c + d)) + (p2 * (1 - p2) / (a + b)))
      ci <- (est + c(-1, 1) * Z * se_RD)
      wald[i, ] <- c(est, ci)
    }
    pv <- epitools::tab2by2.test(x, correction = FALSE)
    colnames(wald) <- c("estimate", "lower", "upper")
    rownames(wald) <- rownames(x)
    cn2 <- paste(
      "risk difference with",
      paste(100 * conf.level, "%", sep = ""),
      "C.I."
    )
    names(dimnames(wald)) <- c(names(dimnames(x))[1], cn2)
    
    rrs <- list(
      data = tmx,
      measure = wald,
      p.value = pv$p.value,
      correction = pv$correction
    )
    attr(rrs, "method") <- "Unconditional MLE & normal approximation (Wald) CI"
    rrs
  }

#' Add Risk Statistics specific Hover Text to data
#'
#' @param df Data frame containing the risk statistics
#' @inheritParams risk_stat
#'
#' @return Data frame
#' @noRd
risk_hover_text <- function(df, summary_by, eventvar) {
  df |>
    mutate(
      HOVER_PCT = paste0(
        "\n",
        .data[["BYVAR1"]],
        "\n",
        eventvar,
        " = ",
        .data[["DPTVAL"]],
        "\n n of ",
        ifelse(summary_by == "Patients", "Participants", summary_by),
        "= ",
        .data[["FREQ"]],
        "\n Percentage = ",
        .data[["PCT"]]
      ),
      HOVER_RISK = paste0(
        "\n",
        .data[["BYVAR1"]],
        "\n",
        eventvar,
        " = ",
        .data[["DPTVAL"]],
        "\n Risk(CI) = ",
        .data[["RISK_CI"]],
        "\n p-value = ",
        .data[["PVALUE"]],
        "\n",
        .data[["TRTPAIR"]]
      ),
      HOVER_TEXT = paste0(
        .data[["HOVER_PCT"]],
        "\n Risk(CI) = ",
        .data[["RISK_CI"]],
        "\n p-value = ",
        .data[["PVALUE"]]
      )
    )
}

#' Extract Risk Statistics
#'
#' @return list of statistics
#' @noRd
extract_riskstats <- function(risk_mat, statistic) {
  risk <- risk_mat$measure[2, 1]
  pval <- risk_mat$p.value[2, 3]
  low_ci <- risk_mat$measure[2, 2]
  upp_ci <- risk_mat$measure[2, 3]
  
  if (statistic == "Risk Difference") {
    out <- list(
      risk = 0 - risk,
      pval = pval,
      upp_ci = 0 - low_ci,
      low_ci = 0 - upp_ci
    )
  } else {
    out <- list(
      risk = risk,
      pval = pval,
      upp_ci = upp_ci,
      low_ci = low_ci
    )
  }
  out
}
