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

#' Calculate Risk Statistics for treatment pairs from pre-processed Adverse Events data
#'
#' @param datain Input dataset after pre_processing and running `mentry()` to *ADAE* data
#' @param a_subset Analysis Subset condition specific to categorical analysis.
#' @param summary_by Measure to construct the summary by. Values: `'Patients' or 'Events'`.
#' @param eventvar Event Variable to review by. Example: `'AEDECOD', 'AEBODSYS'`.
#' @param ctrlgrp Treatment Control value.
#' @param trtgrp Treatment(s) to create pairs. Only 1 value for Volcano/table, can be multiple
#' for `forest_plot()`.
#' @param statistics Statistic to be calculated. Values: `'Risk Ratio' or 'Risk Difference'`.
#' @param alpha Alpha value to determine confidence interval for risk calculation. Default: `0.05`
#' @param cutoff Incidence Cutoff Value; consider only terms with `incidence percentage > cutoff`.
#' @param sort_opt How to sort terms, only for table/forest plot.
#' Values: `'Ascending','Descending','Alphabetical'`.
#' @param sort_var Metric to sort by. Values: `'Count','Percent','RiskValue'`.
#' @param g_sort_by_ht For Forest Plot only - include sorting by high term/*BYVAR1*?
#' Values: "Y"/"N". In the output, terms will be sorted by group first, then term. To be used
#' along with `ht_dispyn` = Y in `ae_forest_plot()`
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
#'   cutoff = 2,
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
           cutoff = 2,
           sort_opt,
           sort_var,
           g_sort_by_ht = "N") {
    trtgrp <- str_to_vec(trtgrp, "~~")
    stopifnot("Invalid Control Group" = ctrlgrp %in% unique(datain[["TRTVAR"]]))
    stopifnot("Invalid Treatment Group" = all(trtgrp %in% unique(datain[["TRTVAR"]])))
    stopifnot(
      "Invalid Risk Statistics; specify any one of `Risk Ratio` or `Risk Difference`" =
        statistics %in% c("Risk Ratio", "Risk Difference")
    )
    trt_list <- datain[["TRTVAR"]]
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
    distinct_vars <- c("TRTVAR", id_vars, value_vars)

    ## calculating risk statistics mapping over each treatment
    risk_out <-
      map(set_names(trtgrp), \(trt) {
        datain <- datain |>
          filter(.data[["TRTVAR"]] %in% c(ctrlgrp, trt))

        mcat_out <- mcatstat(
          datain = datain,
          a_subset = a_subset,
          uniqid = ifelse(tolower(summary_by) == "events", "ALLCT", summ_var),
          dptvar = eventvar,
          pctdisp = "TRT"
        )

        stopifnot(nrow(mcat_out) > 0, !is.null(mcat_out))

        rout <- add_risk_stat(
          mcatout = mcat_out,
          ctrlgrp = ctrlgrp,
          trtgrp = trt,
          id_vars = id_vars,
          value_vars = value_vars,
          statistics = statistics,
          alpha = alpha,
          cutoff = cutoff
        )

        if (nrow(rout) > 0) {
          rout <- rout |>
            left_join(distinct(select(mcat_out, any_of(
              c(distinct_vars)
            ))), by = c("TRTVAR", intersect(id_vars, names(mcat_out)))) |>
            mutate(across(any_of(c(value_vars)), \(x) as.double(x))) |>
            select(c(
              any_of(c(id_vars)),
              contains("PVAL"),
              contains("RISK"),
              contains("CTRL_"),
              starts_with("TRT"),
              any_of(c(value_vars)),
              "TOTAL_N" = "DENOMN"
            ))
        }
        rout
      }) |>
      bind_rows()

    if (nrow(risk_out) > 0) {
      ## Add hover_text and order the final table
      risk_out <- risk_out |>
        risk_hover_text(summary_by, eventvar) |>
        ord_summ_df(sort_var, sort_opt, g_sort_by_ht)
      risk_out[["TRTVAR"]] <- factor(risk_out[["TRTVAR"]],
        levels = unique(risk_out[["TRTVAR"]][order(trt_list)])
      )
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
                          alpha = 0.05,
                          cutoff = 2) {
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
      id_cols = any_of(c(id_vars)),
      names_from = "TRTCD",
      values_from = any_of(c(value_vars))
    ) |>
    mutate(across(where(is.numeric), ~ replace_na(.x, 0))) |>
    filter(.data[["PCT_CTRLGRP"]] > cutoff |
      .data[["PCT_TRTGRP"]] > cutoff)

  if (nrow(risk_prep) < 1) {
    message("`cutoff` value provided is too big, please specify a smaller `cutoff` value")
    return(data.frame(NULL))
  }
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
  ## Extract Risk Statistics from the added `risk_out` column by each row
  rowwise(risk_prep) |>
    mutate(
      PVALUE = flatten(.data[["risk_out"]])[["pval"]],
      RISK = flatten(.data[["risk_out"]])[["risk"]],
      RISKCIL = flatten(.data[["risk_out"]])[["low_ci"]],
      RISKCIU = flatten(.data[["risk_out"]])[["upp_ci"]]
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
    select(-c("risk_out", any_of(starts_with("DENOMN")), any_of(ends_with("TRTGRP")))) |>
    pivot_longer(c("CTRL", "ACTIVE"), values_to = "TRTVAR", names_to = NULL)
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
        suppressWarnings(epitools::riskratio.wald(risk_mat, conf.level = 1 - alpha))
    }

    list(
      risk = round(risk_mat$measure[2, 1], 3),
      pval = round(risk_mat$p.value[2, 3], 4),
      low_ci = round(risk_mat$measure[2, 2], 2),
      upp_ci = round(risk_mat$measure[2, 3], 2)
    )
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
#' @param y single factor or character vector that will be combined with x into a table
#'        (default is NULL)
#' @param conf.level confidence level (default is 0.95)
#' @param rev reverse order of "rows", "colums", "both", or "neither" (default)
#' @param correction Yate's continuity correction
#' @param verbose To return more detailed results
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
  function(x, y = NULL,
           conf.level = 0.95,
           rev = "neither",
           correction = FALSE,
           verbose = FALSE) {
    if (is.matrix(x) && !is.null(y)) {
      stop("y argument should be NULL")
    }
    if (is.null(y)) {
      x <- epitools::epitable(x, rev = rev)
    } else {
      x <- epitools::epitable(x, y, rev = rev)
    }
    tmx <- epitools::table.margins(x)
    p.exposed <- sweep(tmx, 2, tmx["Total", ], "/")
    p.outcome <- sweep(tmx, 1, tmx[, "Total"], "/")
    Z <- qnorm(0.5 * (1 + conf.level))
    nr <- nrow(x)
    wald <- matrix(NA, nr, 3)
    wald[1, 1] <- 1
    for (i in 2:nr) {
      a <- x[i, 2]
      b <- x[i, 1]
      c <- x[1, 2]
      d <- x[1, 1]

      # point estimate of risk difference
      est <- (a / (a + b)) - (c / (c + d))
      # standard error of risk difference
      se_RD <- sqrt((a * b / (a + b)^3) + (c * d / (c + d)^3))

      ci <- est + c(-1, 1) * Z * se_RD
      wald[i, ] <- c(est, ci)
    }
    pv <- epitools::tab2by2.test(x, correction = correction)
    colnames(wald) <- c("estimate", "lower", "upper")
    rownames(wald) <- rownames(x)
    cn2 <- paste(
      "risk difference with",
      paste(100 * conf.level, "%", sep = ""),
      "C.I."
    )
    names(dimnames(wald)) <- c(names(dimnames(x))[1], cn2)

    rr <- list(
      x = x,
      data = tmx,
      p.exposed = p.exposed,
      p.outcome = p.outcome,
      measure = wald,
      conf.level = conf.level,
      p.value = pv$p.value,
      correction = pv$correction
    )
    rrs <- list(
      data = tmx,
      measure = wald,
      p.value = pv$p.value,
      correction = pv$correction
    )

    attr(rr, "method") <- "Unconditional MLE & normal approximation (Wald) CI"
    attr(rrs, "method") <- "Unconditional MLE & normal approximation (Wald) CI"

    if (verbose == FALSE) {
      rrs
    } else {
      rr
    }
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
