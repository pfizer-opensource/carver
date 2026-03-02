#' Function to create Proportional Hazard Plot
#'
#' @inheritParams scatter_plot
#' @param pair_id Pairs for which pairwise statistics are generated in the form
#' 1-2
#' Default: NA - which displays all the combination.
#' Valid Values: specify the pairs between '-' and each pair should be delimited
#' by `~`.
#' Example: "1-2~2-3"
#' @return proportional hazard plot for the each pair of treatments.
#' @import survival
#' @export
#'
#' @examples
#' data("survival")
#'
#' survival$adsl <- survival$adsl |>
#'   dplyr::mutate(
#'     TRT01PN = dplyr::case_when(
#'       TRT01P == "Xanomeline Low Dose" ~ 1,
#'       TRT01P == "Placebo" ~ 2,
#'       TRT01P == "Screen Failure" ~ 3,
#'       TRUE ~ NA_real_
#'     )
#'   )
#'
#' ph_pre <- surv_pre_processor(
#'   dataset_adsl = survival$adsl,
#'   adsl_subset = "SAFFL=='Y'",
#'   dataset_analysis = survival$adtte,
#'   split_by = NA_character_,
#'   analysis_subset = "PARAMCD=='PFS'",
#'   trtsort = "TRT01PN",
#'   censor_var = "CNSR",
#'   censor_val = 1,
#'   trtvar = "TRT01P",
#'   time_var = "AVAL"
#' )
#'
#' series_opts <- plot_aes_opts(ph_pre,
#'   series_color = "red~blue~green",
#'   series_shape = c(1, 1, 1),
#'   series_size = c(1.5, 1.5, 1.5)
#' )
#'
#' ph_plot(
#'   datain = ph_pre,
#'   axis_opts = plot_axis_opts(),
#'   series_opts = series_opts,
#'   pair_id = "1-2"
#' )
#'
ph_plot <- function(datain,
                    series_opts,
                    pair_id = NA,
                    axis_opts = plot_axis_opts(),
                    legend_opts = list(
                      pos = "bottom",
                      label = "Treatment Group:"
                    ),
                    griddisplay = "N") {
  if (nrow(datain) != 0) {
    stopifnot(
      "Missing required Variable(s)" =
        all(c("TRTSORT", "TRTVAR", "TRTTXT", "timevar", "cnsrvar") %in% names(datain)) # nolint
    )
    stopifnot(
      "Invalid value passed to `pair_id`. Specify the pairs with '-' and delimited with '~'." = # nolint
        is.na(pair_id) || grepl("-", pair_id)
    )
    if (all(is.na(pair_id))) {
      pairs_comb <- combn(sort(unique(datain$TRTSORT)), 2)
      mapthru <- seq_len(ncol(pairs_comb))
    } else {
      pair <- str_to_vec(pair_id)
      mapthru <- pair
    }
    map(mapthru, function(i) {
      if (all(is.na(pair_id))) {
        pair_data <- datain |> filter(TRTSORT %in% c(pairs_comb[, i]))
      } else {
        pair_data <- datain |> filter(TRTSORT %in% as.numeric(unlist(str_split(i, "-")))) # nolint
      }
      survfit_datain <- survfit(Surv(timevar, cnsrvar) ~ TRTVAR,
        data = pair_data
      )
      sumt <- survfit_summary(survfit_datain, data = pair_data)
      g <- scatter_plot(
        sumt |>
          distinct(.data[["TRTVAR"]], .data[["surv"]], .keep_all = TRUE) |>
          bind_rows(sumt |>
            filter(.data[["n.risk"]] == 1)) |>
          mutate(
            XVAR = log(.data[["time"]]),
            YVAR = log(-log(.data[["surv"]]))
          ),
        axis_opts = axis_opts,
        series_var = "TRTVAR",
        series_labelvar = "TRTTXT",
        series_opts = series_opts,
        legend_opts = legend_opts,
        griddisplay = griddisplay
      ) +
        geom_line(linewidth = 0.4)
      g
    })
  } else {
    NULL
  }
}

#' Customized function to get the summary dataframe from survfit()
#'
#' @param x Input list object created from survfit().
#' @param data Pair wise dataframe.
#'
#' @return The summary dataframe with cleaned TRTVAR variable.
#' @export
#' @examples
#' data("survival")
#' survival$adsl <- survival$adsl |>
#'   dplyr::mutate(
#'     TRT01PN = dplyr::case_when(
#'       TRT01P == "Xanomeline Low Dose" ~ 1,
#'       TRT01P == "Placebo" ~ 2,
#'       TRT01P == "Screen Failure" ~ 3,
#'       TRUE ~ NA_real_
#'     )
#'   )
#' ph_pre <- surv_pre_processor(
#'   dataset_adsl = survival$adsl,
#'   adsl_subset = "SAFFL=='Y'",
#'   dataset_analysis = survival$adtte,
#'   split_by = NA_character_,
#'   analysis_subset = "PARAMCD=='PFS'",
#'   trtsort = "TRT01PN",
#'   censor_var = "CNSR",
#'   censor_val = 1,
#'   trtvar = "TRT01P",
#'   time_var = "AVAL"
#' )
#' survfit_df <- survival::survfit(survival::Surv(timevar, cnsrvar) ~ TRTVAR,
#'   data = ph_pre
#' )
#' survfit_summary(
#'   x = survfit_df,
#'   data = pair_data
#' )
survfit_summary <- function(x,
                            data) {
  res <- cbind(
    data.frame(unclass(x)[c("time", "n.risk", "n.event", "n.censor")]),
    surv = x[["surv"]], std.err = x[["std.err"]], upper = x[["upper"]], lower = x[["lower"]] # nolint
  )
  res[["strata"]] <- rep(names(x[["strata"]]), x[["strata"]])
  res[["TRTVAR"]] <- as.factor(as.vector(
    sapply(as.vector(res$strata), function(x) {
      x <- unlist(strsplit(x, "=", perl = TRUE))
      index <- grep(paste0("^", "TRTVAR", "$"), x)[1]
      trimws(x[index + 1])
    })
  ))
  res
}
