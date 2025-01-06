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

#' Std Error function definition
#'
#' @param x Input vector
#'
#' @return Numeric value containing standard error
#'
#' @noRd
stderr <- function(x, ...) {
  sd(x, ...) / sqrt(length((x)))
}

#' Dataframe of statistics, labels and derivations
#'
#' @return tibble of 4 columns
#' @noRd
stat_lookup <- function() {
  tibble::tribble(
    ~Stat, ~base, ~label, ~derv,
    "mean(sd)", "mean/sd", "Mean(SD)", "{mean} ({sd})",
    "mean(stderr)", "mean/stderr", "Mean(SE)", "{mean} ({stderr})",
    "minmax", "min/max", "(Min-Max)", "({min}-{max})",
    "minmaxc", "min/max", "(Min,Max)", "({min},{max})",
    "median(minmax)", "median/min/max", "Median (Min-Max)", "{median} ({min}-{max})",
    "median(minmaxc)", "median/min/max", "Median (Min,Max)", "{median} ({min},{max})",
    "q1q3", "q1/q3", "(Q1,Q3)", "({q1}, {q3})",
    "median(q1q3)", "median/q1/q3", "Median (Q1,Q3)", "{median} ({q1},{q3})",
    "geomean(geosd)", "geomean/geosd", "Geomean(Geomean SD)", "{geomean} ({geosd})",
    "q1", "q1", "1st Quantile", "",
    "q3", "q3", "3rd Quantile", "",
    "stderr", "stderr", "s.e.", "",
    "sd", "sd", "s.d.", ""
  )
}

#' Parse Statistics for msumstat
#'
#' @param statvar Input statistics to msumstat
#' @param statdec Input statistics decimal levels to msumstat
#'
#' @return named vector
#' @noRd
parse_stats <- function(statvar, statdec) {
  if (all(is.na(statdec)) || all(statdec == "")) statdec <- rep(2, length(statvar))
  if (length(statdec) == 1) statdec <- rep(statdec, length(statvar))
  stopifnot(length(statvar) == length(statdec))
  lookup <- stat_lookup() |> filter(derv != "")
  stats <- map(seq_along(statvar), \(s) {
    if (statvar[s] %in% lookup$Stat) {
      st <- lookup |>
        filter(Stat == statvar[s]) |>
        pull(base) |>
        str_to_vec("/")
      d <- unlist(str_extract_all(statdec[s], "[0-9]+"))
      last <- d[length(d)]
      length(d) <- length(st)
      d[is.na(d)] <- last
      setNames(d, st)
    } else {
      setNames(statdec[s], statvar[s])
    }
  }) |>
    unlist()
  if (any(duplicated(names(stats)))) {
    stats[unique(names(stats))]
  } else {
    stats
  }
}

#' List of Summary Functions
#'
#' @param statvar Input statistics
#' @param statdec Corresponding number of decimal places for each statistic
#'
#' @return A named list containing function definition for all defined summary
#' statistics - mean, min, max, median, mode iqr, var, sum, sd, q25, q75, p1, p5,
#' p10, p90, p95, p99 (where last digits represent % of quantile), whiskerlow,
#' whiskerup, outliers in the Tukey method for box statistics, geometric mean/sd/CI
#' @export
#'
#' @examples
#' summary_functions(c("mean", "mode"), c(2, 1))
summary_functions <- function(statvar, statdec) {
  base_fns <- c(
    "mean", "min", "max", "median", "IQR", "var", "sum", "sd", "stderr", "mode",
    "whiskerlow", "whiskerup"
  )
  map(seq_along(statvar), \(s) {
    d <- as.numeric(statdec[s])
    f <- statvar[s]
    f <- recode(f, "q1" = "q25", "q3" = "q75", "iqr" = "IQR")
    if (f %in% base_fns) {
      fmtrd(f, d)
    } else if (str_detect(f, "^(q\\d+)$|^(p\\d+)$")) {
      fmtrd(f = "quantile", d = d, as.numeric(gsub("\\D", "", f)) / 100, type = 2)
    } else if (f == "geomean") {
      function(x) round_f(exp(mean(log(x), na.rm = TRUE)), d)
    } else if (f == "geosd") {
      function(x) round_f(exp(sd(log(x), na.rm = TRUE)), d)
    } else if (f == "geomean_lowci") {
      function(x) {
        x <- log(x)
        margin_error <- qt(0.975, df = length(x) - 1) * sd(x, na.rm = TRUE) / sqrt(length(x))
        round_f(exp(mean(x, na.rm = TRUE) - margin_error), d)
      }
    } else if (f == "geomean_upci") {
      function(x) {
        x <- log(x)
        margin_error <- qt(0.975, df = length(x) - 1) * sd(x, na.rm = TRUE) / sqrt(length(x))
        round_f(exp(mean(x, na.rm = TRUE) + margin_error), d)
      }
    } else if (f == "outliers") {
      function(x) {
        x <- x[!is.na(x)]
        paste(unique(x[x < whiskerlow(x) | x > whiskerup(x)]), collapse = "")
      }
    } else if (f == "nobs") {
      function(x) paste(n())
    } else if (f == "n") {
      function(x) as.character(sum(!is.na(x)))
    } else if (f == "nmiss") {
      function(x) as.character(sum(is.na(x)))
    } else {
      function(x) "_NO_STAT"
    }
  }) |>
    setNames(statvar)
}


#' Lower Box Whiskers
#'
#' @param x Input data
#' @param na.rm Remove NA
#'
#' @return Lower Whisker Value for box plot data
#' @noRd
whiskerlow <- function(x, na.rm = TRUE) {
  min(x[(x >= (quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE))) &
          (x <= quantile(x, 0.25, na.rm = TRUE))], na.rm = na.rm)
}

#' Upper Box Whiskers
#'
#' @param x Input data
#' @param na.rm Remove NA
#'
#' @return Upper Whisker Value for box plot data
#' @noRd
whiskerup <- function(x, na.rm = TRUE) {
  max(x[(x <= (quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE))) &
          (x >= quantile(x, 0.75, na.rm = TRUE))], na.rm = na.rm)
}

#' Concatenate to create complex statistics
#'
#' @param data Input dataset
#' @param stats Statistics
#'
#' @return Dataframe with mutated columns
#' @noRd
derv_stats <- function(data, stats, lookup = stat_lookup()) {
  lookup <- lookup |> filter(derv != "")
  if (!any(stats %in% lookup[[1]])) {
    return(data)
  } else {
    stats <- stats[stats %in% lookup[[1]]]
    map(stats, \(s) {
      derv <- lookup |>
        filter(if_all(1) == s) |>
        pull(derv)
      data |>
        mutate({{ s }} := glue::glue(derv)) |>
        select(all_of(s))
    }) |>
      (\(.) bind_cols(data, .))()
  }
}
