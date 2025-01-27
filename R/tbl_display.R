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
#' Prepare data for general table display
#'
#' @param datain Input dataframe
#' @param dptlabel Tilde-separated labels to set to category variables in data.
#' eg: If analysis vars (`DPTVAR`) contains `AGEGR1, RACE`; `dptlabel` may be `"Age Group~Race"`.
#' @param statlabel Tilde-separated labels corresponding to each Statistic in data.
#' @param extra_df Additional dataframe, to merge with `datain`.
#' @param extra_mergeby Variables to merge `extra_df` by, if present.
#' @param dropvars Variables additional to standard present in input data to be removed from output
#' @param keepvars Specific variables to be retained in report in addition to grouping and value
#' columns. If `keepvars` is given it overrides `dropvars` and the columns are located as last
#' columns of the output.
#' @param disp_value_col Hide/Show value column from the final display.
#' @param addrowvars Group Variable(s) to be removed as a column and instead used as row headers
#' in the value column.
#'
#' @return dataframe prepared for display using flextable
#' @export
#'
#' @examples
#' mentry_df <- adsl |>
#'   mentry(
#'     subset = NA_character_,
#'     byvar = "ETHNIC/ETHNICN~BMIBLGR1",
#'     trtvar = "TRT01A",
#'     trtsort = "TRT01AN",
#'     subgrpvar = "SEX",
#'     trttotalyn = "N",
#'     add_grpmiss = "N",
#'     sgtotalyn = "N",
#'     pop_fil = "SAFFL"
#'   )
#' adsl_sum <- adsl_summary(
#'   datain = mentry_df,
#'   vars = "AGEGR1/AGEGR1N~AGE-S~RACE/RACEN"
#' ) |>
#'   display_bign_head(mentry_df)
#' tbl_processor(
#'   datain = adsl_sum,
#'   dptlabel = "Age Group~Age~Race",
#'   statlabel = NA,
#'   addrowvars = "DPTVAR"
#' )
tbl_processor <- function(datain,
                          dptlabel = NA,
                          statlabel = NA,
                          extra_df = NULL,
                          extra_mergeby = "DPTVAL",
                          dropvars = "",
                          keepvars = "",
                          disp_value_col = "Y",
                          addrowvars = NA_character_) {
  stopifnot(nrow(datain) != 0)
  # Identify if by groups exist
  BYVAR <- var_start(datain, "BYVAR")
  SUBGRP <- var_start(datain, "SUBGRP")
  BYVARN <- var_start(datain, "BYVARN")
  SUBGRPN <- var_start(datain, "SUBGRPN")
  if (disp_value_col == "N") {
    datain <- select(datain, -"DPTVAL")
  }
  # Drop variables not in use and convert display to character
  rep <- datain |>
    mutate(across(any_of("CVALUE"), ~ as.character(.x))) |>
    ungroup()
  if (any(keepvars == "")) {
    rep <- rep |>
      select(
        -any_of(c(
          "DENOMN", "TRTTXT", "FREQ", "PCT", "CPCT", "XVAR",
          "TOTAL_N", "TRTPAIR", dropvars
        )),
        -starts_with("HOVER")
      )
  } else {
    rep <- rep |>
      select(
        any_of(c(BYVAR, "DPTVAR", "DPTVAL",
          "DPTVAL" = "STAT", "TRTVAR", SUBGRP, BYVARN, SUBGRPN,
          "DPTVARN", "DPTVALN", "DPTVALN" = "STATN", "CVALUE", "CN", keepvars
        ))
      )
  }

  # IF treatment, subgroup exists, pivot and perform operations
  if (any(c("TRTVAR", SUBGRP) %in% names(rep))) {
    # Workaround for pivot_wider to accept "duplicate" spanned column names (Total)
    rep <- rep |>
      mutate(across(any_of(SUBGRP), ~
        ifelse(
          get(paste0(cur_column(), "N")) == 9999,
          paste0(.x, paste(rep(" ", which(SUBGRP == cur_column())), collapse = "")),
          .x
        ))) |>
      arrange(across(any_of(c("TRTVAR", SUBGRPN)))) |>
      select(-any_of(SUBGRPN)) |>
      pivot_wider(
        names_from = c(any_of("TRTVAR"), all_of(SUBGRP)),
        values_from = "CVALUE",
        values_fill = "-",
        names_sort = FALSE
      ) |>
      ungroup() |>
      mutate(across(
        -any_of(c(BYVAR, "DPTVAR", "DPTVAL")) & where(is.character),
        ~ ifelse(.data[["CN"]] == "C", gsub("^-$", "0", .x), .x)
      ))
  }
  # Only N and uniqN should be 0 instead of -
  if ("DPTVAL" %in% names(rep) && any(c("n", "nmiss", "nobs") %in% unique(rep$DPTVAL))) {
    rep <- rep |> mutate(across(
      -any_of(c(BYVAR, "DPTVAR", "DPTVAL")) & where(is.character),
      ~ if_else(.data[["DPTVAL"]] %in% c("n", "nmiss", "nobs"),
        gsub("^-$", "0", .x), .x
      )
    ))
  }
  # If additional dataset is given:
  if (is.data.frame(extra_df)) {
    rep <- rep |> inner_join(extra_df, by = extra_mergeby)
  }
  # Order Variables to last:
  if (all(keepvars != "") && all(keepvars %in% names(rep))) {
    rep <- relocate(rep, any_of(keepvars), .after = last_col())
  }
  # Labels for Statistics, if present
  if (!all(is.na(statlabel)) && any(rep[["CN"]] == "N")) {
    statn <- setNames(
      str_to_vec(statlabel),
      unique(rep[["DPTVAL"]][rep[["CN"]] == "N"])
    )
    rep <- rep |>
      mutate(
        DPTVAL = ifelse(.data[["CN"]] == "N",
          recode(.data[["DPTVAL"]], !!!statn),
          .data[["DPTVAL"]]
        )
      )
  }
  # dptlabel - Category labels for each 'DPTVAR'.
  rep <- rep |>
    set_cat_labels(dptlabel = dptlabel)
  # Add group header row for requested variables
  if (!is.na(addrowvars) && any(addrowvars %in% names(rep))) {
    rep <- rep |>
      add_row_var(
        byvar = BYVAR,
        byvarn = BYVARN,
        addrowvar = addrowvars
      ) |>
      select(-any_of(addrowvars))
  }
  if (length(unique(rep[["DPTVAR"]])) < 2) {
    rep <- select(rep, -any_of("DPTVAR"))
  }
  # arrange rows and then proceed; combine groups if dptlabel is suitably passed
  rep |>
    arrange(across(any_of(c(BYVARN, "DPTVARN", "DPTVALN")))) |>
    filter(if_all(any_of("DPTVAL"), ~ !grepl("_NONE_$|_JOIN_$", toupper(.x)))) |>
    select(any_of(c(BYVAR, "DPTVAR", "DPTVAL")), everything(), -any_of(BYVARN))
}

# To be changed/split in final version
#' Prepare By variable and dpt variable for display
#'
#' @param data Input dataframe
#' @param dptlabel Label to apply to values in `DPTVAR` column
#'
#' @return list of dataframe, merging and target columns
#'
#' @noRd
set_cat_labels <- function(data,
                           dptlabel) {
  vals <- data |>
    arrange(.data[["DPTVARN"]]) |>
    select(all_of(c("DPTVAR", "DPTVARN"))) |>
    unique()
  # # Labels for categories
  if (all(is.na(dptlabel))) {
    dptlabel <- setNames(str_to_title(vals[["DPTVAR"]]), vals[["DPTVAR"]])
    label_df <- data |>
      mutate(DPTVAR = recode(.data[["DPTVAR"]], !!!dptlabel))
  } else {
    cats <- str_to_vec(dptlabel)
    stopifnot(
      "Supply same number of labels as variables" = length(cats) == length(vals[["DPTVARN"]])
    )
    label_df <- bind_cols(vals, new_var = cats) |>
      select(-DPTVAR) |>
      right_join(data, by = "DPTVARN") |>
      mutate(DPTVAR = .data[["new_var"]]) |>
      select(-all_of("new_var"))
  }
  label_df
}

#' Add empty row for dptvar
#'
#' @param datain Input data
#' @param byvar By variables in data
#' @param byvarn By variables N in data
#' @param addrowvar Variable to be added as row
#'
#' @return dataframe
#' @noRd
add_row_var <- function(datain,
                        byvar,
                        byvarn,
                        addrowvar = "DPTVAR") {
  len <- length(addrowvar)
  dptvaln <- seq(0, 0.9, length.out = len)
  pad <- strrep("\t\t", seq_len(len + 1) - 1)
  if ("DPTVAL" %in% names(datain)) {
    val_var <- "DPTVAL"
  } else {
    val_var <- "DPTVAR"
  }
  val_varn <- paste0(val_var, "N")
  purrr::map(seq_along(addrowvar), \(i) {
    varn <- paste0(addrowvar[[i]], "N")
    var <- addrowvar[[i]]
    datain |>
      distinct(across(any_of(unique(c(byvar, byvarn, var, varn))))) |>
      mutate(
        !!val_varn := dptvaln[i],
        !!val_var := paste0(pad[i], .data[[var]])
      )
  }) |>
    bind_rows(
      datain |>
        mutate(!!val_var := paste0(pad[len + 1], .data[[val_var]]))
    )
}


# Execute only if byvar length > 0
#' Column keys required to merge by variable columns (when multiple)
#'
#' @param data Iput data
#' @param byvar By variables in data
#'
#' @return dataframe with grouping variables
#' @noRd
merge_v_byvars <- function(data,
                           byvar) {
  map(seq_along(byvar), \(iby) {
    data |>
      select(all_of(1:iby)) |>
      unite(BLOCK, sep = "-") |>
      rename(!!paste0("BLOCK_", iby) := "BLOCK")
  }) |>
    (\(.) bind_cols(data, .))() |>
    mutate(across(all_of(byvar), \(x) {
      x <- clear_dup_rows(x, get(paste0(str_replace_all(cur_column(), "BYVAR", "BLOCK_"))))
    })) |>
    select(-starts_with("BLOCK_"))
}

#' Clear values from Duplicate Rows within a column
#'
#' @param col Column Name to perform operation on
#' @param target Column Name to check duplicates for
#'
#' @return dataframe output
#'
#' @noRd
clear_dup_rows <- function(col, target) {
  chk <- target == dplyr::lag(target)
  ifelse(!is.na(chk) & chk, "", col)
}
#' Create flextable output from display templates
#'
#' @param datain Input dataframe
#' @param bylabel Change `BYVAR` names to label, if any.
#' @param dpthead String to become name of the column containing categories (`DPTVAL`) in output.
#' @param font Font face for text inside table
#' @param fontsize Font size for text inside table
#' @param boldheadyn Y/N to determine if table header should be bold
#'
#' @return flextable object
#' @export
#'
#' @examples
#' mentry_df <- adsl |>
#'   mentry(
#'     subset = NA_character_,
#'     byvar = NA_character_,
#'     trtvar = "TRT01A",
#'     trtsort = "TRT01AN",
#'     subgrpvar = "SEX/SEXN",
#'     trttotalyn = "N",
#'     add_grpmiss = "N",
#'     sgtotalyn = "N",
#'     pop_fil = "SAFFL"
#'   )
#' repdata <- adsl_summary(
#'   datain = mentry_df,
#'   vars = "AGEGR1~AGE-S~RACE"
#' ) |>
#'   display_bign_head(mentry_df) |>
#'   tbl_processor(
#'     dptlabel = "Age Group~Age~Race",
#'     statlabel = NA,
#'     addrowvars = "DPTVAR"
#'   )
#' tbl_display(
#'   datain = repdata,
#'   bylabel = NA,
#'   dpthead = "  "
#' )
tbl_display <- function(datain,
                        bylabel = NA,
                        dpthead = "  ",
                        font = "Arial",
                        fontsize = 10,
                        boldheadyn = "N") {
  BYVAR <- var_start(datain, "BYVAR")
  # If by variables exist, process for aptly merging the columns in output
  lenby <- length(BYVAR)
  if (lenby > 0) {
    datain <- datain |> merge_v_byvars(BYVAR)
    bydata <- datain
    if (all(!is.na(bylabel))) {
      # Apply label to BYVAR columns, if given
      datain <- datain |> rename(any_of(setNames(BYVAR, str_to_vec(bylabel))))
    }
  }
  rep <- datain |>
    rename(any_of(setNames(
      c("DPTVAL", "DPTVAR"),
      c(ifelse(dpthead == "", "  ", dpthead), " ")
    )))
  tout <- rep |>
    flextable(col_keys = grep("DPTVARN|^CN$|DPTVALN",
      names(rep),
      invert = TRUE, value = TRUE
    )) |>
    ftExtra::span_header("_") |>
    font(fontname = font, part = "all") |>
    fontsize(size = fontsize, part = "all") |>
    (\(.) border_remove(x = .))()
  # Parameters for border setting:
  last <- ncol_keys(tout)
  big_border <- officer::fp_border(color = "gray", width = 1.5)
  small_border <- officer::fp_border(color = "gray", width = 1)
  # Merge By vars if exists
  if (lenby > 0) {
    for (b in seq_along(BYVAR)) {
      rowh <- which(bydata[[BYVAR[b]]] != "") - 1
      tout <- tout |>
        hline(i = rowh[rowh != 0], j = b:lenby, border = small_border)
    }
  }
  if (boldheadyn != "N") {
    tout <- tout |>
      bold(part = "header")
  }
  tout |>
    hline(j = (lenby + 1):last, border = small_border) |>
    border_outer(part = "all", border = big_border) |>
    border_inner(part = "header", border = big_border) |>
    vline(part = "all", border = small_border) |>
    fix_border_issues()
}

#' Return table if output is empty
#'
#' @param text Text to display under table creation
#'
#' @return flextable output
#' @export
#'
#' @examples
#' empty_tbl()
empty_tbl <- function(text = "No participant meets the reporting criteria") {
  flextable(data.frame("X" = text)) |>
    set_header_labels(X = "Table not created") |>
    bold(part = "header") |>
    font(fontname = "Arial", part = "all") |>
    theme_box() |>
    align(align = "center", part = "all") |>
    autofit()
}
