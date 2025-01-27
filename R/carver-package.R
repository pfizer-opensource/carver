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
#' @import shiny ggplot2 dplyr scales shinyWidgets flextable htmltools tools stringr
#' @importFrom DT addRow colReorder datatable formatDate saveWidget
#'             formatPercentage formatRound formatStyle selectCells
#'             selectColumns selectPage selectRows showCols styleEqual
#'             styleInterval tableFooter tableHeader
#' @importFrom forcats fct_reorder fct_inorder fct_match fct_relevel fct_recode
#'             fct_relabel
#' @importFrom shinyjs enable disable hide show toggle click onclick
#'             toggleClass toggleState hidden useShinyjs delay js runjs
#' @importFrom rlang abort arg_match as_function as_label as_name as_string
#'             call2 caller_env call_name current_env .data enexpr eval_bare
#'             expr expr_interp expr_label exprs f_lhs f_rhs inform
#'             is_missing new_formula parse_expr parse_exprs set_names sym
#'             syms type_of warn eval_tidy is_expression
#' @importFrom stats na.omit p.adjust qnorm reorder setNames median IQR quantile qt sd
#' @importFrom purrr map map2 pmap map_chr map_dbl keep modify modify_at
#' modify_if reduce set_names every none compact every flatten pluck list_modify
#' @importFrom tidyr drop_na pivot_wider replace_na unite pivot_longer
#' @importFrom glue glue
#' @importFrom plotly ggplotly subplot add_annotations layout
"_PACKAGE"


## usethis namespace: start
#' @importFrom bs4Dash navbarTab
#' @importFrom bs4Dash accordion
#' @importFrom bs4Dash accordionItem
#' @importFrom bs4Dash box
#' @importFrom bs4Dash boxSidebar
#' @importFrom bs4Dash bs4Card
#' @importFrom bs4Dash dashboardBody
#' @importFrom bs4Dash dashboardHeader
#' @importFrom bs4Dash dashboardPage
#' @importFrom bs4Dash dashboardSidebar
#' @importFrom bs4Dash menuItem
#' @importFrom bs4Dash menuItemOutput
#' @importFrom bs4Dash navbarTab
#' @importFrom bs4Dash renderMenu
#' @importFrom bs4Dash sidebarMenu
#' @importFrom bs4Dash tabBox
#' @importFrom bs4Dash tabItem
#' @importFrom bs4Dash tabItems
#' @importFrom bs4Dash tooltip
#' @importFrom bs4Dash updateNavbarTabs
## usethis namespace: end
NULL
